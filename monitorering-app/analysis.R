# Pure helpers: app readers require neither DuckDB nor access to the live database.
validate_analysis_bundle <- function(x) {
  required <- c('consumption','historical_prices','current_prices','tariff_changes','extracts','tariffs','tariff_products')
  stopifnot(is.list(x),identical(x$version,1L),is.character(x$published_at),length(x$published_at)==1L,
            all(required %in% names(x$tables)),all(vapply(x$tables[required],is.data.frame,logical(1))))
  columns<-list(
    consumption=c('atc5','varenummer','month','ddd','regionalt_tilskud_kr'),
    historical_prices=c('atc5','varenummer','month','ddd','actual_regional_subsidy_kr','compatible_tsp_days'),
    current_prices=c('atc5','varenummer','month','ddd','current_price_subsidy_basis_kr','linkage_status'),
    extracts=c('atc_prefix','month','extracted_at','imported_at','row_count','population_verified'),
    tariffs=c('snapshot_id','observed_at','validity_kind','complete'),
    tariff_products=c('snapshot_id','varenummer','atc'),
    tariff_changes=c('snapshot_id','previous_id','varenummer','change'))
  for(name in names(columns))stopifnot(all(columns[[name]] %in% names(x$tables[[name]])))
  d <- x$tables$consumption
  stopifnot(all(c('atc5','varenummer','month','ddd','regionalt_tilskud_kr') %in% names(d)),
            !anyDuplicated(d[c('atc5','varenummer','month')]))
  for(name in c('historical_prices','current_prices')) {
    z <- x$tables[[name]]
    key<-function(v)paste(v$atc5,v$varenummer,v$month,sep='|')
    stopifnot(nrow(z)==nrow(d),!anyDuplicated(z[c('atc5','varenummer','month')]),setequal(key(z),key(d)))
  }
  invisible(TRUE)
}
analysis_children <- function(prefix) {
  if(nchar(prefix)==7L) return(character())
  paste0(prefix,if(nchar(prefix) %in% c(1,5)) sprintf('%02d',0:99) else LETTERS)
}
analysis_coverage <- function(extracts,prefix,months) {
  do.call(rbind,lapply(as.character(months),function(month) {
    e <- extracts[format(as.Date(extracts$month),'%Y-%m')==month,,drop=FALSE]
    e <- e[startsWith(e$atc_prefix,prefix)|startsWith(prefix,e$atc_prefix),,drop=FALSE]
    covered <- function(p) {
      if(any(startsWith(p,e$atc_prefix))) return(TRUE)
      if(!any(startsWith(e$atc_prefix,p))) return(FALSE)
      kids <- analysis_children(p)
      length(kids)>0L && all(vapply(kids,covered,logical(1)))
    }
    complete <- covered(prefix)
    data.frame(month=month,scope=prefix,status=if(complete) {
      if(nrow(e)>0L && all(e$population_verified)) 'population_verified' else 'query_complete_population_unverified'
    } else if(nrow(e)) 'partial_subscopes' else 'missing',captures=nrow(e))
  }))
}
# An all-missing series remains NA. Partial observed sums are labelled elsewhere.
observed_sum <- function(x) if(!length(x)||all(is.na(x))) NA_real_ else sum(x,na.rm=TRUE)
analysis_months <- function(bundle,prefix,from,to,product='') {
  dates <- seq(as.Date(paste0(from,'-01')),as.Date(paste0(to,'-01')),by='month')
  months <- format(dates,'%Y-%m')
  coverage <- analysis_coverage(bundle$tables$extracts,prefix,months)
  d <- bundle$tables$consumption
  d <- d[startsWith(d$atc5,prefix) & (product=='' | d$varenummer==product),,drop=FALSE]
  do.call(rbind,lapply(seq_along(months),function(i) {
    z <- d[format(as.Date(d$month),'%Y-%m')==months[i],,drop=FALSE]
    cbind(coverage[i,,drop=FALSE],data.frame(ddd=observed_sum(z$ddd),
      actual_subsidy_kr=observed_sum(z$regionalt_tilskud_kr),products=nrow(z),
      missing_ddd=sum(is.na(z$ddd)),missing_subsidy=sum(is.na(z$regionalt_tilskud_kr))))
  }))
}
# Simple observed-subsidy run rate; no seasonal claim, no filling missing months.
# Require >=3 consecutive, scope-complete, nonmissing full months in the target year.
subsidy_forecast <- function(months,year,today=Sys.Date()) {
  fail <- function(why) list(available=FALSE,reason=why)
  if(as.integer(year)!=as.integer(format(today,'%Y'))) return(fail('Fremskrivning tilbydes kun for indeværende år.'))
  d <- months[substr(months$month,1,4)==as.character(year) & months$month<format(today,'%Y-%m'),,drop=FALSE]
  if(nrow(d)<3) return(fail('Utilstrækkeligt datagrundlag: mindst tre afsluttede måneder kræves.'))
  expected <- sprintf('%d-%02d',year,seq_len(max(as.integer(substr(d$month,6,7)))))
  if(!identical(d$month,expected) || any(!d$status %in% c('query_complete_population_unverified','population_verified')) ||
     any(d$missing_subsidy>0) || anyNA(d$actual_subsidy_kr))
    return(fail('Utilstrækkeligt datagrundlag: kræver januar til seneste valgte afsluttede måned uden dækningshuller eller manglende tilskudsmålinger.'))
  list(available=TRUE,observed=sum(d$actual_subsidy_kr),months=nrow(d),
       remaining=12-nrow(d),monthly_rate=mean(tail(d$actual_subsidy_kr,3)),
       estimate=sum(d$actual_subsidy_kr)+(12-nrow(d))*mean(tail(d$actual_subsidy_kr,3)),
       reason='Observeret tilskud til dato + resterende måneder × gennemsnit af seneste tre afsluttede måneder. Ingen sæsonkorrektion; populationsdækning kan være uverificeret.')
}
# Product-level price scenario. Reuses latest observed prices prospectively only;
# products absent from any recent month remain unmodelled, never zero-filled.
price_projection <- function(prices,months,year,today=Sys.Date()) {
  fail<-function(reason)list(available=FALSE,reason=reason)
  if(as.integer(year)!=as.integer(format(today,'%Y')))return(fail('Prisfremskrivning tilbydes kun for indeværende år.'))
  m<-months[substr(months$month,1,4)==as.character(year) & months$month<format(today,'%Y-%m'),,drop=FALSE]
  if(nrow(m)<3L)return(fail('Utilstrækkeligt datagrundlag: mindst tre afsluttede måneder kræves.'))
  last<-max(as.integer(substr(m$month,6,7)))
  if(!identical(m$month,sprintf('%d-%02d',year,seq_len(last))) ||
     any(!m$status %in% c('query_complete_population_unverified','population_verified')))
    return(fail('Utilstrækkeligt datagrundlag: indlæs afgrænsningen fra januar uden månedshuller.'))
  recent<-tail(m$month,3)
  z<-prices[format(as.Date(prices$month),'%Y-%m') %in% recent,,drop=FALSE]
  if(!nrow(z))return(fail('Ingen produktmålinger i de seneste tre valgte måneder.'))
  groups<-split(z,interaction(z$atc5,z$varenummer,drop=TRUE))
  products<-do.call(rbind,lapply(groups,function(d){
    complete<-nrow(d)==3L && setequal(format(as.Date(d$month),'%Y-%m'),recent) && all(!is.na(d$ddd))
    volume<-if(complete)mean(d$ddd)*(12-last) else NA_real_
    price<-d$tsp_per_ddd[1]
    eligible<-complete && is.finite(volume) && volume>=0 && !is.na(price) &&
      all(d$linkage_status=='review_ddd_definition_and_price_date')
    data.frame(atc5=d$atc5[1],varenummer=d$varenummer[1],recent_ddd=observed_sum(d$ddd),
      future_ddd=volume,price_per_ddd=price,modelled_future_basis_kr=if(eligible)volume*price else NA_real_,
      status=if(!complete)'Manglende nylig produktmåling'else if(!eligible)'Intet egnet pris/DDD-grundlag'else 'Scenarie beregnet')
  }))
  list(available=TRUE,products=products,remaining=12-last,
    reason='Resten af året: gennemsnitlig produkt-DDD i seneste tre valgte afsluttede måneder × resterende måneder × senest observeret tilskudspris/DDD. Uændret pris og fortsat produktanvendelse er antagelser; nye/udgåede produkter modelleres ikke uden tilstrækkelige målinger.')
}
