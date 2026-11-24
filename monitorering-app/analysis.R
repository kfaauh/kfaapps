# Pure helpers: app readers require neither DuckDB nor access to the live database.
analysis_regions <- c(DK='Hele Danmark',H='Region Hovedstaden',SJ='Region Sjælland',
  SD='Region Syddanmark',MJ='Region Midtjylland',NJ='Region Nordjylland')
analysis_region_rows <- function(d,region='DK') {
  stopifnot(length(region)==1L,region %in% names(analysis_regions))
  codes <- d[['region']]
  if(is.null(codes)) return(if(region=='DK') d else d[FALSE,,drop=FALSE])
  d[codes==region,,drop=FALSE]
}
validate_analysis_bundle <- function(x) {
  required <- c('consumption','historical_prices','current_prices','tariff_changes','extracts','tariffs','tariff_products')
  stopifnot(is.list(x),identical(x$version,1L)||identical(x$version,2L),is.character(x$published_at),length(x$published_at)==1L,
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
  series_tables <- c('consumption','historical_prices','current_prices')
  key_columns <- c('atc5','varenummer','month')
  for(name in c(series_tables,'extracts')) {
    table <- x$tables[[name]]
    if(x$version==2L) stopifnot('region' %in% names(table),all(table$region %in% names(analysis_regions)))
    else stopifnot(!'region' %in% names(table))
  }
  if(x$version==2L) key_columns <- c(key_columns,'region')
  d <- x$tables$consumption
  key <- function(v) do.call(paste,c(as.list(v[key_columns]),sep='|'))
  stopifnot(!anyDuplicated(d[key_columns]))
  for(name in series_tables[-1]) {
    z <- x$tables[[name]]
    stopifnot(nrow(z)==nrow(d),!anyDuplicated(z[key_columns]),setequal(key(z),key(d)))
  }
  invisible(TRUE)
}
analysis_children <- function(prefix) {
  if(nchar(prefix)==7L) return(character())
  paste0(prefix,if(nchar(prefix) %in% c(1,5)) sprintf('%02d',0:99) else LETTERS)
}
analysis_coverage <- function(extracts,prefix,months,region='DK') {
  extracts <- analysis_region_rows(extracts,region)
  extract_month <- format(as.Date(extracts$month),'%Y-%m')
  do.call(rbind,lapply(as.character(months),function(month) {
    e <- extracts[extract_month==month,,drop=FALSE]
    e <- e[startsWith(e$atc_prefix,prefix)|startsWith(prefix,e$atc_prefix),,drop=FALSE]
    covered <- function(p) {
      if(any(startsWith(p,e$atc_prefix))) return(TRUE)
      if(!any(startsWith(e$atc_prefix,p))) return(FALSE)
      kids <- analysis_children(p)
      length(kids)>0L && all(vapply(kids,covered,logical(1)))
    }
    # Keep audit counts, but only the latest exact scope revision informs verification.
    active <- if('extracted_at' %in% names(e)) {
      current <- e[order(e$extracted_at,decreasing=TRUE),,drop=FALSE]
      current[!duplicated(current$atc_prefix),,drop=FALSE]
    } else e
    complete <- covered(prefix)
    data.frame(month=month,scope=prefix,status=if(complete) {
      if(nrow(active)>0L && all(active$population_verified)) 'population_verified' else 'query_complete_population_unverified'
    } else if(nrow(e)) 'partial_subscopes' else 'missing',captures=nrow(e))
  }))
}
analysis_complete_status <- c('population_verified','query_complete_population_unverified')

analysis_main_groups <- function(bundle) {
  tables <- bundle$tables
  codes <- c(tables$extracts$atc_prefix,tables$consumption$atc5)
  sort(unique(substr(codes[!is.na(codes) & grepl('^[A-Z][0-9]{2}',codes)],1,3)))
}

analysis_extract_months <- function(bundle) {
  dates <- as.Date(c(as.character(bundle$tables$extracts$month),
    as.character(bundle$tables$consumption$month)))
  dates <- dates[!is.na(dates)]
  if(!length(dates)) return(character())
  format(seq(min(dates),max(dates),by='month'),'%Y-%m')
}

analysis_month_state <- function(d) {
  ifelse(d$observation_status=='Observationer findes','observed',
    ifelse(d$products>0,'missing_measure',
      ifelse(d$status %in% analysis_complete_status,'empty',
        ifelse(d$status=='partial_subscopes','partial','missing'))))
}

analysis_data_inventory <- function(bundle,region='DK') {
  months <- analysis_extract_months(bundle)
  groups <- analysis_main_groups(bundle)
  if(!length(months) || !length(groups)) return(data.frame())
  do.call(rbind,lapply(groups,function(group) {
    data <- analysis_months(bundle,group,months[1],tail(months,1),region=region)
    data.frame(group=group,month=data$month,state=analysis_month_state(data))
  }))
}

analysis_default_period <- function(bundle,prefix,region,today=Sys.Date()) {
  d <- analysis_region_rows(bundle$tables$consumption,region)
  e <- analysis_region_rows(bundle$tables$extracts,region)
  matches <- startsWith(d$atc5,prefix) & (!is.na(d$ddd) | !is.na(d$regionalt_tilskud_kr))
  months <- sort(unique(format(as.Date(d$month[matches]),'%Y-%m')))
  if(!length(months)) {
    matches <- startsWith(e$atc_prefix,prefix) | startsWith(prefix,e$atc_prefix)
    months <- sort(unique(format(as.Date(e$month[matches]),'%Y-%m')))
  }
  if(!length(months)) months <- analysis_extract_months(bundle)
  if(!length(months)) months <- format(today,'%Y-%m')
  last <- tail(months,1)
  c(from=max(months[1],paste0(substr(last,1,4),'-01')),to=last)
}

# An all-missing series remains NA. Partial observed sums are labelled elsewhere.
observed_sum <- function(x) if(!length(x)||all(is.na(x))) NA_real_ else sum(x,na.rm=TRUE)
analysis_months <- function(bundle,prefix,from,to,product='',region='DK') {
  dates <- seq(as.Date(paste0(from,'-01')),as.Date(paste0(to,'-01')),by='month')
  months <- format(dates,'%Y-%m')
  coverage <- analysis_coverage(bundle$tables$extracts,prefix,months,region)
  d <- analysis_region_rows(bundle$tables$consumption,region)
  d <- d[startsWith(d$atc5,prefix) & (product=='' | d$varenummer==product),,drop=FALSE]
  observed_month <- format(as.Date(d$month),'%Y-%m')
  do.call(rbind,lapply(seq_along(months),function(i) {
    z <- d[observed_month==months[i],,drop=FALSE]
    cbind(coverage[i,,drop=FALSE],data.frame(ddd=observed_sum(z$ddd),
      actual_subsidy_kr=observed_sum(z$regionalt_tilskud_kr),products=nrow(z),
      observation_status=if(!nrow(z)) 'Ingen observationer' else if(all(is.na(z$ddd)) && all(is.na(z$regionalt_tilskud_kr))) 'Produktrækker uden målinger' else 'Observationer findes',
      missing_ddd=sum(is.na(z$ddd)),missing_subsidy=sum(is.na(z$regionalt_tilskud_kr))))
  }))
}
# Price captures are point observations. A product identity change starts a new comparison.
tariff_catalog <- function(tables) {
  d <- tables$tariff_products
  idx <- match(d$snapshot_id,tables$tariffs$snapshot_id)
  observed_at <- tables$tariffs$observed_at[idx]
  valid <- which(!is.na(observed_at))
  ordered <- valid[order(observed_at[valid],d$snapshot_id[valid],decreasing=TRUE)]
  d <- d[ordered[!duplicated(d$varenummer[ordered])],,drop=FALSE]
  d$label <- d$varenummer
  for(field in intersect(c('atc','name','strength','form','pack'),names(d))) {
    values <- as.character(d[[field]])
    present <- !is.na(values) & nzchar(values)
    d$label[present] <- paste(d$label[present],values[present],sep=' · ')
  }
  d[order(d$label),,drop=FALSE]
}

tariff_product_history <- function(tables,product) {
  p <- tables$tariff_products
  fields <- c('esp','tsp')
  if(!all(fields %in% names(p))) return(data.frame())
  d <- p[p$varenummer==product,,drop=FALSE]
  idx <- match(d$snapshot_id,tables$tariffs$snapshot_id)
  d$observed_at <- as.POSIXct(tables$tariffs$observed_at[idx],tz='UTC')
  d <- d[!is.na(d$observed_at),,drop=FALSE]
  d <- d[order(d$observed_at,d$snapshot_id),,drop=FALSE]
  identity <- intersect(c('atc','name','strength','form','pack'),names(d))
  for(field in fields) {
    previous <- rep(NA_integer_,nrow(d))
    last <- NA_integer_
    for(i in seq_len(nrow(d))) {
      if(i>1L && !identical(as.list(d[i,identity]),as.list(d[i-1L,identity]))) last <- NA_integer_
      if(is.finite(d[[field]][i])) {
        previous[i] <- last
        last <- i
      }
    }
    d[[paste0(field,'_change')]] <- d[[field]]-d[[field]][previous]
    d[[paste0(field,'_previous_at')]] <- d$observed_at[previous]
  }
  d
}

# Estimate only from measured, scope-complete months in the selected year.
# In annualisation, fill unobserved calendar slots explicitly, including earlier gaps.
subsidy_forecast <- function(months,year,today=Sys.Date()) {
  year <- as.integer(year)
  calendar <- sprintf('%d-%02d',year,1:12)
  d <- months[match(calendar,months$month),,drop=FALSE]
  measured <- is.finite(d$actual_subsidy_kr) & calendar<format(today,'%Y-%m')
  usable <- measured & d$status %in% analysis_complete_status
  if(!any(usable)) return(list(available=FALSE,
    reason='Vælg mindst én afsluttet måned med en målt tilskudssum og et udtræk, der dækker den valgte ATC-kode.'))
  observed <- d$actual_subsidy_kr[usable]
  recent <- tail(which(usable),3)
  last <- max(which(usable))
  rate <- mean(d$actual_subsidy_kr[recent])
  remaining <- sum(!usable)
  future <- remaining*rate
  series <- data.frame(month=calendar,
    observed=ifelse(usable,d$actual_subsidy_kr,NA_real_),
    incomplete=usable & (is.na(d$missing_subsidy) | d$missing_subsidy>0),
    estimated=ifelse(usable,NA_real_,rate))
  list(available=TRUE,annualised=any(!usable[seq_len(last)]),
    retrospective=year<as.integer(format(today,'%Y')),months=length(observed),
    period=scenario_period(year,last),observed=sum(observed),
    remaining=remaining,after_last=12L-last,monthly_rate=rate,future=future,
    estimate=sum(observed)+future,missing=sum(d$missing_subsidy[usable],na.rm=TRUE),
    recent_missing=sum(d$missing_subsidy[recent],na.rm=TRUE),
    recent=calendar[recent],last_month=calendar[last],excluded=calendar[measured & !usable],series=series)
}
observed_subsidy_estimate <- subsidy_forecast

# Product-level price scenario. Latest observed prices are scenario assumptions;
# products absent from any recent month remain unmodelled, never zero-filled.
price_projection <- function(prices,months,year,today=Sys.Date()) {
  fail<-function(reason)list(available=FALSE,reason=reason)
  if(as.integer(year)!=as.integer(format(today,'%Y')))return(fail('Prisfremskrivning tilbydes kun for indeværende år.'))
  months<-through_last_observation(months)
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
    priced<-is.finite(price) && price>=0 &&
      all(d$linkage_status %in% c('review_ddd_definition_and_price_date','estimated_from_price_ratio'))
    eligible<-complete && is.finite(volume) && volume>=0 && priced
    data.frame(atc5=d$atc5[1],varenummer=d$varenummer[1],recent_ddd=observed_sum(d$ddd),
      future_ddd=volume,price_per_ddd=price,modelled_future_basis_kr=if(eligible)volume*price else NA_real_,
      ddd_provenance=if(!priced) 'unavailable' else if(any(d$linkage_status=='estimated_from_price_ratio')) 'unverified_ratio' else 'source_or_verified',
      status=if(!complete)'Manglende nylig produktmåling'else if(!eligible)'Intet egnet pris/DDD-grundlag'else 'Scenarie beregnet')
  }))
  list(available=TRUE,products=products,remaining=12-last,period=scenario_period(year,last),recent=recent,
    reason=paste0(scenario_period(year,last),': gennemsnitlig produkt-DDD i seneste tre valgte afsluttede måneder × resterende måneder × senest observeret tilskudspris/DDD. Afledt DDD kan indgå og er påvirket af kildeafrunding. Uændret pris og fortsat produktanvendelse er antagelser; nye/udgåede produkter modelleres ikke uden tilstrækkelige målinger. Priser observeret efter scenarieperiodens start anvendes også på tidligere scenariemåneder som en kontrafaktisk antagelse, ikke dokumenteret historik.'))
}

# Ignore trailing requests without any measured values, never bridge internal holes.
through_last_observation <- function(months) {
  if(!all(c('ddd','actual_subsidy_kr','products') %in% names(months))) return(months)
  has <- months$products>0 & (!is.na(months$ddd) | !is.na(months$actual_subsidy_kr))
  if(!any(has)) return(months[FALSE,,drop=FALSE])
  months[seq_len(max(which(has))),,drop=FALSE]
}
format_da <- function(x,digits=0L) {
  out <- formatC(x,format='f',digits=digits,big.mark='.',decimal.mark=',')
  out[is.na(x)|!is.finite(x)] <- 'Ikke tilgængelig'
  out
}
money_da <- function(x) {
  if(length(x)!=1L || is.na(x) || !is.finite(x)) return('Ikke tilgængelig')
  if(abs(x)>=1e6) paste(format_da(x/1e6,1),'mio. kr.') else paste(format_da(x),'kr.')
}
scenario_period <- function(year,last) {
  names <- c('januar','februar','marts','april','maj','juni','juli','august','september','oktober','november','december')
  if(last>=12L) return(paste('Ingen scenariemåneder efter december',year))
  paste0('Scenarie ',names[last+1L],if(last<11L) '–december' else '', ' ',year)
}
# Match month strings rather than row positions (leap years and missing months).
compare_months <- function(current,previous) {
  key <- sprintf('%d%s',as.integer(substr(current$month,1,4))-1,substr(current$month,5,7))
  ix <- match(key,previous$month)
  partial <- function(status,missing) !status %in% analysis_complete_status | is.na(missing) | missing>0
  partial_ddd <- partial(current$status,current$missing_ddd)
  previous_partial_ddd <- partial(previous$status[ix],previous$missing_ddd[ix])
  partial_subsidy <- partial(current$status,current$missing_subsidy)
  previous_partial_subsidy <- partial(previous$status[ix],previous$missing_subsidy[ix])
  pct <- function(a,b,incomplete) ifelse(incomplete | is.na(a) | is.na(b) | b==0,NA_real_,100*(a/b-1))
  data.frame(month=current$month,status=current$status,previous_status=previous$status[ix],
    ddd=current$ddd,previous_ddd=previous$ddd[ix],
    ddd_change_pct=pct(current$ddd,previous$ddd[ix],partial_ddd | previous_partial_ddd),
    actual_subsidy_kr=current$actual_subsidy_kr,previous_subsidy_kr=previous$actual_subsidy_kr[ix],
    subsidy_change_pct=pct(current$actual_subsidy_kr,previous$actual_subsidy_kr[ix],partial_subsidy | previous_partial_subsidy),
    partial_ddd=partial_ddd,previous_partial_ddd=previous_partial_ddd,
    partial_subsidy=partial_subsidy,previous_partial_subsidy=previous_partial_subsidy,
    incomplete=partial_ddd | partial_subsidy | is.na(current$ddd) | is.na(current$actual_subsidy_kr),
    previous_incomplete=previous_partial_ddd | previous_partial_subsidy | is.na(previous$ddd[ix]) | is.na(previous$actual_subsidy_kr[ix]))
}
