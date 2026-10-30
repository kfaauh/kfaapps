# Strict live collection and publication, isolated from legacy apps and other jobs.
price_period <- function(page) {
  text <- rvest::html_text2(page)
  p <- 'Prisperiode:[[:space:]]*([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})[[:space:]]*[-–—][[:space:]]*([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})'
  hit <- regmatches(text,regexec(p,text,ignore.case=TRUE))[[1]]
  must(length(hit)==3,'Source price-period banner missing')
  dates <- as.Date(hit[2:3],'%d.%m.%Y')
  must(!anyNA(dates)&&dates[2]>=dates[1],'Invalid source price period')
  list(from=as.character(dates[1]),to=as.character(dates[2]))
}
assert_price_page <- function(page) {
  login <- rvest::html_element(page,'#CanvasHolder_LoginBox_MailBox')
  must(inherits(login,'xml_missing'),'Source returned a login page')
}
live_groups <- function(page,expected) {
  node <- rvest::html_element(page,'select[name="ctl00$CanvasHolder$SearchBox$UdleveringDropdown"]')
  must(!inherits(node,'xml_missing'),'Delivery-group selector missing')
  values <- rvest::html_attr(rvest::html_elements(node,'option'),'value')
  values <- unique(values[!is.na(values)&!values %in% c('','0','-1','Alle','alle')])
  must(length(values)>=length(expected)&&length(values)<=100,'Unexpected delivery-group catalogue')
  must(all(expected %in% values),'A previously supported delivery group disappeared; review the source')
  must(all(grepl('^([A-Z][A-Z0-9]*|Udgaaet)$',values)),'Unknown delivery-group value format')
  values
}
search_state <- function(page,group) {
  assert_price_page(page)
  selected <- rvest::html_element(page,'select[name="ctl00$CanvasHolder$SearchBox$UdleveringDropdown"] option[selected]')
  must(identical(rvest::html_attr(selected,'value'),group),'Source did not confirm the requested delivery group')
  box <- rvest::html_element(page,'#CanvasHolder_SearchResultBox')
  must(!inherits(box,'xml_missing'),'Search-result container missing; cannot infer zero results')
  text <- tolower(stringr::str_squish(rvest::html_text2(box)))
  patterns <- c('(ingen|intet) (søgeresultat(er)?|resultat(er)?|lægemidler|medicin|match)',
                '(søgningen|din søgning) (gav|giver|har givet) (ingen|intet)',
                'der (blev|er) ikke fundet',
                '(^|[^0-9])0 (resultater|lægemidler|træffere)([^a-zæøå]|$)')
  empty <- any(vapply(patterns,grepl,logical(1),x=text,perl=TRUE))
  cells <- trimws(rvest::html_text2(rvest::html_elements(box,'td')))
  hrefs <- rvest::html_attr(rvest::html_elements(box,'a'),'href')
  products <- any(grepl('^[0-9]{6}$',cells)) || any(grepl('[?&](vnr|varenummer|drugid)=[0-9]+',hrefs,ignore.case=TRUE),na.rm=TRUE)
  must(!(empty&&products),'Contradictory empty-result message and product links')
  list(empty=empty,evidence=if(empty) substr(text,1,1000) else NULL)
}
source_form <- function(env,page,fields) {
  all <- env$extract_all_form_fields(page)
  for(name in names(fields)) all[[name]] <- fields[[name]]
  req <- httr2::request(env$sogning_url)
  req <- do.call(httr2::req_headers,c(list(req),as.list(env$browser_headers)))
  req <- httr2::req_headers(req,Referer=env$sogning_url,Origin=env$base_url)
  req <- do.call(httr2::req_body_form,c(list(req),all))
  req <- httr2::req_options(req,cookiefile=env$cookie_file,cookiejar=env$cookie_file)
  httr2::req_perform(httr2::req_retry(httr2::req_timeout(req,60),max_tries=3))
}
source_search_page <- function(env) {
  req <- httr2::request(env$sogning_url)
  req <- do.call(httr2::req_headers,c(list(req),as.list(env$browser_headers)))
  req <- httr2::req_options(req,cookiefile=env$cookie_file,cookiejar=env$cookie_file)
  page <- httr2::resp_body_html(httr2::req_perform(httr2::req_retry(httr2::req_timeout(req,60),max_tries=3)))
  assert_price_page(page);page
}
strict_group <- function(env,group,run_dir,period) {
  search <- source_form(env,env$current_page,setNames(list(group,'ctl00$CanvasHolder$SearchBox$SearchButton',''),
    c('ctl00$CanvasHolder$SearchBox$UdleveringDropdown','__EVENTTARGET','__EVENTARGUMENT')))
  page <- httr2::resp_body_html(search)
  must(identical(price_period(page),period),'Source price period changed during collection')
  state <- search_state(page,group)
  if(state$empty) {
    message('  ',group,': confirmed empty')
    return(list(data=NULL,check=list(group=group,status='confirmed_empty',rows=0,evidence=state$evidence)))
  }
  export <- source_form(env,page,list('__EVENTTARGET'='ctl00$CanvasHolder$ExcelButton','__EVENTARGUMENT'=''))
  type <- httr2::resp_header(export,'content-type') %or% ''
  if(!grepl('excel|octet-stream|spreadsheet',type,ignore.case=TRUE)) {
    # No raw HTML/viewstate/cookies are logged or archived.
    diagnostic <- file.path(run_dir,paste0('group-failure-',group,'.json'))
    jsonlite::write_json(list(group=group,content_type=type,search_text=substr(rvest::html_text2(rvest::html_element(page,'#CanvasHolder_SearchResultBox')),1,2000)),diagnostic,auto_unbox=TRUE,pretty=TRUE)
    stop('Export for ',group,' returned ',type,' without a confirmed empty search. Capture rejected. Diagnostic: ',diagnostic,call.=FALSE)
  }
  data <- env$parse_xls_response(export,group)
  must(!is.null(data)&&nrow(data)>0,paste('Empty/invalid Excel export for',group))
  message('  ',group,': ',nrow(data),' rows')
  list(data=data,check=list(group=group,status='exported',rows=nrow(data)))
}
collect_tariff <- function(run_dir,catalog) {
  for(p in c('httr2','rvest','readxl','writexl','dplyr','stringr','tibble'))
    must(requireNamespace(p,quietly=TRUE),paste('Missing R package:',p))
  must(nzchar(Sys.getenv('MEDPRIS_EMAIL'))&&nzchar(Sys.getenv('MEDPRIS_PASSWORD')),'Missing configured Medicine Prices credentials')
  src <- file.path(monitor_root,'..','data-scripts','fetch_data2.R')
  exprs <- parse(src,keep.source=FALSE)
  assignment <- function(n) which(vapply(exprs,function(e) is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name(n)),logical(1)))
  for(n in c('email','pw')) must(length(assignment(n))==1,'Upstream credential layout changed')
  exprs[[assignment('email')]] <- quote(email <- Sys.getenv('MEDPRIS_EMAIL'))
  exprs[[assignment('pw')]] <- quote(pw <- Sys.getenv('MEDPRIS_PASSWORD'))
  loops <- which(vapply(exprs,function(e) is.call(e)&&identical(e[[1]],as.name('for'))&&identical(e[[2]],as.name('i')),
                        logical(1)))
  must(length(loops)==1,'Upstream collection-loop layout changed')
  env <- new.env(parent=globalenv());env$.job_env <- env
  env$commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) c(run_dir,catalog) else base::commandArgs()
  env$req_perform <- function(req,...) httr2::req_perform(httr2::req_retry(httr2::req_timeout(req,60),max_tries=3),...)
  env$.job_run <- run_dir
  env$.job_today <- as.Date(format(Sys.time(),'%Y-%m-%d',tz='Europe/Copenhagen'))
  exprs[[loops]] <- quote({
    assert_price_page(sogning_page)
    .job_period <- price_period(sogning_page)
    must(.job_today>=as.Date(.job_period$from)&&.job_today<=as.Date(.job_period$to),'Source banner is stale or future-dated')
    udleveringsgrupper <- live_groups(sogning_page,udleveringsgrupper)
    group_checks <- list()
    for(gruppe in udleveringsgrupper) {
      Sys.sleep(2)
      one <- strict_group(.job_env,gruppe,.job_run,.job_period)
      group_checks[[gruppe]] <- one$check
      jsonlite::write_json(group_checks,file.path(.job_run,'group_checks.json'),auto_unbox=TRUE,pretty=TRUE)
      if(!is.null(one$data)) alle_data[[gruppe]] <- one$data
      current_page <- source_search_page(.job_env)
      must(identical(price_period(current_page),.job_period),'Price period changed during collection')
    }
    must(length(group_checks)==length(udleveringsgrupper),'Unprocessed delivery groups')
    must(sum(vapply(group_checks,function(x) x$rows,numeric(1)))==sum(vapply(alle_data,nrow,integer(1))),'Group row-count mismatch')
    .job_capture_time <- format(Sys.time(),'%Y-%m-%dT%H:%M:%OS3Z',tz='UTC')
  })
  old <- options(timeout=max(180,getOption('timeout',60)));on.exit(options(old),add=TRUE)
  # Whole script evaluation is necessary for legacy on.exit temporary-file cleanup.
  eval(exprs,env)
  must(identical(env$.job_today,as.Date(format(Sys.time(),'%Y-%m-%d',tz='Europe/Copenhagen'))),'Collection crossed Danish midnight')
  env
}
validate_app_tariff <- function(raw_path,rds_path,old_path=NULL,min_rows=10000L,max_drop=0.2) {
  raw <- read_tariff(raw_path);d <- readRDS(rds_path)
  required <- c('Kategori','Underkategori','ATCtekst','ATC','Indholdsstof','Styrke','Form','Pakning',
                'Lægemiddel','Firma','Varenummer','Tilskudssubstitutionsgruppe','Overordnet_substitutionsgruppe',
                'Kan_leveres','Udleveringsgruppe')
  prices <- c('Ekspeditionspris (kr.)','DDD','Ekspeditionspris pr. DDD (kr.)','Tilskudspris (kr.)','Tilskudspris pr. DDD (kr.)')
  must(nrow(d)>=min_rows&&nrow(d)==nrow(raw),'Unexpected raw/prepared row counts')
  must(all(c(required,prices) %in% names(d)),'Prepared app columns missing')
  must(!anyNA(d$Varenummer)&&all(grepl('^[0-9]{6}$',d$Varenummer))&&!anyDuplicated(d$Varenummer),'Invalid or duplicate app product keys')
  rawkeys <- sprintf('%06d',as.integer(raw[['Varenr.']]))
  must(!anyNA(rawkeys)&&setequal(rawkeys,d$Varenummer),'Raw/app product keys differ')
  for(n in prices) must(is.numeric(d[[n]])&&all(is.na(d[[n]])|(is.finite(d[[n]])&d[[n]]>=0)),paste('Invalid app numeric field:',n))
  must(any(!is.na(d[[prices[1]]]))&&any(!is.na(d[[prices[4]]])),'All app prices missing')
  ix <- match(d$Varenummer,rawkeys)
  for(spec in list(c('ESP (kr.)',prices[1]),c('TSP (kr.)',prices[4]))) {
    expected <- number_tariff(raw[[spec[1]]])[ix];actual <- d[[spec[2]]]
    must(identical(is.na(expected),is.na(actual))&&all(abs(expected-actual)<1e-6,na.rm=TRUE),'Raw/app package prices differ')
  }
  if(!is.null(old_path)&&file.exists(old_path)) {
    previous <- readRDS(old_path)
    must(nrow(d)>=nrow(previous)*(1-max_drop),'Product count dropped beyond configured publication threshold')
  }
  invisible(nrow(d))
}
publish_tariff <- function(source,target,backup_dir,restart,restart_fn=function(path) Sys.setFileTime(path,Sys.time())) {
  must(file.exists(target),'Existing app file required before publication')
  must(file.exists(restart)&&file.access(restart,2)==0,'App restart marker not writable')
  dir.create(backup_dir,recursive=TRUE,showWarnings=FALSE,mode='0700')
  stamp <- format(Sys.time(),'%Y%m%dT%H%M%OS3Z',tz='UTC');stamp <- gsub('[^0-9TZ]','',stamp)
  backup <- file.path(backup_dir,paste0('clean_data2-',stamp,'.rds'))
  if(file.exists(target)) must(file.copy(target,backup,copy.mode=FALSE),'Old app file backup failed')
  tmp <- tempfile('.monitorering-',tmpdir=dirname(target))
  on.exit(unlink(tmp),add=TRUE)
  must(file.copy(source,tmp,copy.mode=FALSE),'Staged app copy failed')
  must(identical(sha(source),sha(tmp)),'Staged app checksum mismatch')
  must(Sys.chmod(tmp,'0640',use_umask=FALSE),'Cannot set app file permissions')
  must(file.rename(tmp,target),'Atomic app publication failed')
  # Restart only the target app. Record error even though the validated data was published.
  restarted <- tryCatch(isTRUE(restart_fn(restart)),error=function(e) FALSE)
  if(!restarted) {
    rollback <- tempfile('.monitorering-rollback-',tmpdir=dirname(target));on.exit(unlink(rollback),add=TRUE)
    restored <- file.copy(backup,rollback,copy.mode=FALSE)
    if(restored) {restored <- isTRUE(Sys.chmod(rollback,'0640',use_umask=FALSE));if(restored) restored <- file.rename(rollback,target)}
    stop(if(restored) 'App restart failed; previous app file restored' else 'App restart and rollback failed; restore from app backup',call.=FALSE)
  }
  invisible(backup)
}
# Explicit opt-in: existing tariff services keep working until their operator adds
# MONITOR_PUBLICATION and permits this directory in the systemd sandbox.
republish_tariff_analysis <- function(cfg,destination=Sys.getenv('MONITOR_PUBLICATION',''),
                                      before_replace=function(path) NULL) {
  if(!nzchar(destination)) return(NULL)
  with_monitor_db(cfg,TRUE,function(con) {
    validate_db(con,cfg)
    publish_analysis(con,destination,before_replace=before_replace)
  })
}
