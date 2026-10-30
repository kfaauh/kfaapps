national_regions <- c('Region Hovedstaden','Region Sjælland','Region Syddanmark','Region Midtjylland','Region Nordjylland')
consumption_metrics <- c('ddd','regionalt_tilskud_kr')
validate_consumption <- function(csv,manifest) {
  m <- read_meta(manifest)
  must(length(m$schema_version)==1L && is.numeric(m$schema_version) && m$schema_version %in% c(1,2),'Unsupported consumption manifest version')
  must(isTRUE(m$complete_query_response) && identical(as.integer(m$http),200L),'Incomplete/failed source response')
  must(valid_prefix(m$atc_prefix),'Invalid ATC prefix')
  month_date(m$period); iso_time(m$extracted_at); scalar(m$source,'source')
  must(length(m$region_filter)==5L && setequal(m$region_filter,national_regions),'Expected all five regions exactly once')
  must(length(m$metrics)==2L && setequal(m$metrics,consumption_metrics),'Expected both metrics')
  for(k in c('row_count','atc5_count')) must(is.numeric(m[[k]]) && length(m[[k]])==1 &&
    is.finite(m[[k]]) && m[[k]]>=0 && m[[k]]==floor(m[[k]]),paste('Invalid',k))
  must(!isTRUE(m$has_restart_token) && !isTRUE(m$response_truncated),'Manifest reports incomplete response')
  must(isTRUE(m$zero_filter),'Expected validated nonzero-filter contract')
  must(is.numeric(m$duration_ms) && length(m$duration_ms)==1L && is.finite(m$duration_ms) && m$duration_ms>=0,'Invalid HTTP duration')
  if (m$schema_version==2) {
    must(identical(m$csv_sha256,sha(csv)),'CSV checksum mismatch')
    must(identical(m$query_contract,'compact-national-v1'),'Unexpected query/filter contract')
    must(is.character(m$query_sha256) && length(m$query_sha256)==1 && grepl('^[0-9a-f]{64}$',m$query_sha256),'Missing query fingerprint')
    must(isFALSE(m$has_restart_token),'RestartToken/continuation response not complete')
  }
  d <- read.table(csv,header=TRUE,sep=';',quote='"',comment.char='',colClasses='character',
                  check.names=FALSE,fileEncoding='UTF-8-BOM',na.strings=NULL,fill=FALSE)
  must(identical(names(d),c('atc5','varenummer','aar_maaned','maaling','vaerdi')),'Unexpected CSV columns')
  must(nrow(d)==m$row_count,'CSV/manifest row count mismatch')
  must(nrow(d)>0 || (m$schema_version==2 && isTRUE(m$allow_empty)),'Empty unverified extraction')
  must(!anyNA(d) && all(nzchar(as.matrix(d))),'Missing CSV cell')
  must(all(grepl('^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}$',d$atc5)) && all(startsWith(d$atc5,m$atc_prefix)),'ATC outside extraction scope')
  must(all(grepl('^[0-9]{6}$',d$varenummer)),'Varenummer must be six-digit text')
  must(all(d$aar_maaned==m$period),'CSV month outside manifest scope')
  must(all(d$maaling %in% consumption_metrics),'Unknown metric')
  must(all(grepl('^-?[0-9]+(\\.[0-9]{1,8})?$',d$vaerdi)),'Noncanonical/overprecision numeric value')
  v <- suppressWarnings(as.numeric(d$vaerdi))
  must(all(is.finite(v) & abs(v)<1e20 & v!=0),'Invalid/zero/overflow metric value')
  must(!anyDuplicated(d[1:4]),'Duplicate observation key')
  must(length(unique(d$atc5))==m$atc5_count,'CSV/manifest ATC count mismatch')
  for (metric in consumption_metrics) {
    expected <- m[[paste0('total_',metric)]]
    must(is.numeric(expected) && length(expected)==1L && is.finite(expected),'Missing metric total')
    must(abs(sum(v[d$maaling==metric])-expected)<=1e-6,'CSV/manifest metric total mismatch')
    if (m$schema_version==2) must(sum(d$maaling==metric)==m$metric_row_counts[[metric]],'Metric row count mismatch')
  }
  list(data=d,manifest=m,csv_sha=sha(csv),manifest_sha=sha(manifest))
}
import_consumption <- function(con,cfg,csv,manifest) {
  x <- validate_consumption(csv,manifest); m <- x$manifest; d <- x$data
  id <- digest::digest(paste(x$csv_sha,x$manifest_sha,sep=':'),algo='sha256',serialize=FALSE)
  if (DBI::dbGetQuery(con,'SELECT count(*) n FROM consumption_extract WHERE extract_id=?',params=list(id))$n>0) return(id)
  # Equal source times and overlapping scopes are ambiguous; never select by arrival order.
  conflict <- DBI::dbGetQuery(con,paste(
    'SELECT count(*) n FROM consumption_extract WHERE month=? AND extracted_at=?::TIMESTAMPTZ',
    'AND (starts_with(atc_prefix,?) OR starts_with(?,atc_prefix))'),
    params=list(as.character(month_date(m$period)),m$extracted_at,m$atc_prefix,m$atc_prefix))$n
  must(conflict==0,'Conflicting extraction at same source time and overlapping scope')
  ac <- archive_file(cfg,csv,x$csv_sha); am <- archive_file(cfg,manifest,x$manifest_sha)
  DBI::dbWithTransaction(con,{
    DBI::dbExecute(con,paste('INSERT INTO consumption_extract',
      '(extract_id,source,extracted_at,atc_prefix,month,csv_sha256,manifest_sha256,row_count,query_complete,population_coverage,manifest_json,archive_csv,archive_manifest)',
      'VALUES (?,?,?::TIMESTAMPTZ,?,?::DATE,?,?,?,true,\'unverified\',?,?,?)'),
      params=list(id,m$source,m$extracted_at,m$atc_prefix,as.character(month_date(m$period)),
                  x$csv_sha,x$manifest_sha,nrow(d),json(m),ac,am))
    if (nrow(d)) {
      stage <- data.frame(extract_id=id,atc5=d$atc5,varenummer=d$varenummer,
                          month=paste0(d$aar_maaned,'-01'),metric=d$maaling,value=d$vaerdi)
      DBI::dbWriteTable(con,'consumption_stage',stage,temporary=TRUE,overwrite=TRUE)
      DBI::dbExecute(con,paste('INSERT INTO consumption_observation SELECT extract_id,atc5,varenummer,',
                              'month::DATE,metric,value::DECIMAL(28,8) FROM consumption_stage'))
      DBI::dbRemoveTable(con,'consumption_stage')
    }
  })
  log_event(cfg,'consumption_import',extract_id=id,rows=nrow(d),prefix=m$atc_prefix,period=m$period)
  id
}
atc_children <- function(prefix) {
  n <- nchar(prefix)
  if(n==7) return(character())
  suffix <- if(n %in% c(1,5)) sprintf('%02d',0:99) else LETTERS
  paste0(prefix,suffix)
}
coverage_report <- function(con,prefix,from,to) {
  must(valid_prefix(prefix),'Invalid ATC prefix')
  dates <- seq(month_date(from),month_date(to),by='month')
  do.call(rbind,lapply(dates,function(day) {
    day <- as.Date(day,origin='1970-01-01')
    all <- DBI::dbGetQuery(con,paste('SELECT e.extract_id,e.atc_prefix,e.extracted_at,a.evidence',
      'FROM consumption_extract e LEFT JOIN coverage_attestation a USING(extract_id)',
      'WHERE e.month=?::DATE ORDER BY e.extracted_at DESC'),params=list(as.character(day)))
    related <- startsWith(all$atc_prefix,prefix)|startsWith(prefix,all$atc_prefix)
    all <- all[related,,drop=FALSE]
    # Completeness can be assembled only from exhaustive next-level partitions,
    # including explicitly empty responses. Observed ATCs alone cannot prove it.
    covered <- function(p) {
      if(any(startsWith(p,all$atc_prefix))) return(TRUE)
      if(!any(startsWith(all$atc_prefix,p))) return(FALSE)
      kids <- atc_children(p)
      length(kids)>0 && all(vapply(kids,covered,logical(1)))
    }
    scope_complete <- covered(prefix)
    # Check all current/overriding captures conservatively before declaring verified.
    verified <- scope_complete && nrow(all)>0 && all(!is.na(all$evidence))
    data.frame(prefix=prefix,month=format(day,'%Y-%m'),status=if(scope_complete) {
      if(verified) 'population_verified' else 'query_complete_population_unverified'
    } else if(nrow(all)) 'partial_subscopes' else 'missing',
    capture_count=nrow(all),extract_id=if(nrow(all)) all$extract_id[1] else NA_character_)
  }))
}
attest_coverage <- function(con,id,evidence_file) {
  text <- paste(readLines(evidence_file,warn=FALSE),collapse='\n')
  must(nchar(trimws(text))>=40L,'Supply substantive external coverage evidence')
  must(DBI::dbGetQuery(con,'SELECT count(*) n FROM consumption_extract WHERE extract_id=?',params=list(id))$n==1,'Unknown extract')
  DBI::dbExecute(con,'INSERT INTO coverage_attestation(extract_id,evidence) VALUES (?,?)',params=list(id,text))
}
