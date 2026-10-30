# Public entry point: source this file, then use monitor_config()/with_monitor_db().
monitor_root <- local({
  f <- tryCatch(sys.frame(1)$ofile, error=function(e) NULL)
  if (is.null(f)) stop('Source monitorering/R/core.R with source().')
  normalizePath(file.path(dirname(f), '..'))
})
for (pkg in c('DBI','duckdb','jsonlite','digest','filelock')) {
  if (!requireNamespace(pkg,quietly=TRUE)) stop('Install required package: ',pkg)
}
`%or%` <- function(x,y) if (is.null(x)) y else x
must <- function(ok,msg) if (length(ok)!=1L || is.na(ok) || !ok) stop(msg,call.=FALSE)
sha <- function(path) digest::digest(file=path,algo='sha256')
json <- function(x) as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null='null',na='null',digits=16))
read_meta <- function(path) {
  m <- jsonlite::fromJSON(path,simplifyVector=TRUE)
  must(is.list(m),'Metadata must be a JSON object')
  check <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    must(!any(tolower(names(x)) %in% c('authorization','password','access_token','embed_token','credentials')),
         'Credentials must not be stored in metadata')
    lapply(x,check); invisible(NULL)
  }
  check(m); m
}
scalar <- function(x,label) {
  must(is.character(x) && length(x)==1L && !is.na(x) && nzchar(x),paste('Missing/invalid',label)); x
}
valid_prefix <- function(x) is.character(x) && length(x)==1L &&
  grepl('^[A-Z]([0-9]{2}([A-Z]([A-Z]([0-9]{2})?)?)?)?$',x)
month_date <- function(x) {
  must(is.character(x) && all(grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',x)),'Invalid YYYY-MM')
  as.Date(paste0(x,'-01'))
}
iso_time <- function(x) {
  scalar(x,'UTC source timestamp')
  must(grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$',x),
       'Timestamp must be ISO-8601 UTC ending in Z')
  must(!is.na(as.POSIXct(x,format='%Y-%m-%dT%H:%M:%OS',tz='UTC')),'Invalid timestamp'); x
}
monitor_config <- function() {
  runtime <- Sys.getenv('MONITOR_RUNTIME',file.path(monitor_root,'runtime'))
  list(db=Sys.getenv('MONITOR_DB',file.path(runtime,'monitorering.duckdb')),
       archive=Sys.getenv('MONITOR_ARCHIVE',file.path(runtime,'archive')),
       log=Sys.getenv('MONITOR_LOG',file.path(runtime,'events.jsonl')))
}
log_event <- function(cfg,event,...) {
  dir.create(dirname(cfg$log),recursive=TRUE,showWarnings=FALSE)
  cat(json(c(list(time=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),event=event),list(...))),
      '\n',file=cfg$log,append=TRUE,sep='')
}
with_monitor_db <- function(cfg=monitor_config(),write=FALSE,code) {
  if (write) dir.create(dirname(cfg$db),recursive=TRUE,showWarnings=FALSE)
  lock <- NULL
  if (write) {
    lock <- filelock::lock(paste0(cfg$db,'.writer.lock'),timeout=0)
    must(!is.null(lock),'Another writer holds the database lock')
    on.exit(filelock::unlock(lock),add=TRUE)
  }
  con <- DBI::dbConnect(duckdb::duckdb(),dbdir=cfg$db,read_only=!write)
  on.exit(DBI::dbDisconnect(con,shutdown=TRUE),add=TRUE)
  code(con)
}
init_db <- function(con) {
  if (DBI::dbExistsTable(con,'schema_version'))
    must(identical(as.integer(DBI::dbGetQuery(con,'SELECT version FROM schema_version')$version),1L),
         'Unsupported database schema; explicit migration required')
  DBI::dbWithTransaction(con,{
    for (f in c('001_schema.sql','002_views.sql')) {
      sql <- paste(readLines(file.path(monitor_root,'sql',f),warn=FALSE),collapse='\n')
      sql <- gsub('--[^\n]*','',sql)
      for (s in strsplit(sql,';',fixed=TRUE)[[1]]) if (nzchar(trimws(s))) DBI::dbExecute(con,s)
    }
  })
}
archive_file <- function(cfg,path,hash=sha(path)) {
  dest <- file.path(cfg$archive,hash,basename(path))
  dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE,mode='0700')
  if (file.exists(dest)) must(identical(sha(dest),hash),'Corrupt existing raw archive') else {
    tmp <- tempfile(tmpdir=dirname(dest))
    on.exit(unlink(tmp),add=TRUE)
    must(file.copy(path,tmp),'Raw archive copy failed')
    must(identical(sha(tmp),hash),'Raw archive checksum mismatch')
    must(file.rename(tmp,dest),'Raw archive atomic rename failed')
    Sys.chmod(dest,'0400')
  }
  normalizePath(dest)
}
source(file.path(monitor_root,'R','consumption.R'))
source(file.path(monitor_root,'R','tariff.R'))
source(file.path(monitor_root,'R','operations.R'))
