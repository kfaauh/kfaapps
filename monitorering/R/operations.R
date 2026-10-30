validate_db <- function(con,cfg,archives=TRUE) {
  must(identical(as.integer(DBI::dbGetQuery(con,'SELECT version FROM schema_version')$version),1L),'Schema version mismatch')
  bad <- DBI::dbGetQuery(con,paste('SELECT count(*) n FROM consumption_extract e LEFT JOIN',
    '(SELECT extract_id,count(*) n FROM consumption_observation GROUP BY extract_id) o USING(extract_id)',
    'WHERE e.row_count<>coalesce(o.n,0)'))$n
  must(bad==0,'Consumption extraction row counts are inconsistent')
  must(DBI::dbGetQuery(con,paste('SELECT count(*) n FROM consumption_observation o JOIN consumption_extract e USING(extract_id)',
    'WHERE o.month<>e.month OR NOT starts_with(o.atc5,e.atc_prefix)'))$n==0,'Scope integrity failure')
  if(archives) {
    c <- DBI::dbGetQuery(con,'SELECT archive_csv,archive_manifest,csv_sha256,manifest_sha256 FROM consumption_extract')
    for(i in seq_len(nrow(c))) {
      must(file.exists(c$archive_csv[i]) && identical(sha(c$archive_csv[i]),c$csv_sha256[i]),'Consumption archive corruption/missing file')
      must(file.exists(c$archive_manifest[i]) && identical(sha(c$archive_manifest[i]),c$manifest_sha256[i]),'Manifest archive corruption/missing file')
      validate_consumption(c$archive_csv[i],c$archive_manifest[i])
    }
    t <- DBI::dbGetQuery(con,'SELECT archive_raw,archive_metadata,raw_sha256 FROM tariff_snapshot')
    for(i in seq_len(nrow(t))) {
      must(file.exists(t$archive_raw[i]) && identical(sha(t$archive_raw[i]),t$raw_sha256[i]),'Tariff archive corruption/missing file')
      must(file.exists(t$archive_metadata[i]),'Tariff metadata archive missing')
      must(identical(basename(dirname(t$archive_metadata[i])),sha(t$archive_metadata[i])),'Tariff metadata archive corruption')
    }
  }
  DBI::dbGetQuery(con,paste("SELECT 'consumption_extract' AS object,count(*) AS rows FROM consumption_extract UNION ALL",
    "SELECT 'consumption_observation',count(*) FROM consumption_observation UNION ALL SELECT 'tariff_snapshot',count(*) FROM tariff_snapshot UNION ALL SELECT 'tariff_product',count(*) FROM tariff_product"))
}
# Call only with exclusive writer lock held, after CHECKPOINT and disconnect.
backup_monitor <- function(cfg,destination) {
  dir.create(destination,recursive=TRUE,showWarnings=FALSE,mode='0700')
  lock <- filelock::lock(paste0(cfg$db,'.writer.lock'),timeout=0)
  must(!is.null(lock),'Another writer is active; backup cancelled')
  on.exit(filelock::unlock(lock),add=TRUE)
  con <- DBI::dbConnect(duckdb::duckdb(),dbdir=cfg$db)
  tryCatch({validate_db(con,cfg); DBI::dbExecute(con,'CHECKPOINT')},
           finally=DBI::dbDisconnect(con,shutdown=TRUE))
  stamp <- format(Sys.time(),'%Y%m%dT%H%M%SZ',tz='UTC')
  final <- file.path(destination,paste0('monitorering-',stamp))
  must(!dir.exists(final),'Backup already exists at timestamp')
  tmp <- tempfile('backup-',tmpdir=destination); dir.create(tmp,mode='0700')
  on.exit(if(dir.exists(tmp)) unlink(tmp,recursive=TRUE),add=TRUE)
  must(file.copy(cfg$db,file.path(tmp,'monitorering.duckdb')),'Database backup failed')
  dir.create(file.path(tmp,'archive'))
  files <- list.files(cfg$archive,full.names=TRUE,all.files=TRUE,no..=TRUE)
  if(length(files)) must(all(file.copy(files,file.path(tmp,'archive'),recursive=TRUE)),'Archive backup failed')
  all <- list.files(tmp,full.names=TRUE,recursive=TRUE)
  sums <- setNames(lapply(all,sha),substring(all,nchar(tmp)+2))
  jsonlite::write_json(list(database_sha256=sha(cfg$db),files=sums),file.path(tmp,'checksums.json'),auto_unbox=TRUE,pretty=TRUE)
  must(file.rename(tmp,final),'Backup atomic finalization failed')
  log_event(cfg,'backup',path=normalizePath(final)); normalizePath(final)
}
import_inbox <- function(con,cfg,inbox) {
  must(dir.exists(inbox),'Inbox does not exist')
  manifests <- sort(list.files(inbox,pattern='^monitorering_manifest_.*\\.json$',full.names=TRUE))
  results <- list(); failed <- FALSE
  for(m in manifests) {
    csv <- file.path(dirname(m),sub('^monitorering_manifest_(.*)\\.json$','monitorering_forbrug_\\1.csv',basename(m)))
    result <- tryCatch({must(file.exists(csv),'CSV pair not ready'); import_consumption(con,cfg,csv,m)},error=function(e) {
      log_event(cfg,'inbox_rejected',file=basename(m),reason=conditionMessage(e)); failed <<- TRUE; NA_character_
    })
    results[[basename(m)]] <- result
  }
  if(failed) stop('One or more inbox pairs failed; see event log. Successful pairs remain committed.',call.=FALSE)
  results
}
restore_monitor <- function(cfg,backup) {
  must(!file.exists(cfg$db) && !dir.exists(cfg$archive),'Restore requires a new empty database and archive location')
  sums <- jsonlite::fromJSON(file.path(backup,'checksums.json'),simplifyVector=FALSE)
  for(rel in names(sums$files)) {
    must(!grepl('(^/|(^|/)\\.\\.(/|$))',rel),'Unsafe backup path')
    p <- file.path(backup,rel)
    must(file.exists(p)&&identical(sha(p),sums$files[[rel]]),'Backup checksum verification failed')
  }
  must(identical(sha(file.path(backup,'monitorering.duckdb')),sums$database_sha256),'Backup database checksum mismatch')
  dir.create(dirname(cfg$db),recursive=TRUE,showWarnings=FALSE)
  lock <- filelock::lock(paste0(cfg$db,'.writer.lock'),timeout=0)
  must(!is.null(lock),'Writer lock is held');on.exit(filelock::unlock(lock),add=TRUE)
  must(file.copy(file.path(backup,'monitorering.duckdb'),cfg$db),'Database restore copy failed')
  dir.create(cfg$archive,recursive=TRUE,mode='0700')
  src <- list.files(file.path(backup,'archive'),full.names=TRUE,all.files=TRUE,no..=TRUE)
  if(length(src)) must(all(file.copy(src,cfg$archive,recursive=TRUE)),'Archive restore copy failed')
  con <- DBI::dbConnect(duckdb::duckdb(),dbdir=cfg$db)
  on.exit(DBI::dbDisconnect(con,shutdown=TRUE),add=TRUE)
  # Only local raw-file locations change. Source data and metadata remain immutable.
  DBI::dbWithTransaction(con,{
    for(spec in list(c('consumption_extract','extract_id','archive_csv','archive_manifest'),
                    c('tariff_snapshot','snapshot_id','archive_raw','archive_metadata'))) {
      d <- DBI::dbGetQuery(con,paste('SELECT * FROM',spec[1]))
      for(i in seq_len(nrow(d))) for(column in spec[-(1:2)]) {
        old <- d[[column]][i]
        dest <- normalizePath(file.path(cfg$archive,basename(dirname(old)),basename(old)),mustWork=TRUE)
        DBI::dbExecute(con,paste('UPDATE',spec[1],'SET',column,'=? WHERE',spec[2],'=?'),params=list(dest,d[[spec[2]]][i]))
      }
    }
  })
  validate_db(con,cfg);log_event(cfg,'restore',backup=normalizePath(backup))
  invisible(cfg$db)
}
