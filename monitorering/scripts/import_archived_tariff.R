#!/usr/bin/env Rscript
# Preserve an existing archive with honest unknown validity and completeness.
a <- commandArgs(trailingOnly=TRUE)
if(length(a)!=1) stop('Usage: import_archived_tariff.R EXISTING_TARIFF_FILE')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
path <- normalizePath(a[1],mustWork=TRUE)
info <- file.info(path)
must(!is.na(info$mtime),'File modification time missing')
d <- read_tariff(path)
m <- list(schema_version=1,source='erhverv.medicinpriser.dk / legacy local archive',
 observed_at=format(info$mtime,'%Y-%m-%dT%H:%M:%OS3Z',tz='UTC'),
 observed_at_basis='filesystem_mtime_proxy_not_verified_source_capture_time',
 source_filename=basename(path),raw_sha256=sha(path),row_count=nrow(d),
 nominal_from=NULL,nominal_to=NULL,validity_kind='observed_only',complete=FALSE,
 unknown_period_reason='Original file has no verified nominal price-period metadata',
 completeness_note='Legacy fetch may skip groups; source completeness has not been independently verified')
# Stable metadata makes a repeated import of an unchanged archive a no-op.
tmp <- tempfile(fileext='.json')
jsonlite::write_json(m,tmp,auto_unbox=TRUE,null='null',pretty=TRUE)
cfg <- monitor_config()
with_monitor_db(cfg,TRUE,function(con) {
 version <- as.integer(DBI::dbGetQuery(con,'SELECT version FROM schema_version')$version)
 must(identical(version,2L),'Run monitor.R init to upgrade the database first (take a backup before upgrading)')
 id <- import_tariff(con,cfg,path,tmp)
 cat('Archived tariff snapshot:',id,'\n')
 cat('Original rows:',nrow(d),'\n')
 cat('Validity period: unknown. Source completeness: unverified. Excluded from automatic price scenarios.\n')
 print(validate_db(con,cfg))
})
unlink(tmp)
