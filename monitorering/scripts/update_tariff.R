#!/usr/bin/env Rscript
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
source(file.path(monitor_root,'R','tariff_job.R'))
cfg <- monitor_config()
job <- function() {
 dir.create(dirname(cfg$db),recursive=TRUE,showWarnings=FALSE)
 lock <- filelock::lock(file.path(dirname(cfg$db),'tariff-job.lock'),timeout=0)
 must(!is.null(lock),'Another tariff update is running');on.exit(filelock::unlock(lock),add=TRUE)
 catalog <- normalizePath(Sys.getenv('MONITOR_CATALOG'),mustWork=TRUE)
 target <- normalizePath(Sys.getenv('MONITOR_APP_RDS'),mustWork=TRUE)
 restart <- normalizePath(Sys.getenv('MONITOR_APP_RESTART'),mustWork=TRUE)
 must(file.access(dirname(target),2)==0&&file.access(restart,2)==0,'App data directory/restart marker not writable')
 minimum <- as.integer(Sys.getenv('MONITOR_MIN_ROWS','10000'));drop <- as.numeric(Sys.getenv('MONITOR_MAX_ROW_DROP','0.2'))
 must(!is.na(minimum)&&minimum>0&&!is.na(drop)&&drop>=0&&drop<1,'Invalid row quality thresholds')
 # Require an existing initialized database; never silently start a new store at a typo path.
 must(file.exists(cfg$db),'Initialize the intended database first')
 with_monitor_db(cfg,FALSE,function(con) validate_db(con,cfg))
 backup_dir <- file.path(dirname(cfg$db),'backups')
 manifests <- list.files(backup_dir,pattern='^checksums\\.json$',recursive=TRUE,full.names=TRUE)
 if(!length(manifests)||as.numeric(difftime(Sys.time(),max(file.info(manifests)$mtime),units='days'))>=7)
   backup_monitor(cfg,backup_dir)
 run <- tempfile(paste0('run-',format(Sys.time(),'%Y%m%dT%H%M%SZ',tz='UTC'),'-'),tmpdir=dirname(cfg$db))
 dir.create(run,mode='0700')
 log_event(cfg,'tariff_job_started',run=run)
 env <- collect_tariff(run,catalog)
 must(file.exists(env$xlsx_file)&&file.exists(env$rds_file),'Collector output missing')
 validate_app_tariff(env$xlsx_file,env$rds_file,target,minimum,drop)
 m <- list(schema_version=1,source='erhverv.medicinpriser.dk / strict daily capture',
   observed_at=env$.job_capture_time,nominal_from=env$.job_period$from,nominal_to=env$.job_period$to,
   validity_kind='observed_only',complete=TRUE,
   completeness_evidence='Every live dropdown group exported and parsed, or explicitly confirmed empty in its current search response. This verifies requested-source-group processing, not independent population coverage.',
   raw_sha256=sha(env$xlsx_file),row_count=nrow(env$samlet),fetch_script_sha256=sha(file.path(monitor_root,'..','data-scripts','fetch_data2.R')),
   group_checks=env$group_checks,collector_sha256=sha(file.path(monitor_root,'R','tariff_job.R')),ddd_note='Legacy derived DDD is not imported as a verified denominator')
 meta <- file.path(run,'metadata.json');jsonlite::write_json(m,meta,auto_unbox=TRUE,pretty=TRUE,null='null')
 id <- with_monitor_db(cfg,TRUE,function(con) {init_db(con);import_tariff(con,cfg,env$xlsx_file,meta)})
 app_backup <- publish_tariff(env$rds_file,target,file.path(dirname(cfg$db),'app-backups'),restart)
 analysis_publication <- republish_tariff_analysis(cfg)
 summary <- list(analysis_publication=analysis_publication,status='published',time=format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC'),snapshot_id=id,
   rows=nrow(env$samlet),run=run,nominal_period=env$.job_period,app_backup=app_backup,group_checks=env$group_checks)
 tmp <- tempfile(tmpdir=dirname(cfg$db));jsonlite::write_json(summary,tmp,auto_unbox=TRUE,pretty=TRUE,null='null')
 must(file.rename(tmp,file.path(dirname(cfg$db),'last-tariff-success.json')),'Could not record publication receipt')
 log_event(cfg,'tariff_job_published',snapshot_id=id,rows=nrow(env$samlet),run=run)
 cat('SUCCESS: tariff archived, app data validated and atomically published.\nSnapshot:',id,'\nRows:',nrow(env$samlet),'\n')
}
tryCatch(job(),error=function(e) {log_event(cfg,'tariff_job_failed',reason=conditionMessage(e));message(conditionMessage(e));quit(status=1)})
