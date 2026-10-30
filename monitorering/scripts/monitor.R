#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
cfg <- monitor_config()
usage <- function() stop(paste('Usage: monitor.R init | import-consumption CSV MANIFEST | import-tariff FILE METADATA |',
 'import-inbox DIR | validate | coverage PREFIX FROM TO | backup DIR | restore BACKUP_DIR | attest EXTRACT EVIDENCE | compare-tariff CURRENT_ID PREVIOUS_ID | export VIEW CSV'),call.=FALSE)
if(!length(args)) usage()
cmd <- args[1]; a <- args[-1]
expected <- c(init=0,'import-consumption'=2,'import-tariff'=2,'import-inbox'=1,validate=0,coverage=3,backup=1,restore=1,attest=2,'compare-tariff'=2,export=2)
if(!cmd %in% names(expected) || length(a)!=expected[[cmd]]) usage()
tryCatch({
 if(cmd=='backup') {cat(backup_monitor(cfg,a[1]),'\n'); quit(status=0)}
 if(cmd=='restore') {restore_monitor(cfg,a[1]); cat('Restore verified\n'); quit(status=0)}
 write <- cmd %in% c('init','import-consumption','import-tariff','import-inbox','attest')
 with_monitor_db(cfg,write,function(con) {
  if(cmd=='init') {init_db(con); cat('Database initialized\n'); return(invisible(NULL))}
  must(DBI::dbExistsTable(con,'schema_version'),'Run init first')
  switch(cmd,
   'import-consumption'=cat(import_consumption(con,cfg,a[1],a[2]),'\n'),
   'import-tariff'=cat(import_tariff(con,cfg,a[1],a[2]),'\n'),
   'import-inbox'=print(import_inbox(con,cfg,a[1])),
   validate=print(validate_db(con,cfg)),
   'compare-tariff'=print(compare_tariffs(con,a[1],a[2])),
   coverage=print(coverage_report(con,a[1],a[2],a[3]),row.names=FALSE),
   attest=attest_coverage(con,a[1],a[2]),
   export={
    allowed <- c('v_consumption','v_consumption_long','v_consumption_atc','v_consumption_price_month',
                  'v_consumption_current_price','v_tariff_latest','v_tariff_changes')
    must(a[1] %in% allowed,'Unknown export view')
    must(!file.exists(a[2]),'Export target already exists')
    DBI::dbExecute(con,paste('COPY (SELECT * FROM',DBI::dbQuoteIdentifier(con,a[1]),') TO',
      DBI::dbQuoteString(con,normalizePath(a[2],mustWork=FALSE)),"(HEADER,DELIMITER ';')"))
   })
 })
},error=function(e) {log_event(cfg,'job_failed',command=cmd,reason=conditionMessage(e)); message(conditionMessage(e)); quit(status=1)})
