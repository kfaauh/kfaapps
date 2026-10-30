#!/usr/bin/env Rscript
# Operator-only. No public HTTP upload endpoint. Failed inbox batch never publishes.
args<-commandArgs(trailingOnly=TRUE)
script<-sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
if(length(args)!=2L) stop('Usage: import_and_publish.R INBOX PUBLICATION_RDS')
cfg<-monitor_config()
with_monitor_db(cfg,TRUE,function(con){
 import_inbox(con,cfg,args[1]);validate_db(con,cfg)
 cat('Published:',publish_analysis(con,args[2]),'\n')
})
