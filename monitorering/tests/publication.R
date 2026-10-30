# Real Test 8 + synthetic failure/revision cases. Temporary data only.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
work<-tempfile('publication-test-');dir.create(work)
cfg<-list(db=file.path(work,'test.duckdb'),archive=file.path(work,'archive'),log=file.path(work,'events.jsonl'))
fix<-file.path(monitor_root,'tests','fixtures')
path<-file.path(work,'analysis.rds')
with_monitor_db(cfg,TRUE,function(con){
 init_db(con)
 import_consumption(con,cfg,file.path(fix,'monitorering_forbrug_N06_2025-01.csv'),file.path(fix,'monitorering_manifest_N06_2025-01.json'))
 old<-Sys.umask('0077');on.exit(Sys.umask(old),add=TRUE)
 publish_analysis(con,path)
 stopifnot(bitwAnd(as.integer(file.info(path)$mode),511L)==416L) # 0640 under strict umask
 x<-readRDS(path);validate_analysis_bundle(x)
 stopifnot(sum(x$tables$consumption$ddd,na.rm=TRUE)==25198980,
   sum(x$tables$consumption$regionalt_tilskud_kr,na.rm=TRUE)==80632838)
 m<-analysis_months(x,'N06','2025-01','2025-03')
 stopifnot(m$status[1]=='query_complete_population_unverified',m$status[2]=='missing',
   is.na(m$ddd[2]),m$missing_subsidy[1]>0,m$captures[1]==1)
 # Partial scope and empty scoped responses must not become full national coverage.
 stopifnot(analysis_months(x,'N','2025-01','2025-01')$status=='partial_subscopes')
 before<-sha(path)
 error<-try(publish_analysis(con,path,before_replace=function(p)stop('synthetic publication failure')),silent=TRUE)
 stopifnot(inherits(error,'try-error'),identical(before,sha(path)))
 # A live writer is held while a separate process reads the published RDS.
 reader<-file.path(work,'reader.R')
 writeLines(c('args<-commandArgs(TRUE)','x<-readRDS(args[1])','stopifnot(sum(x$tables$consumption$ddd,na.rm=TRUE)==25198980)'),reader)
 stopifnot(system2(file.path(R.home('bin'),'Rscript'),c(shQuote(reader),shQuote(path)))==0)
 publish_analysis(con,path)
 stopifnot(file.exists(paste0(path,'.previous')),
   identical(before,sha(paste0(path,'.previous'))))
})
# Pure forecast cases (synthetic numbers, explicitly labelled).
m<-data.frame(month=sprintf('2026-%02d',1:3),status='query_complete_population_unverified',
 actual_subsidy_kr=c(100,200,300),missing_subsidy=0L)
f<-subsidy_forecast(m,2026,as.Date('2026-04-03'))
stopifnot(f$available,f$estimate==2400)
m$missing_subsidy[2]<-1L
stopifnot(!subsidy_forecast(m,2026,as.Date('2026-04-03'))$available)
m$missing_subsidy[2]<-0L;m$status[2]<-'missing'
stopifnot(!subsidy_forecast(m,2026,as.Date('2026-04-03'))$available)
stopifnot(!subsidy_forecast(m[1,,drop=FALSE],2026,as.Date('2026-04-03'))$available)
# Synthetic product scenario: three real month slots; a vanished product is excluded.
pm<-data.frame(atc5='N06BA04',varenummer=c('000001','000001','000001','000002'),
 month=as.Date(c('2026-01-01','2026-02-01','2026-03-01','2026-01-01')),ddd=c(10,20,30,5),
 tsp_per_ddd=2,linkage_status='review_ddd_definition_and_price_date')
m$status<-'query_complete_population_unverified'
pr<-price_projection(pm,m,2026,as.Date('2026-04-03'))
stopifnot(pr$available,pr$products$modelled_future_basis_kr[pr$products$varenummer=='000001']==360,
 is.na(pr$products$modelled_future_basis_kr[pr$products$varenummer=='000002']))
# App startup and reactives with real publication; no live DB access by Shiny.
Sys.setenv(MONITOR_PUBLICATION=path)
app_path<-normalizePath(file.path(monitor_root,'..','monitorering-app'))
app<-shiny::shinyAppDir(app_path)
shiny::testServer(app,{
 session$setInputs(prefix='N06',from='2025-01',to='2025-02',product='',level='7')
 stopifnot(monthly()$ddd[1]==25198980,is.na(monthly()$ddd[2]),!is.null(cache()))
 stopifnot(grepl('N06',output$scope),grepl('Fremskrivning|Utilstrækkeligt',output$forecast))
 # Bad replacement must retain the previous object and surface a refresh failure.
 saveRDS(list(version=999),path);session$elapse(30001)
 stopifnot(monthly()$ddd[1]==25198980,nzchar(problem()))
})
cat('PASS: atomic publication, strict umask, retained previous file, concurrent reader, coverage, forecast guards and Shiny startup/reactives.\n')
# Optional post-tariff publication uses the same writer lock and keeps last good file.
source(file.path(monitor_root,'R','tariff_job.R'))
stopifnot(is.null(republish_tariff_analysis(cfg,'')))
republish_tariff_analysis(cfg,path)
before<-sha(path)
error<-try(republish_tariff_analysis(cfg,path,before_replace=function(p)stop('synthetic failure')),silent=TRUE)
stopifnot(inherits(error,'try-error'),identical(before,sha(path)))
contender<-file.path(work,'contender.R')
writeLines(c(paste0('source(',deparse(file.path(monitor_root,'R','core.R')),')'),
 paste0('source(',deparse(file.path(monitor_root,'R','tariff_job.R')),')'),
 'a<-commandArgs(TRUE)',
 'cfg<-list(db=a[1],archive=a[2],log=a[3])',
 'error<-try(republish_tariff_analysis(cfg,a[4]),silent=TRUE)',
 'stopifnot(inherits(error,"try-error"))'),contender)
with_monitor_db(cfg,TRUE,function(con) {
  status<-system2(file.path(R.home('bin'),'Rscript'),c(shQuote(contender),shQuote(cfg$db),shQuote(cfg$archive),shQuote(cfg$log),shQuote(path)))
  stopifnot(status==0,identical(before,sha(path)))
})
cat('PASS: opt-in post-tariff publication, writer contention and retained last good analysis.\n')
