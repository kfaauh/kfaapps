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
# Synthetic UI calendar: repeated fixture volumes are NOT live 2026 observations.
synthetic<-readRDS(path)
for(name in c('consumption','historical_prices','current_prices')) {
  original<-synthetic$tables[[name]]
  synthetic$tables[[name]]<-do.call(rbind,lapply(1:7,function(i){
    d<-original;d$month<-as.Date(sprintf('2026-%02d-01',i));d
  }))
}
original<-synthetic$tables$extracts[1,,drop=FALSE]
synthetic$tables$extracts<-do.call(rbind,lapply(1:9,function(i){
  d<-original;d$month<-as.Date(sprintf('2026-%02d-01',i));if(i>7)d$row_count<-0L;d
}))
validate_analysis_bundle(synthetic);saveRDS(synthetic,path)
# Fix the app's clock for stable calendar assertions in future CI years.
Sys.Date<-function()as.Date('2026-10-30')
shiny::testServer(shiny::shinyAppDir(app_path),{
 session$setInputs(prefix='N06',from='2026-01',to='2026-09',product='',level='7')
 stopifnot(grepl('2026-07',output$latest_observation),
  grepl('Scenarie august–december 2026',output$observed_estimate),
  grepl('Manglende produktmålinger',output$observed_estimate),
  grepl('Utilstrækkeligt',output$forecast),
  grepl('Scenarie august–december 2026',output$future_heading),
  !is.null(output$estimate_series),!is.null(output$year_series),!is.null(output$year_comparison))
})
rm(Sys.Date)
cat('PASS: synthetic July observation boundary, separate estimate text, strict forecast guard and comparison/scenario plots in Shiny.\n')

# A populated v2 database keeps its national capture, raw hashes and attestation.
# The regional and national captures deliberately have the same source time.
regional_cfg<-cfg;regional_cfg$db<-file.path(work,'migration.duckdb')
with_monitor_db(regional_cfg,TRUE,function(con) {
 csv<-file.path(fix,'monitorering_forbrug_N06_2025-01.csv')
 manifest<-file.path(fix,'monitorering_manifest_N06_2025-01.json')
 execute_monitor_sql(con,'001_schema.sql')
 national_id<-import_consumption(con,regional_cfg,csv,manifest)
 evidence<-file.path(work,'evidence.txt')
 writeLines('Synthetic evidence used only to test preservation of the national attestation.',evidence)
 attest_coverage(con,national_id,evidence)
 before<-DBI::dbReadTable(con,'consumption_extract')
 attestation<-DBI::dbReadTable(con,'coverage_attestation')
 init_db(con);init_db(con)
 stopifnot(database_version(con)==3L,identical(before,DBI::dbReadTable(con,'consumption_extract')),
   identical(attestation,DBI::dbReadTable(con,'coverage_attestation')),
   coverage_report(con,'N06','2025-01','2025-01')$status=='population_verified',
   coverage_report(con,'N06','2025-01','2025-01','MJ')$status=='missing')
 # V3 observations are synthetic. One region lacks both measures in February.
 mf<-file.path(work,'regional.json');cf<-file.path(work,'regional.csv')
 m<-read_meta(manifest);m$schema_version<-3;m$query_contract<-'compact-regional-v1'
 m$query_sha256<-paste(rep('a',64),collapse='');m$has_restart_token<-FALSE
 regional_id<-function(month,nregions=5L,multiplier=1) {
   m$period<-month
   d<-expand.grid(region=head(regional_codes,nregions),maaling=consumption_metrics,stringsAsFactors=FALSE)
   d$atc5<-'N06BA04';d$varenummer<-'019307';d$aar_maaned<-month
   d$vaerdi<-match(d$region,regional_codes)*multiplier
   d<-d[c('atc5','varenummer','aar_maaned','maaling','vaerdi','region')]
   write.table(d,cf,sep=';',quote=FALSE,row.names=FALSE)
   m$csv_sha256<-sha(cf);m$row_count<-nrow(d);m$atc5_count<-as.integer(nrow(d)>0);m$allow_empty<-nrow(d)==0
   m$metric_row_counts<-setNames(lapply(consumption_metrics,function(metric) sum(d$maaling==metric)),consumption_metrics)
   for(metric in consumption_metrics) m[[paste0('total_',metric)]]<-sum(d$vaerdi[d$maaling==metric])
   m$region_metrics<-setNames(lapply(regional_codes,function(region) {
     setNames(lapply(consumption_metrics,function(metric) {
       values<-d$vaerdi[d$maaling==metric & d$region==region]
       list(rows=length(values),total=sum(values))
     }),consumption_metrics)
   }),regional_codes)
   writeLines(json(m),mf)
   id<-import_consumption(con,regional_cfg,cf,mf)
   stopifnot(identical(id,import_consumption(con,regional_cfg,cf,mf)))
   bad<-m;bad$region_metrics$MJ$ddd$total<-999
   writeLines(json(bad),mf)
   stopifnot(grepl('Regional metric total mismatch',as.character(try(import_consumption(con,regional_cfg,cf,mf),silent=TRUE))))
   id
 }
 regional_id('2025-01')
 regional_id('2025-02',4L)
 regional_id('2025-03',5L,3L)
 stopifnot(DBI::dbGetQuery(con,"SELECT sum(ddd) total FROM v_consumption")$total==25198980,
   DBI::dbGetQuery(con,"SELECT sum(ddd) total FROM v_consumption_by_region WHERE month='2025-01-01' AND region<>'DK'")$total==15,
   coverage_report(con,'N06','2025-02','2025-02','NJ')$status=='query_complete_population_unverified')
 validate_db(con,regional_cfg)
 publish_analysis(con,path)
 z<-readRDS(path);validate_analysis_bundle(z)
 stopifnot(z$version==2L,analysis_months(z,'N06','2025-01','2025-01')$ddd==25198980)
 mj<-analysis_months(z,'N06','2025-01','2025-04',region='MJ')
 nj<-analysis_months(z,'N06','2025-01','2025-03',region='NJ')
 stopifnot(identical(mj$ddd,c(4,4,12,NA_real_)),mj$status[4]=='missing',
   is.na(nj$actual_subsidy_kr[2]),nj$status[2]=='query_complete_population_unverified',
   subsidy_forecast(mj,2025,as.Date('2025-04-03'))$estimate==80,
   !subsidy_forecast(nj,2025,as.Date('2025-04-03'))$available)
 legacy<-z;legacy$version<-1L
 for(name in c('consumption','historical_prices','current_prices','extracts')) {
   legacy$tables[[name]]<-analysis_region_rows(legacy$tables[[name]],'DK')
   legacy$tables[[name]]$region<-NULL
 }
 validate_analysis_bundle(legacy)
 stopifnot(analysis_months(legacy,'N06','2025-01','2025-01')$ddd==25198980,
   is.na(analysis_months(legacy,'N06','2025-01','2025-01',region='MJ')$ddd))
})
shiny::testServer(shiny::shinyAppDir(app_path),{
 session$setInputs(prefix='N06',from='2025-01',to='2025-03',product='',level='7')
 stopifnot(monthly()$ddd[1]==25198980)
 session$setInputs(region='MJ')
 stopifnot(monthly()$ddd[1]==4,grepl('Region Midtjylland',output$scope),
   all(subset_rows(bundle()$tables$current_prices)$region=='MJ'),
   all(subset_rows(bundle()$tables$historical_prices)$region=='MJ'))
 session$setInputs(region='NJ')
 stopifnot(is.na(monthly()$ddd[2]),all(is.na(comparison()$previous_ddd)))
})
cat('PASS: v2 migration, national continuity, v3 regional import/reconciliation, publication, Shiny filtering and regional forecast gaps.\n')
