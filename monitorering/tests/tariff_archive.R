# Rscript monitorering/tests/tariff_archive.R
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
work <- tempfile('tariff-archive-');dir.create(work)
cfg <- list(db=file.path(work,'old.duckdb'),archive=file.path(work,'archive'),log=file.path(work,'events.jsonl'))
f <- file.path(work,'prices.csv');meta <- file.path(work,'meta.json')
writeLines(c('varenummer;atc;esp;tsp;ddd_per_pack;ddd_provenance','019307;N06AA04;20;10;10;source'),f)
m <- list(schema_version=1,source='synthetic test',observed_at='2025-01-01T00:00:00Z',nominal_from='2025-01-01',nominal_to='2025-01-31',
 validity_kind='observed_only',complete=TRUE,completeness_evidence='Synthetic complete dataset',ddd_evidence='Synthetic source DDD')
jsonlite::write_json(m,meta,auto_unbox=TRUE,pretty=TRUE)
with_monitor_db(cfg,TRUE,function(con) {
 # Reconstruct schema v1 with required nominal dates, then populate BOTH histories.
 sql <- paste(readLines(file.path(monitor_root,'sql','001_schema.sql')),collapse='\n')
 sql <- sub('VALUES (2)','VALUES (1)',sql,fixed=TRUE)
 sql <- sub('nominal_from DATE,','nominal_from DATE NOT NULL,',sql,fixed=TRUE)
 sql <- sub('nominal_to DATE CHECK','nominal_to DATE NOT NULL CHECK',sql,fixed=TRUE)
 sql <- gsub('--[^\n]*','',sql)
 for(s in strsplit(sql,';',fixed=TRUE)[[1]]) if(nzchar(trimws(s))) DBI::dbExecute(con,s)
 views <- paste(readLines(file.path(monitor_root,'sql','002_views.sql')),collapse='\n')
 views <- gsub('--[^\n]*','',views)
 for(s in strsplit(views,';',fixed=TRUE)[[1]]) if(nzchar(trimws(s))) DBI::dbExecute(con,s)
 fix <- file.path(monitor_root,'tests','fixtures')
 id <- import_consumption(con,cfg,file.path(fix,'monitorering_forbrug_N06_2025-01.csv'),file.path(fix,'monitorering_manifest_N06_2025-01.json'))
 priceid <- import_tariff(con,cfg,f,meta)
 before <- DBI::dbGetQuery(con,'SELECT * FROM tariff_snapshot')
 init_db(con);init_db(con)
 stopifnot(DBI::dbGetQuery(con,'SELECT version FROM schema_version')$version==2)
 stopifnot(identical(before,DBI::dbGetQuery(con,'SELECT * FROM tariff_snapshot')))
 stopifnot(DBI::dbGetQuery(con,'SELECT count(*) n FROM consumption_observation')$n==1143)
 stopifnot(DBI::dbGetQuery(con,'SELECT count(*) n FROM tariff_product')$n==1)
 # Archive unknown nominal dates, with no permission to use it as a current price.
 m$nominal_from <- NULL;m$nominal_to <- NULL;m$complete <- FALSE
 m$unknown_period_reason <- 'Synthetic legacy file without source period'
 m$observed_at <- '2025-02-01T00:00:00Z'
 jsonlite::write_json(m,meta,auto_unbox=TRUE,pretty=TRUE,null='null')
 archiveid <- import_tariff(con,cfg,f,meta)
 stopifnot(identical(archiveid,import_tariff(con,cfg,f,meta)))
 dates <- DBI::dbGetQuery(con,'SELECT nominal_from,nominal_to FROM tariff_snapshot WHERE snapshot_id=?',params=list(archiveid))
 stopifnot(all(is.na(dates)))
 stopifnot(all(DBI::dbGetQuery(con,'SELECT snapshot_id FROM v_tariff_latest')$snapshot_id==priceid))
 stopifnot(DBI::dbGetQuery(con,'SELECT count(*) n FROM v_tariff_daily')$n==0)
 print(validate_db(con,cfg))
})
cat('PASS: populated v1 migration preserves consumption and tariff history; unknown-period archives are idempotent and excluded from valuations.\n')
