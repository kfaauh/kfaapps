# Rscript monitorering/tests/integration.R -- uses temporary database only.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
work <- tempfile('monitorering-tests-');dir.create(work)
cfg <- list(db=file.path(work,'test.duckdb'),archive=file.path(work,'archive'),log=file.path(work,'events.jsonl'))
fix <- file.path(monitor_root,'tests','fixtures')
csv <- file.path(fix,'monitorering_forbrug_N06_2025-01.csv'); manifest <- file.path(fix,'monitorering_manifest_N06_2025-01.json')
fails <- function(code,pattern=NULL) {
 error <- tryCatch({force(code);NULL},error=function(e)e)
 stopifnot(inherits(error,'error'))
 if(!is.null(pattern)) stopifnot(grepl(pattern,conditionMessage(error),ignore.case=TRUE))
}
q <- function(con,sql) DBI::dbGetQuery(con,sql)
write_pair <- function(d,m,name) {
 a <- file.path(work,paste0(name,'.csv'));b <- file.path(work,paste0(name,'.json'))
 write.table(d,a,sep=';',row.names=FALSE,quote=FALSE,na='')
 m$row_count <- nrow(d);m$atc5_count <- length(unique(d$atc5))
 m$total_ddd <- sum(as.numeric(d$vaerdi[d$maaling=='ddd']))
 m$total_regionalt_tilskud_kr <- sum(as.numeric(d$vaerdi[d$maaling=='regionalt_tilskud_kr']))
 jsonlite::write_json(m,b,auto_unbox=TRUE,pretty=TRUE);list(csv=a,manifest=b)
}
x <- validate_consumption(csv,manifest);stopifnot(nrow(x$data)==1143)
with_monitor_db(cfg,TRUE,function(con) {
 init_db(con);init_db(con)
 id <- import_consumption(con,cfg,csv,manifest)
 stopifnot(identical(id,import_consumption(con,cfg,csv,manifest)))
 stopifnot(q(con,'SELECT count(*) n FROM consumption_observation')$n==1143)
 stopifnot(q(con,'SELECT sum(ddd) n FROM v_consumption')$n==25198980)
 stopifnot(q(con,'SELECT sum(regionalt_tilskud_kr) n FROM v_consumption')$n==80632838)
 stopifnot(q(con,"SELECT sum(ddd) n FROM v_consumption WHERE atc5='N06BA04'")$n==3340703)
 stopifnot(q(con,"SELECT count(*) n FROM v_consumption WHERE varenummer='019307'")$n==1)
 stopifnot(q(con,'SELECT count(*) n FROM v_consumption WHERE ddd IS NULL OR regionalt_tilskud_kr IS NULL')$n>0)
 stopifnot(coverage_report(con,'N06','2025-01','2025-02')$status[2]=='missing')
 stopifnot(coverage_report(con,'N06BA04','2025-01','2025-01')$status[1]=='query_complete_population_unverified')
 bad <- x$manifest;bad$row_count <- 1
 f <- file.path(work,'bad.json');jsonlite::write_json(bad,f,auto_unbox=TRUE)
 fails(import_consumption(con,cfg,csv,f),'row count');stopifnot(q(con,'SELECT count(*) n FROM consumption_extract')$n==1)
 # Duplicate keys fail before transaction; no zero imputation.
 d <- rbind(x$data,x$data[1,]);pair <- write_pair(d,x$manifest,'duplicate')
 fails(import_consumption(con,cfg,pair$csv,pair$manifest),'Duplicate')
 # Latest source snapshot removes one metric and one full ATC, including overlap.
 d <- x$data[x$data$atc5=='N06BA04',];d <- d[-1,];m <- x$manifest;m$atc_prefix <- 'N06BA04';m$extracted_at <- '2026-10-31T10:00:00Z'
 d$vaerdi[1] <- as.character(as.numeric(d$vaerdi[1])+3)
 pair <- write_pair(d,m,'revision');id2 <- import_consumption(con,cfg,pair$csv,pair$manifest)
 stopifnot(q(con,"SELECT count(*) n FROM v_consumption_long WHERE atc5='N06BA04'")$n==nrow(d))
 stopifnot(q(con,"SELECT count(*) n FROM consumption_observation WHERE atc5='N06BA04'")$n>nrow(d))
 # Late older source must not roll back current records.
 m$extracted_at <- '2026-10-01T10:00:00Z';pair <- write_pair(d,m,'older');import_consumption(con,cfg,pair$csv,pair$manifest)
 stopifnot(all(q(con,"SELECT DISTINCT extract_id FROM v_consumption_long WHERE atc5='N06BA04'")$extract_id==id2))
 # Same source instant overlapping prefix is ambiguous and rejected.
 m$extracted_at <- '2026-10-31T10:00:00Z';d$vaerdi[1] <- '123';pair <- write_pair(d,m,'conflict')
 fails(import_consumption(con,cfg,pair$csv,pair$manifest),'Conflicting')
 # Broader latest snapshot tombstones a previously represented full ATC code.
 d <- x$data[x$data$atc5!='N06BA04',];m <- x$manifest;m$extracted_at <- '2026-11-01T10:00:00Z'
 pair <- write_pair(d,m,'broader');import_consumption(con,cfg,pair$csv,pair$manifest)
 stopifnot(q(con,"SELECT count(*) n FROM v_consumption_long WHERE atc5='N06BA04'")$n==0)
 # Restore genuine reference as a later source snapshot for tariff tests.
 m <- x$manifest;m$extracted_at <- '2026-11-02T10:00:00Z';pair <- write_pair(x$data,m,'reference-again');import_consumption(con,cfg,pair$csv,pair$manifest)
 # Transaction rollback on database constraint failure.
 n <- q(con,'SELECT count(*) n FROM consumption_extract')$n
 fails(DBI::dbWithTransaction(con,{
  DBI::dbExecute(con,"INSERT INTO coverage_attestation(extract_id,evidence) VALUES ('missing','invalid')")
 }))
 stopifnot(q(con,'SELECT count(*) n FROM consumption_extract')$n==n)
 # Monthly price: 15 days * 1 kr/DDD plus 16 days * 2 kr/DDD.
 price <- data.frame(varenummer=c('019307','999999'),atc=c('N06AA04','N06BA04'),name=c('A','B'),
  esp=c('20','10'),tsp=c('10','10'),ddd_per_pack=c('10','0'),ddd_provenance=c('source','missing'))
 pricefile <- file.path(work,'tariff.csv');metafile <- file.path(work,'tariff.json')
 meta <- list(schema_version=1,source='synthetic tariff test',observed_at='2025-01-01T12:00:00Z',
  nominal_from='2025-01-01',nominal_to='2025-01-31',effective_from='2025-01-01',effective_to='2025-01-15',
  validity_kind='source_interval',complete=TRUE,completeness_evidence='Synthetic complete test universe',
  validity_evidence='Synthetic known price validity',ddd_evidence='Synthetic known DDD per pack')
 saveprice <- function() {write.table(price,pricefile,sep=';',row.names=FALSE,quote=FALSE);jsonlite::write_json(meta,metafile,auto_unbox=TRUE)}
 saveprice();pid <- import_tariff(con,cfg,pricefile,metafile);stopifnot(identical(pid,import_tariff(con,cfg,pricefile,metafile)))
 stopifnot(is.na(q(con,"SELECT calendar_weighted_tsp_per_ddd p FROM v_consumption_price_month WHERE varenummer='019307'")$p))
 meta$observed_at <- '2025-01-16T12:00:00Z';meta$effective_from <- '2025-01-16';meta$effective_to <- '2025-01-31'
 price$tsp[1] <- '20';price <- price[1,];saveprice();pid2 <- import_tariff(con,cfg,pricefile,metafile)
 stopifnot(any(compare_tariffs(con,pid2,pid)$change=='removed'),any(compare_tariffs(con,pid,pid2)$change=='added'))
 p <- q(con,"SELECT * FROM v_consumption_price_month WHERE varenummer='019307'")
 stopifnot(p$compatible_tsp_days==31,abs(p$calendar_weighted_tsp_per_ddd-47/31)<1e-12,
           abs(p$modelled_subsidy_basis_kr-809*47/31)<1e-9,p$actual_regional_subsidy_kr==826)
 stopifnot(q(con,"SELECT count(*) n FROM v_tariff_changes WHERE change='removed' AND varenummer='999999'")$n==1)
 # Unverified DDD and zero denominator cannot fabricate subsidy per DDD.
 clean <- data.frame(Varenummer='019307',DDD='9.999',check.names=FALSE)
 clean[['Ekspeditionspris (kr.)']] <- '20';clean[['Ekspeditionspris pr. DDD (kr.)']] <- '2.00';clean[['Tilskudspris (kr.)']] <- '10'
 z <- normalize_tariff(clean,list());stopifnot(is.na(z$ddd_per_pack),is.na(z$tsp_per_ddd),z$ddd_provenance=='unverified_ratio')
 # Observed-only snapshot does not rewrite historical price intervals.
 meta$observed_at <- '2025-02-01T12:00:00Z';meta$validity_kind <- 'observed_only';meta$effective_from <- NULL;meta$effective_to <- NULL
 price$tsp[1] <- '90';saveprice();import_tariff(con,cfg,pricefile,metafile)
 stopifnot(abs(q(con,"SELECT calendar_weighted_tsp_per_ddd p FROM v_consumption_price_month WHERE varenummer='019307'")$p-47/31)<1e-12)
 stopifnot(q(con,"SELECT tsp_per_ddd p FROM v_tariff_latest WHERE varenummer='019307'")$p==9)
 # Conflicting duplicate products rejected without partial snapshot registration.
 before <- q(con,'SELECT count(*) n FROM tariff_snapshot')$n
 price <- rbind(price,price);price$tsp[2] <- '91';meta$observed_at <- '2025-02-02T12:00:00Z';saveprice()
 fails(import_tariff(con,cfg,pricefile,metafile),'duplicates');stopifnot(q(con,'SELECT count(*) n FROM tariff_snapshot')$n==before)
 print(validate_db(con,cfg))
})
backup <- backup_monitor(cfg,file.path(work,'backups'))
stopifnot(file.exists(file.path(backup,'monitorering.duckdb')),file.exists(file.path(backup,'checksums.json')))
restored <- list(db=file.path(work,'restored','test.duckdb'),archive=file.path(work,'restored','archive'),log=file.path(work,'restored','events.jsonl'))
restore_monitor(restored,backup)
with_monitor_db(restored,FALSE,function(con) stopifnot(nrow(validate_db(con,restored))==4L))
lock <- filelock::lock(paste0(cfg$db,'.writer.lock'),timeout=0)
# Lock exclusion is tested with a second R process in the CI shell workflow.
filelock::unlock(lock)
cat('PASS: Test 8 numerical import, idempotence, revisions, overlap/tombstones, rollback, tariff history, missing data, daily joins, backup.\n')
cat('Temporary test directory:',work,'\n')
