# The live DuckDB is never opened by Shiny. Publish one self-contained RDS,
# atomically replaced on the same filesystem; readers retain the previous object.
publication_tables <- c(
 consumption='v_consumption', historical_prices='v_consumption_price_month',
 current_prices='v_consumption_current_price', tariff_changes='v_tariff_changes')
publish_analysis <- function(con,destination,before_replace=function(path) NULL) {
  must(dir.exists(dirname(destination)),'Create publication directory with operator/Shiny group permissions first')
  bundle <- DBI::dbWithTransaction(con,{
    tables <- lapply(publication_tables,function(view) DBI::dbGetQuery(con,paste('SELECT * FROM',view)))
    tables$extracts <- DBI::dbGetQuery(con,paste('SELECT e.extract_id,e.atc_prefix,e.month,e.extracted_at,e.imported_at,e.row_count,',
      'a.extract_id IS NOT NULL AS population_verified FROM consumption_extract e LEFT JOIN coverage_attestation a USING(extract_id)'))
    tables$tariffs <- DBI::dbGetQuery(con,paste('SELECT snapshot_id,observed_at,imported_at,nominal_from,nominal_to,',
      'validity_kind,complete,effective_from,effective_to FROM tariff_snapshot'))
    tables$tariff_products <- DBI::dbGetQuery(con,paste('SELECT snapshot_id,varenummer,atc,name,strength,form,pack,esp,tsp,',
      'ddd_provenance,tsp_per_ddd,quality_flags FROM tariff_product'))
    list(version=1L,published_at=format(Sys.time(),'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'),tables=tables)
  })
  validate_analysis_bundle(bundle)
  tmp <- tempfile('.analysis-',tmpdir=dirname(destination));on.exit(unlink(tmp),add=TRUE)
  saveRDS(bundle,tmp,compress=TRUE)
  validate_analysis_bundle(readRDS(tmp))
  # chmod must override the service's restrictive umask (regression from tariff job).
  must(Sys.chmod(tmp,'0640',use_umask=FALSE),'Cannot set Shiny-readable publication permissions')
  # Caller holds writer lock. Retain the last good publication for operator rollback.
  if(file.exists(destination)) {
    backup <- tempfile('.previous-',tmpdir=dirname(destination));on.exit(unlink(backup),add=TRUE)
    must(file.copy(destination,backup),'Cannot retain previous publication')
    must(Sys.chmod(backup,'0640',use_umask=FALSE),'Cannot set backup permissions')
    must(file.rename(backup,paste0(destination,'.previous')),'Cannot save previous publication')
  }
  before_replace(tmp) # Test hook: failure here leaves the active file untouched.
  must(file.rename(tmp,destination),'Atomic analysis publication failed')
  invisible(bundle$published_at)
}
# Shared pure read/analysis functions are deployed with the app, without DB packages.
source(file.path(monitor_root,'..','monitorering-app','analysis.R'))
