# Input contract supports raw Taksten_*.xlsx, clean_data2.rds, or normalized CSV.
# Preserve every original column in raw_json; do not infer DDD from rounded ratios.
read_tariff <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if(ext=='rds') return(as.data.frame(readRDS(path),stringsAsFactors=FALSE))
  if(ext %in% c('xlsx','xls')) {
    must(requireNamespace('readxl',quietly=TRUE),'Install readxl for Excel tariff inputs')
    return(as.data.frame(readxl::read_excel(path,col_types='text'),stringsAsFactors=FALSE))
  }
  must(ext=='csv','Tariff must be RDS, Excel or semicolon CSV')
  read.table(path,header=TRUE,sep=';',quote='"',comment.char='',colClasses='character',
             check.names=FALSE,na.strings='',fileEncoding='UTF-8-BOM',fill=FALSE)
}
number_tariff <- function(x) {
  if(is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x)); x[x %in% c('','-','NA','NaN')] <- NA_character_
  # Danish decimal commas, optionally with thousand dots. Bare dots are decimals.
  both <- !is.na(x) & grepl(',',x) & grepl('.',x,fixed=TRUE)
  x[both] <- gsub('.','',x[both],fixed=TRUE)
  x <- gsub(',','.',x,fixed=TRUE)
  suppressWarnings(as.numeric(x))
}
normalize_tariff <- function(d,m) {
  must(is.data.frame(d) && nrow(d)>0,'Empty tariff input')
  get <- function(...) {
    keys <- c(...); found <- keys[keys %in% names(d)]
    if(!length(found)) return(rep(NA_character_,nrow(d)))
    # Conflicting aliases are not silently selected.
    if(length(found)>1) for(k in found[-1]) {
      a <- as.character(d[[found[1]]]); b <- as.character(d[[k]])
      must(all(is.na(a) | is.na(b) | a==b),'Conflicting tariff column aliases')
    }
    out <- as.character(d[[found[1]]])
    for(k in found[-1]) {
      other <- as.character(d[[k]])
      missing <- is.na(out) | out==''
      out[missing] <- other[missing]
    }
    out
  }
  units <- function(value,unit) {
    use <- !is.na(value) & !is.na(unit) & nzchar(unit) & !grepl('[[:alpha:]%µμ]',value)
    value[use] <- paste(value[use],unit[use]); value
  }
  v <- get('varenummer','Varenummer','Varenr.')
  must(all(!is.na(v) & grepl('^[0-9]{1,6}$',v)),'Invalid tariff varenummer')
  v <- sprintf('%06d',as.integer(v))
  is_clean <- 'Ekspeditionspris (kr.)' %in% names(d) && !'ddd_provenance' %in% names(d)
  provenance <- get('ddd_provenance')
  provenance[is.na(provenance)] <- if(is_clean) 'unverified_ratio' else 'missing'
  must(all(provenance %in% c('source','verified','unverified_ratio','missing')),'Unknown DDD provenance')
  ddd <- number_tariff(get('ddd_per_pack','DDD'))
  esp <- number_tariff(get('esp','ESP (kr.)','Ekspeditionspris (kr.)'))
  tsp <- number_tariff(get('tsp','TSP (kr.)','Tilskudspris (kr.)'))
  esp_source <- number_tariff(get('source_esp_per_ddd','Pris pr. DDD (kr.)','Ekspeditionspris pr. DDD (kr.)'))
  tsp_source <- number_tariff(get('source_tsp_per_ddd','Tilskudspris pr. DDD (kr.)'))
  valid_num <- function(x) !is.na(x) & is.finite(x) & x>=0 & x<1e14
  trusted <- provenance %in% c('source','verified') & !is.na(ddd) & is.finite(ddd) & ddd>0 & ddd<1e12
  # Provenance assertions require a written source/validation record.
  if(any(provenance %in% c('source','verified'))) scalar(m$ddd_evidence,'DDD provenance evidence')
  flags <- vapply(seq_len(nrow(d)),function(i) paste(c(
    if(!trusted[i]) 'ddd_unverified_or_missing',
    if(!valid_num(esp)[i]) 'esp_missing_or_invalid',
    if(!valid_num(tsp)[i]) 'tsp_missing_or_invalid'
  ),collapse=';'),character(1))
  ddd[!trusted] <- NA_real_
  esp[!valid_num(esp)] <- NA_real_; tsp[!valid_num(tsp)] <- NA_real_
  esp_source[!valid_num(esp_source)] <- NA_real_; tsp_source[!valid_num(tsp_source)] <- NA_real_
  expddd <- ifelse(trusted,esp/ddd,esp_source)
  # clean_data2's subsidy/DDD is derived from an unverified ratio, so never use as source.
  tspddd <- ifelse(trusted,tsp/ddd,NA_real_)
  expddd[!is.finite(expddd)] <- NA_real_; tspddd[!is.finite(tspddd)] <- NA_real_
  atc <- get('atc','ATC','ATC-kode'); atc[atc==''] <- NA_character_
  out <- data.frame(varenummer=v,atc=atc,name=get('name','Lægemiddel'),strength=units(get('strength','Styrke'),get('Enhed, Styrke')),
    form=get('form','Form'),pack=units(get('pack','Pakning','Pakningsstr.'),get('Enhed, Pakningsstr.','Enhed, Pakning','Pakningsenhed')),
    marketing_status=get('marketing_status','Markedsføring'),delivery_status=get('delivery_status','Kan_leveres','Kan leveres (Leveringssvigt)'),
    withdrawal_date=get('withdrawal_date','Udgået dato'),
    substitution_group=get('substitution_group','Tilskudssubstitutionsgruppe','Substitutionsgrp.'),
    legacy_substitution_group=get('legacy_substitution_group','Overordnet_substitutionsgruppe'),
    esp=esp,tsp=tsp,ddd_per_pack=ddd,ddd_provenance=provenance,esp_per_ddd=expddd,tsp_per_ddd=tspddd,
    source_esp_per_ddd=esp_source,source_tsp_per_ddd=tsp_source,quality_flags=flags,stringsAsFactors=FALSE)
  out$raw_json <- vapply(seq_len(nrow(d)),function(i) json(as.list(d[i,,drop=FALSE])),character(1))
  # Compare every raw property too (pack units, strengths, availability etc.).
  out$row_hash <- vapply(out$raw_json,digest::digest,character(1),algo='sha256',serialize=FALSE)
  if(anyDuplicated(out$varenummer)) {
    for(k in unique(out$varenummer[duplicated(out$varenummer)]))
      must(length(unique(out$row_hash[out$varenummer==k]))==1L,paste('Conflicting tariff duplicates:',k))
    out <- out[!duplicated(out$varenummer),,drop=FALSE]
  }
  out
}
import_tariff <- function(con,cfg,path,metadata) {
  m <- read_meta(metadata)
  must(identical(as.integer(m$schema_version),1L),'Unsupported tariff metadata schema')
  scalar(m$source,'source'); iso_time(m$observed_at)
  for(k in c('nominal_from','nominal_to')) {
    must(is.character(m[[k]]) && length(m[[k]])==1L && grepl('^[0-9]{4}-[0-9]{2}-[0-9]{2}$',m[[k]]) && !is.na(as.Date(m[[k]])),paste('Missing/invalid',k))
  }
  must(as.Date(m$nominal_to)>=as.Date(m$nominal_from),'Reversed nominal tariff period')
  must(is.logical(m$complete) && length(m$complete)==1 && !is.na(m$complete),'Explicit complete flag required')
  if(isTRUE(m$complete)) scalar(m$completeness_evidence,'tariff completeness evidence')
  must(m$validity_kind %in% c('source_interval','observed_only'),'Explicit tariff validity kind required')
  if(m$validity_kind=='source_interval') {
    scalar(m$validity_evidence,'effective price interval evidence')
    a <- as.Date(scalar(m$effective_from,'effective_from')); b <- as.Date(scalar(m$effective_to,'effective_to'))
    must(!is.na(a) && !is.na(b) && b>=a && b-a<=366,'Invalid effective interval')
    must(a>=as.Date(m$nominal_from) && b<=as.Date(m$nominal_to),'Effective interval outside nominal period')
  } else {
    must(is.null(m$effective_from) && is.null(m$effective_to),'Observed-only snapshot must not assert effective dates')
    m$effective_from <- NA_character_; m$effective_to <- NA_character_
  }
  h <- sha(path)
  if(!is.null(m$raw_sha256)) must(identical(h,m$raw_sha256),'Tariff raw checksum mismatch')
  id <- digest::digest(paste(h,sha(metadata),sep=':'),algo='sha256',serialize=FALSE)
  if(DBI::dbGetQuery(con,'SELECT count(*) n FROM tariff_snapshot WHERE snapshot_id=?',params=list(id))$n>0) return(id)
  must(DBI::dbGetQuery(con,'SELECT count(*) n FROM tariff_snapshot WHERE observed_at=?::TIMESTAMPTZ',params=list(m$observed_at))$n==0,
       'Conflicting tariff snapshots at the same source timestamp')
  raw <- read_tariff(path)
  if(!is.null(m$row_count)) must(nrow(raw)==m$row_count,'Tariff raw row count mismatch')
  d <- normalize_tariff(raw,m)
  ar <- archive_file(cfg,path,h); am <- archive_file(cfg,metadata)
  DBI::dbWithTransaction(con,{
    DBI::dbExecute(con,paste('INSERT INTO tariff_snapshot',
      '(snapshot_id,source,observed_at,nominal_from,nominal_to,effective_from,effective_to,validity_kind,complete,raw_sha256,metadata_json,archive_raw,archive_metadata)',
      'VALUES (?,?,?::TIMESTAMPTZ,?::DATE,?::DATE,?::DATE,?::DATE,?,?,?,?,?,?)'),
      params=list(id,m$source,m$observed_at,m$nominal_from,m$nominal_to,m$effective_from,m$effective_to,
                  m$validity_kind,m$complete,h,json(m),ar,am))
    d$snapshot_id <- id
    DBI::dbAppendTable(con,'tariff_product',d)
  })
  log_event(cfg,'tariff_import',snapshot_id=id,rows=nrow(d),validity=m$validity_kind,complete=m$complete)
  id
}
compare_tariffs <- function(con,current_id,previous_id) {
  ids <- DBI::dbGetQuery(con,'SELECT snapshot_id FROM tariff_snapshot WHERE snapshot_id IN (?,?)',params=list(current_id,previous_id))$snapshot_id
  must(current_id %in% ids && previous_id %in% ids,'Unknown tariff snapshot ID')
  DBI::dbGetQuery(con,paste(
    'WITH a AS (SELECT * FROM tariff_product WHERE snapshot_id=?),',
    'b AS (SELECT * FROM tariff_product WHERE snapshot_id=?)',
    'SELECT coalesce(a.varenummer,b.varenummer) AS varenummer,a.atc,b.atc AS previous_atc,',
    'a.esp,b.esp AS previous_esp,a.tsp,b.tsp AS previous_tsp,a.ddd_per_pack,b.ddd_per_pack AS previous_ddd_per_pack,',
    "CASE WHEN b.varenummer IS NULL THEN 'added' WHEN a.varenummer IS NULL THEN 'removed'",
    "WHEN a.row_hash<>b.row_hash THEN 'changed' ELSE 'unchanged' END AS change",
    'FROM a FULL OUTER JOIN b USING(varenummer) ORDER BY varenummer'),params=list(current_id,previous_id))
}
