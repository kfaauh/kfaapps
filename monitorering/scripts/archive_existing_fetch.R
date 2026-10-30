#!/usr/bin/env Rscript
# Opt-in hook runner: executes the existing fetch exactly once, then archives the
# raw Taksten workbook. No changes to fetch_data2.R or to either Shiny app.
a <- commandArgs(trailingOnly=TRUE)
if(length(a)!=3) stop('Usage: archive_existing_fetch.R OUT_DIR CATALOG_XLSX TARIFF_METADATA_JSON')
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
meta <- read_meta(a[3])
# This runner requires fresh, explicit credentials. Ignore assignments embedded
# in the old script by replacing ONLY those parsed top-level assignments.
email <- Sys.getenv('MEDPRIS_EMAIL'); password <- Sys.getenv('MEDPRIS_PASSWORD')
must(nzchar(email)&&nzchar(password),'Set MEDPRIS_EMAIL and MEDPRIS_PASSWORD explicitly')
fetch <- file.path(monitor_root,'..','data-scripts','fetch_data2.R')
exprs <- parse(fetch,keep.source=FALSE)
find_assignment <- function(name) which(vapply(exprs,function(e)
  is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name(name)),logical(1)))
for(n in c('email','pw')) must(length(find_assignment(n))==1L,paste('Upstream credential layout changed:',n))
exprs[[find_assignment('email')]] <- quote(email <- Sys.getenv('MEDPRIS_EMAIL'))
exprs[[find_assignment('pw')]] <- quote(pw <- Sys.getenv('MEDPRIS_PASSWORD'))
# Arguments and state are isolated from the caller. The source script is not edited.
env <- new.env(parent=globalenv())
fetchargs <- normalizePath(a[1:2],mustWork=FALSE)
env$commandArgs <- function(trailingOnly=FALSE) if(trailingOnly) fetchargs else base::commandArgs()
start <- Sys.time()
for(e in exprs) eval(e,env)
must(exists('samlet',env,inherits=FALSE)&&exists('xlsx_file',env,inherits=FALSE),'Existing fetch did not finish')
# The legacy scraper may skip failed groups. Never certify them as complete.
missing <- setdiff(env$udleveringsgrupper,names(env$alle_data))
if(length(missing)) {
 meta$complete <- FALSE
 meta$missing_fetch_groups <- missing
}
# Capture source price-period banner after the same authorized fetch (no extra HTTP request).
page <- env$current_page
banner <- rvest::html_text2(page)
pattern <- 'Prisperiode:[[:space:]]*([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})[[:space:]]*-[[:space:]]*([0-9]{2}\\.[0-9]{2}\\.[0-9]{4})'
match <- regmatches(banner,regexec(pattern,banner))[[1]]
must(length(match)==3L,'Source price period not found; raw fetch remains available, import withheld')
from <- format(as.Date(match[2],'%d.%m.%Y'),'%Y-%m-%d'); to <- format(as.Date(match[3],'%d.%m.%Y'),'%Y-%m-%d')
must(identical(meta$nominal_from,from)&&identical(meta$nominal_to,to),'Configured nominal period differs from observed source banner')
# Capture time, not historic price validity, is observed here. Avoid midnight races.
localday <- format(start,'%Y-%m-%d',tz='Europe/Copenhagen')
must(identical(localday,format(Sys.time(),'%Y-%m-%d',tz='Europe/Copenhagen')),'Fetch crossed Danish midnight; repeat or review before import')
meta$observed_at <- format(Sys.time(),'%Y-%m-%dT%H:%M:%OS3Z',tz='UTC')
meta$validity_kind <- 'observed_only';meta$effective_from <- NULL;meta$effective_to <- NULL
meta$source_period_banner <- match[1];meta$raw_sha256 <- sha(env$xlsx_file)
meta$row_count <- nrow(env$samlet)
meta$fetch_script_sha256 <- sha(fetch)
meta$fetch_groups <- names(env$alle_data)
tmp <- tempfile(fileext='.json')
jsonlite::write_json(meta,tmp,auto_unbox=TRUE,pretty=TRUE,null='null')
cfg <- monitor_config()
with_monitor_db(cfg,TRUE,function(con) {
 init_db(con);cat(import_tariff(con,cfg,env$xlsx_file,tmp),'\n')
})
unlink(tmp)
