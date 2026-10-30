# Rscript monitorering/tests/tariff_job.R -- no external requests or app writes.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
source(file.path(monitor_root,'R','tariff_job.R'))
stopifnot(requireNamespace('rvest',quietly=TRUE))
page <- function(message,group='AP',extra='',period='26.10.2026 - 08.11.2026') xml2::read_html(paste0(
 '<html><body><select name="ctl00$CanvasHolder$SearchBox$UdleveringDropdown"><option selected value="',group,'">',group,'</option></select>',
 '<div id="CanvasHolder_SearchResultBox">',message,extra,'</div><p>Prisperiode: ',period,'</p></body></html>'))
fails <- function(code) stopifnot(inherits(tryCatch({force(code);NULL},error=function(e)e),'error'))
stopifnot(isTRUE(search_state(page('Din søgning gav ingen resultater'),'AP')$empty))
stopifnot(isTRUE(search_state(page('Der blev ikke fundet lægemidler'),'AP')$empty))
stopifnot(!search_state(page('Viser resultater','AP',extra='<table><tr><td>019307</td></tr></table>'),'AP')$empty)
fails(search_state(page('Ingen resultater','B'),'AP'))
fails(search_state(page('Ingen resultater','AP',extra='<table><tr><td>019307</td></tr></table>'),'AP'))
stopifnot(!search_state(page('Serverfejl. Prøv igen.'),'AP')$empty) # never infer zero from errors
fails(search_state(xml2::read_html('<input id="CanvasHolder_LoginBox_MailBox"/>'),'AP'))
stopifnot(identical(price_period(page('Ingen resultater')),list(from='2026-10-26',to='2026-11-08')))
fails(price_period(xml2::read_html('<p>Unavailable</p>')))
# Full live catalogue is used, including new groups, but disappearing known groups fail closed.
catalog <- xml2::read_html('<select name="ctl00$CanvasHolder$SearchBox$UdleveringDropdown"><option value="">Alle</option><option value="A">A</option><option value="B">B</option><option value="NEW">NEW</option></select>')
stopifnot(identical(live_groups(catalog,c('A','B')),c('A','B','NEW')));fails(live_groups(catalog,c('A','AP')))
# Reuse the unchanged legacy preparation loop with synthetic request/response boundaries.
work <- tempfile('tariff-job-tests-');dir.create(work)
raw <- data.frame('Varenr.'='019307','ESP (kr.)'='20','TSP (kr.)'='10',check.names=FALSE)
f <- file.path(work,'raw.csv');write.table(raw,f,sep=';',row.names=FALSE,quote=FALSE)
d <- as.data.frame(setNames(rep(list('x'),15),c('Kategori','Underkategori','ATCtekst','ATC','Indholdsstof','Styrke','Form','Pakning','Lægemiddel','Firma','Varenummer','Tilskudssubstitutionsgruppe','Overordnet_substitutionsgruppe','Kan_leveres','Udleveringsgruppe')))
d$Varenummer <- '019307'
for(n in c('Ekspeditionspris (kr.)','DDD','Ekspeditionspris pr. DDD (kr.)','Tilskudspris (kr.)','Tilskudspris pr. DDD (kr.)')) d[[n]] <- 10
 d[['Ekspeditionspris (kr.)']] <- 20
rds <- file.path(work,'new.rds');saveRDS(d,rds)
validate_app_tariff(f,rds,min_rows=1)
bad <- d;bad[['Tilskudspris (kr.)']] <- 1000;saveRDS(bad,rds);fails(validate_app_tariff(f,rds,min_rows=1))
saveRDS(d,rds)
app <- file.path(work,'app.rds');saveRDS('old',app)
restart <- file.path(work,'restart.txt');file.create(restart)
old_umask <- Sys.umask('0077')
publish_tariff(rds,app,file.path(work,'backups'),restart)
stopifnot(as.integer(file.info(app)$mode)==strtoi('640',8L))
stopifnot(identical(readRDS(app),d),length(list.files(file.path(work,'backups')))==1)
saveRDS('old-again',app)
fails(publish_tariff(rds,app,file.path(work,'backups'),restart,restart_fn=function(path) FALSE))
stopifnot(identical(readRDS(app),'old-again'),as.integer(file.info(app)$mode)==strtoi('640',8L))
Sys.umask(old_umask)
cat('PASS: explicit empty evidence, wrong-group/login/error rejection, source periods/catalogue, raw/app price checks, atomic publication and rollback.\n')
# Exercise strict_group itself with genuine httr2 response objects and no requests.
html_response <- function(doc) httr2::response(headers=list('content-type'='text/html'),body=charToRaw(as.character(doc)))
invoke <- function(search,export=NULL) {
 calls <- 0L
 scope <- new.env(parent=environment(strict_group))
 scope$source_form <- function(...) {calls <<- calls+1L;if(calls==1L) search else export}
 fn <- strict_group;environment(fn) <- scope
 env <- new.env();env$current_page <- page('Initial')
 env$parse_xls_response <- function(resp,group) {
   tmp <- tempfile(fileext='.xlsx');on.exit(unlink(tmp),add=TRUE)
   writeBin(httr2::resp_body_raw(resp),tmp);as.data.frame(readxl::read_excel(tmp,col_types='text'))
 }
 result <- tryCatch(fn(env,'AP',work,list(from='2026-10-26',to='2026-11-08')),error=function(e)e)
 list(calls=calls,result=result)
}
zero <- invoke(html_response(page('Ingen resultater')))
stopifnot(zero$calls==1L,zero$result$check$status=='confirmed_empty')
html_failure <- invoke(html_response(page('Export available')),html_response(page('Internal failure')))
stopifnot(html_failure$calls==2L,inherits(html_failure$result,'error'),file.exists(file.path(work,'group-failure-AP.json')))
excel <- file.path(work,'export.xlsx');writexl::write_xlsx(data.frame(Varenummer='019307'),excel)
body <- readBin(excel,'raw',n=file.info(excel)$size)
exported <- invoke(html_response(page('1 lægemiddel',extra='<td>019307</td>')),
                   httr2::response(headers=list('content-type'='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'),body=body))
stopifnot(exported$calls==2L,exported$result$check$status=='exported',exported$result$check$rows==1)
cat('PASS: confirmed empty skips export; ambiguous HTML rejects capture; actual Excel response parses and counts.\n')
