#!/usr/bin/env Rscript
# Compare actual exported observations with an archived reported reference.
# Usage: Rscript .../check_reference.R test7 file1.csv file2.csv ...
a <- commandArgs(TRUE)
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','R','core.R'))
must(length(a)>=2,'Supply test4|test5|test6|test7|test8 followed by all source CSV files')
ref <- jsonlite::fromJSON(file.path(monitor_root,'tests','fixtures','known_results.json'))[[a[1]]]
must(!is.null(ref),'Unknown reference')
rows <- do.call(rbind,lapply(a[-1],function(f) read.table(f,sep=';',header=TRUE,colClasses='character',fileEncoding='UTF-8-BOM',check.names=FALSE)))
must(identical(names(rows),c('atc5','varenummer','aar_maaned','maaling','vaerdi')),'Invalid schema')
must(!anyDuplicated(rows[1:4]),'Duplicate/overlapping observations')
must(all(rows$aar_maaned>=ref$from & rows$aar_maaned<=ref$to),'Rows outside reference period')
# Year-wide references require all 12 months, so a partial import cannot pass accidentally.
expected_months <- format(seq(month_date(ref$from),month_date(ref$to),by='month'),'%Y-%m')
must(setequal(unique(rows$aar_maaned),expected_months),'Reference months missing')
if(!is.null(ref$prefix)) must(all(startsWith(rows$atc5,ref$prefix)),'Rows outside reference ATC scope')
v <- as.numeric(rows$vaerdi)
actual <- list(rows=nrow(rows),atc5_count=length(unique(rows$atc5)),ddd=sum(v[rows$maaling=='ddd']),
 regionalt_tilskud_kr=sum(v[rows$maaling=='regionalt_tilskud_kr']),
 product_months=nrow(unique(rows[1:3])),ddd_rows=sum(rows$maaling=='ddd'),subsidy_rows=sum(rows$maaling=='regionalt_tilskud_kr'),
 methylphenidate_ddd=sum(v[rows$atc5=='N06BA04'&rows$maaling=='ddd']),
 methylphenidate_subsidy=sum(v[rows$atc5=='N06BA04'&rows$maaling=='regionalt_tilskud_kr']),
 methylphenidate_january_ddd=sum(v[rows$atc5=='N06BA04'&rows$aar_maaned=='2025-01'&rows$maaling=='ddd']))
fields <- intersect(names(actual),names(ref))
result <- data.frame(check=fields,expected=unlist(ref[fields]),actual=unlist(actual[fields]))
result$difference <- result$actual-result$expected
print(result,row.names=FALSE)
must(all(abs(result$difference)<=1e-6),'Reported reference differs; inspect source revisions, rounding and scope before accepting')
if(a[1] %in% c('test5','test6')) message('The five-code scope must be independently checked against the original test; aggregate agreement alone does not verify its identity.')
