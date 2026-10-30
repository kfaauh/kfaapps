#!/usr/bin/env Rscript
# Read-only measurement, no database connection and no source requests.
script<-sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','..','monitorering-app','analysis.R'))
args<-commandArgs(TRUE)
if(!length(args)||length(args)>2L)stop('Usage: publication_metrics.R ANALYSIS_RDS [ATC_PREFIX]')
prefix<-if(length(args)==2L)args[2]else'N06'
read_time<-system.time({x<-readRDS(args[1]);validate_analysis_bundle(x)})[['elapsed']]
months<-sort(unique(format(as.Date(x$tables$extracts$month),'%Y-%m')))
aggregation_time<-if(length(months))system.time({
 m<-analysis_months(x,prefix,months[1],tail(months,1))
})[['elapsed']]else NA_real_
cat('Published:',x$published_at,'\nFile MiB:',round(file.info(args[1])$size/1024^2,2),
 '\nBundle RAM MiB:',round(as.numeric(object.size(x))/1024^2,2),
 '\nConsumption product-month rows:',nrow(x$tables$consumption),
 '\nValidated read seconds:',read_time,'\nAggregation seconds:',aggregation_time,'\n')
if(length(months))print(m,row.names=FALSE)
