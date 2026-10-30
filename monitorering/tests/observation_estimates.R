# Synthetic cases exercise guards, not reproduction of source Test 4–7.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','..','monitorering-app','analysis.R'))
m <- data.frame(month=sprintf('2026-%02d',1:9),status='query_complete_population_unverified',
  products=c(rep(2,7),0,0),ddd=c(rep(10,7),NA,NA),
  actual_subsidy_kr=c(100,200,300,400,500,600,700,NA,NA),
  missing_ddd=0L,missing_subsidy=c(rep(1L,7),0L,0L))
today<-as.Date('2026-10-30')
f<-observed_subsidy_estimate(m,2026,today)
stopifnot(f$available,f$last_month=='2026-07',f$observed==2800,f$future==3000,f$estimate==5800,
  f$missing==7,f$recent_missing==3,f$period=='Scenarie august–december 2026')
stopifnot(!subsidy_forecast(m,2026,today)$available)
m$missing_subsidy<-0L
stopifnot(subsidy_forecast(m,2026,today)$available,subsidy_forecast(m,2026,today)$estimate==5800)
x<-m;x$actual_subsidy_kr[4]<-NA
stopifnot(!observed_subsidy_estimate(x,2026,today)$available)
x<-m;x$status[4]<-'missing'
stopifnot(!observed_subsidy_estimate(x,2026,today)$available)
x<-m[-4,]
stopifnot(!observed_subsidy_estimate(x,2026,today)$available)
x<-m;x$actual_subsidy_kr[7]<-0
stopifnot(observed_subsidy_estimate(x,2026,today)$last_month=='2026-07')
x<-m;x$actual_subsidy_kr[]<-NA
stopifnot(!observed_subsidy_estimate(x,2026,today)$available)
stopifnot(!observed_subsidy_estimate(m,2025,today)$available)
prices<-data.frame(atc5='N06BA04',varenummer='000001',month=as.Date(paste0(m$month[1:7],'-01')),
 ddd=10,tsp_per_ddd=2,linkage_status='estimated_from_price_ratio')
p<-price_projection(prices,m,2026,today)
stopifnot(p$available,p$remaining==5,p$period==f$period,p$products$modelled_future_basis_kr==100)
prices$linkage_status<-'no_valid_ddd_price';prices$tsp_per_ddd<-NA_real_
p<-price_projection(prices,m,2026,today)
stopifnot(is.na(p$products$modelled_future_basis_kr),p$products$ddd_provenance=='unavailable')
# Empty query vs missing query vs measured zero vs all-NA product row.
b<-list(tables=list(extracts=data.frame(atc_prefix='N06',month=as.Date(c('2026-01-01','2026-02-01','2026-03-01')),population_verified=FALSE),
 consumption=data.frame(atc5='N06BA04',varenummer='000001',month=as.Date(c('2026-01-01','2026-03-01')),ddd=c(0,NA_real_),regionalt_tilskud_kr=c(0,NA_real_))))
z<-analysis_months(b,'N06','2026-01','2026-04')
stopifnot(z$ddd[1]==0,is.na(z$ddd[2]),z$status[2]=='query_complete_population_unverified',
 z$observation_status[2]=='Ingen observationer',z$observation_status[3]=='Produktrækker uden målinger',z$status[4]=='missing')
prior<-m;prior$month<-sub('2026','2025',prior$month);prior$ddd<-5
comparison<-compare_months(m,prior)
stopifnot(comparison$ddd_change_pct[1]==100,is.na(tail(comparison$ddd_change_pct,1)))
prior$ddd<-0
stopifnot(all(is.na(compare_months(m,prior)$ddd_change_pct)))
stopifnot(format_da(1234567.89,2)=='1.234.567,89',money_da(543083211.70)=='543,1 mio. kr.',
 money_da(NA_real_)=='Ikke tilgængelig')
cat('PASS: trailing empty months, internal holes, strict vs partial subsidy, measured zero, missing provenance, comparison and Danish formatting.\n')
