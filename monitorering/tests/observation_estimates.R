# Synthetic figures cover calendar and missing-value rules only.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
source(file.path(dirname(normalizePath(script)),'..','..','monitorering-app','analysis.R'))
m <- data.frame(month=sprintf('2026-%02d',1:9),status='query_complete_population_unverified',
  products=c(rep(2,7),0,0),ddd=c(rep(10,7),NA,NA),
  actual_subsidy_kr=c(100,200,300,400,500,600,700,NA,NA),
  missing_ddd=0L,missing_subsidy=c(rep(1L,7),0L,0L))
today <- as.Date('2026-10-30')
forecast <- function(data,year=2026,now=today) subsidy_forecast(data,year,now)
f <- forecast(m)
stopifnot(f$available,!f$annualised,f$last_month=='2026-07',
 f$observed==2800,f$future==3000,f$estimate==5800,f$missing==7,f$recent_missing==3,
 identical(f,observed_subsidy_estimate(m,2026,today)),
 all(f$series$incomplete[1:7]),all(is.na(f$series$observed[8:12])))
m$missing_subsidy <- 0L
stopifnot(forecast(m)$estimate==5800)
x <- m
x$actual_subsidy_kr[4] <- NA
gap <- forecast(x)
stopifnot(gap$annualised,gap$observed==2400,gap$remaining==6,gap$after_last==5,
 gap$series$estimated[4]==600,gap$estimate==6000)
x <- m
x$status[4] <- 'partial_subscopes'
stopifnot(forecast(x)$estimate==6000,identical(forecast(x)$excluded,'2026-04'))
stopifnot(forecast(m[-4,])$estimate==6000)
late <- forecast(m[5:7,])
stopifnot(late$annualised,late$observed==1800,late$estimate==7200)
short <- forecast(m[1:2,])
stopifnot(!short$annualised,short$months==2,short$estimate==1800)
x <- m
x$actual_subsidy_kr[7] <- 0
stopifnot(forecast(x)$last_month=='2026-07',forecast(x)$series$observed[7]==0)
stopifnot(forecast(m,now=as.Date('2026-03-12'))$last_month=='2026-02')
x$actual_subsidy_kr[] <- NA_real_
stopifnot(!forecast(x)$available,!forecast(m,2025)$available)
prior <- m
prior$month <- sub('2026','2025',prior$month)
historical <- forecast(prior,2025)
stopifnot(historical$retrospective,historical$estimate==5800)
full <- historical$series
full$status <- 'population_verified'
full$actual_subsidy_kr <- rep(100,12)
full$missing_subsidy <- 0L
full <- forecast(full,2025)
stopifnot(full$remaining==0,full$future==0,full$estimate==1200)
prices <- data.frame(atc5='N06BA04',varenummer='000001',month=as.Date(paste0(m$month[1:7],'-01')),
 ddd=10,tsp_per_ddd=2,linkage_status='estimated_from_price_ratio')
p <- price_projection(prices,m,2026,today)
stopifnot(p$available,p$remaining==5,p$period==f$period,p$products$modelled_future_basis_kr==100)
prices$linkage_status <- 'no_valid_ddd_price'
prices$tsp_per_ddd <- NA_real_
p <- price_projection(prices,m,2026,today)
stopifnot(is.na(p$products$modelled_future_basis_kr),p$products$ddd_provenance=='unavailable')
prior$ddd <- 5
stopifnot(compare_months(m,prior)$ddd_change_pct[1]==100)
prior$missing_ddd[1] <- 1L
prior$ddd[2] <- 0
comparison <- compare_months(m,prior)
stopifnot(is.na(comparison$ddd_change_pct[1]),is.na(comparison$ddd_change_pct[2]),
 is.na(tail(comparison$ddd_change_pct,1)))
stopifnot(format_da(1234567.89,2)=='1.234.567,89',money_da(543083211.70)=='543,1 mio. kr.',
 money_da(NA_real_)=='Ikke tilgængelig')
cat('PASS: missing months and measures, annualisation, zero, historical scenarios, comparable monthly percentages and Danish formatting.\n')
