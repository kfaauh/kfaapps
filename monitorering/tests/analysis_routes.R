# Small synthetic RDS v2 with independent national and regional series.
script <- sub('^--file=','',commandArgs()[grepl('^--file=',commandArgs())][1])
root <- normalizePath(file.path(dirname(script),'..','..'))
source(file.path(root,'monitorering-app','analysis.R'))
a10 <- expand.grid(month=as.Date(c(sprintf('2025-%02d-01',1:3),sprintf('2026-%02d-01',1:3))),
  region=c('DK','MJ'),stringsAsFactors=FALSE)
a10$atc5 <- 'A10BA02'
a10$varenummer <- '000001'
a10$ddd <- c(100,110,120,150,160,170,10,11,12,15,16,17)
a10$regionalt_tilskud_kr <- 10*a10$ddd
n06 <- data.frame(month=as.Date(paste0('2026-',c('01','02','03','08','09'),'-01')),
  region='MJ',atc5='N06BA04',varenummer='000002',
  ddd=c(10,20,30,40,50),regionalt_tilskud_kr=c(100,200,300,400,500))
extra <- n06[4:5,]
extra$varenummer <- '000003'
extra$ddd <- c(0,NA_real_)
extra$regionalt_tilskud_kr <- c(NA_real_,0)
c09 <- transform(a10[1,],month=as.Date('2026-03-01'),atc5='C09AA01',
  varenummer='000004',ddd=NA_real_,regionalt_tilskud_kr=NA_real_)
consumption <- rbind(a10,n06,extra,c09)
historical <- transform(consumption,actual_regional_subsidy_kr=regionalt_tilskud_kr,
  compatible_tsp_days=0L,modelled_subsidy_basis_kr=NA_real_)
current <- transform(consumption,current_price_subsidy_basis_kr=ifelse(varenummer=='000001',ddd*2,NA_real_),
  tsp_per_ddd=ifelse(varenummer=='000001',2,NA_real_),
  linkage_status=ifelse(varenummer=='000001','review_ddd_definition_and_price_date','missing_product'))
extracts <- unique(data.frame(atc_prefix=substr(consumption$atc5,1,3),
  month=consumption$month,region=consumption$region))
extracts <- rbind(extracts,data.frame(atc_prefix=c('A10','B01','C09','N06BA04','B01AA'),
  month=as.Date(c('2026-04-01','2026-01-01','2026-02-01','2026-08-01','2026-07-01')),
  region=c('DK','DK','DK','MJ','DK')))
extracts$row_count <- as.integer(!startsWith(extracts$atc_prefix,'B01') &
  extracts$month!=as.Date('2026-04-01') & !(extracts$atc_prefix=='C09' & extracts$month==as.Date('2026-02-01')))
extracts$imported_at <- extracts$extracted_at <- as.POSIXct(extracts$month,tz='UTC')
extracts$population_verified <- FALSE
old <- extracts[extracts$region=='MJ' & extracts$atc_prefix=='N06' & extracts$month==as.Date('2026-08-01'),]
old$extracted_at <- old$extracted_at-3600
old$row_count <- 0L
extracts <- rbind(extracts,old)
tariffs <- data.frame(snapshot_id=c('s1','s2','s3','s4','s5'),
  observed_at=as.POSIXct(c('2025-12-01 13:00','2026-02-01 12:00','2026-09-02 23:30','2026-09-04 12:00','2026-09-06 12:00'),tz='UTC'),
  validity_kind='observed_only',complete=c(TRUE,TRUE,FALSE,TRUE,TRUE))
products <- data.frame(snapshot_id=tariffs$snapshot_id,atc='A10BA02',varenummer='000001',
  name='Eksempelmedicin',strength='500 mg',form='tablet',pack=c('100 stk','100 stk','100 stk','200 stk','200 stk'),
  esp=c(40,NA,50,60,65),tsp=c(30,35,NA,50,49))
changes <- data.frame(snapshot_id=c('s1','s2'),previous_id=c(NA,'s1'),varenummer='000001',
  esp=c(40,NA),tsp=c(30,35),previous_esp=c(NA,40),previous_tsp=c(NA,30),change=c('added','changed'))
bundle <- list(version=2L,published_at='2026-11-24T00:00:00Z',tables=list(consumption=consumption,
  extracts=extracts,historical_prices=historical,current_prices=current,tariffs=tariffs,tariff_products=products,
  tariff_changes=changes))
validate_analysis_bundle(bundle)

output_path <- Sys.getenv('MONITOR_TEST_RDS',tempfile(fileext='.rds'))
saveRDS(bundle,output_path)
Sys.setenv(MONITOR_PUBLICATION=normalizePath(output_path))

dk <- analysis_data_inventory(bundle,'DK')
mj <- analysis_data_inventory(bundle,'MJ')
lookup <- function(d,group,month) d$state[d$group==group & d$month==month]
stopifnot(identical(analysis_main_groups(bundle),c('A10','B01','C09','N06')),
 lookup(dk,'A10','2026-03')=='observed',lookup(dk,'A10','2026-04')=='empty',
 lookup(mj,'A10','2026-04')=='missing',lookup(mj,'N06','2026-08')=='observed',
 lookup(dk,'N06','2026-08')=='missing',lookup(dk,'B01','2026-01')=='empty',
 lookup(dk,'B01','2026-07')=='partial',lookup(dk,'C09','2026-03')=='missing_measure')
price <- tariff_product_history(bundle$tables,'000001')
stopifnot(nrow(price)==5,price$esp_change[3]==10,is.na(price$tsp_change[3]),
 is.na(price$esp_change[4]),price$esp_change[5]==5,price$tsp_change[5]==-1,
 identical(price$esp_previous_at[3],price$observed_at[1]),
 format(price$observed_at[3],'%Y-%m-%d',tz='Europe/Copenhagen')=='2026-09-03',
 grepl('Eksempelmedicin',tariff_catalog(bundle$tables)$label[1]))

Sys.Date <- function() as.Date('2026-11-24')
shiny::testServer(shiny::shinyAppDir(file.path(root,'monitorering-app')),{
 session$setInputs(prefix='A10',from='2026-01',to='2026-03',region='DK',product='',
   inventory_year='2026',inventory_region='MJ',tariff_product='000001')
 stopifnot(sum(monthly()$ddd)==480,sum(comparison()$previous_ddd)==330,forecast_result()$estimate==19200,
   !forecast_result()$annualised,grepl('N06',output$inventory$html))
 session$setInputs(region='MJ')
 stopifnot(sum(monthly()$ddd)==48,sum(comparison()$previous_ddd)==33,forecast_result()$estimate==1920)
 session$setInputs(prefix='N06',from='2026-08',to='2026-10')
 stopifnot(identical(analysis_month_state(monthly()),c('observed','observed','missing')),
   monthly()$captures[1]==3,monthly()$ddd[1]==40,
   forecast_result()$annualised,forecast_result()$months==2,forecast_result()$estimate==5400,
   grepl('2026-10',output$selection_status$html))
 session$setInputs(from='2026-01')
 stopifnot(forecast_result()$annualised,forecast_result()$after_last==3,
   forecast_result()$series$estimated[4]==400,length(forecast_result()$excluded)==0)
 session$setInputs(prefix='A10',from='2025-01',to='2025-03')
 stopifnot(forecast_result()$retrospective,forecast_result()$estimate==1320,nrow(price_history())==5,
   grepl('Ingen',output$historical_status$html) || grepl('0 af',output$historical_status$html))
 stopifnot(!is.null(output$prices),!is.null(output$price_plot),!is.null(output$changes))
})
cat('PASS: synthetic A10 national/regional comparison and forecast, N06 gaps, and observed-only tariff history.\n')
