library(shiny)
library(bslib)
library(DT)
source('analysis.R',local=TRUE)
publication <- Sys.getenv('MONITOR_PUBLICATION','/var/lib/monitorering-public/analysis.rds')
status_da <- c(population_verified='Population verificeret',query_complete_population_unverified='Forespørgsel indlæst · population ikke verificeret',partial_subscopes='Kun dele af ATC-afgrænsningen indlæst',missing='Ingen indlæst afgrænsning')
ui <- fluidPage(
 theme=bs_theme(version=5,bg='#F6F8FB',fg='#1F2933',primary='#0033A0',
   base_font="-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif"),
 tags$head(tags$title('Monitorering'),tags$style(HTML('
   body{padding:clamp(12px,3vw,28px)} pre{white-space:pre-wrap;overflow-wrap:anywhere}
   .container-fluid{max-width:1500px;margin:auto}
   h1{color:#0033a0;font-weight:700;margin-bottom:1rem}
   h2{font-size:1.25rem;margin:0 0 .8rem}
   h3{font-size:1.05rem;margin:1.1rem 0 .6rem}
   .well,.tab-content{background:white;border:1px solid #e1e7ef;border-radius:12px;padding:clamp(12px,2vw,22px)}
   .tab-content{margin-top:.65rem}
   .tab-content .tab-content{border:0;padding:0}
   .shiny-plot-output img{max-width:100%;height:auto}
   .scope{font-weight:600;margin-bottom:.65rem}
   .note{color:#475467}
   .metrics{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,12rem),1fr));gap:.75rem;margin-bottom:.9rem}
   .metric{padding:.8rem 1rem;background:#edf2ff;border-radius:8px}
   .metric strong{display:block;font-size:1.35rem;color:#0033a0}
   .metric small{display:block}
   .quality{padding:.65rem .85rem;background:#f6f8fb;border-left:3px solid #63758b;margin-bottom:.8rem}
   details{margin:.75rem 0} summary{cursor:pointer;font-weight:600}
   details[open]>summary{margin-bottom:.7rem}
   table{font-size:13px}')),
   # Native disclosure elements need to report visibility changes to Shiny.
   tags$script(HTML("
     document.addEventListener('toggle', function(event) {
       if (event.target.tagName === 'DETAILS' && window.jQuery) {
         window.jQuery(event.target).trigger(event.target.open ? 'shown' : 'hidden');
       }
     }, true);
   "))),
 h1('Monitorering'),
 sidebarLayout(
  sidebarPanel(width=3,
   h2('Vælg data'),
   uiOutput('atc_ui'),
   uiOutput('dates_ui'),
   selectInput('region','Bopælsregion',choices=setNames(names(analysis_regions),analysis_regions),selected='DK',selectize=FALSE),
   tags$details(tags$summary('Flere filtre'),
     textInput('product','Varenummer (6 cifre)','')),
   downloadButton('export','Hent CSV')),
  mainPanel(width=9,uiOutput('health'),div(class='scope',textOutput('scope')),
   tabsetPanel(
    tabPanel('Forbrug og tilskud',
      uiOutput('overview'),
      plotOutput('series',height='400px'),
      p(class='note','Orange ring markerer delvis dækning eller manglende produktmålinger. Huller i kurven viser måneder uden en observeret værdi.'),
      tags$details(tags$summary('Månedstal'),DTOutput('monthly')),
      tags$details(tags$summary('ATC og varenumre'),
        selectInput('level','Tabellens ATC-niveau',choices=setNames(c(1,3,4,5,7),paste('ATC',1:5)),selected=7,selectize=FALSE),
        DTOutput('details'),h3('Varenumre'),DTOutput('products'))),
    tabPanel('Sammenlign år',h2('Samme måneder året før'),
      p(class='note','Sammenligningen bruger observerede summer for samme ATC og geografi. Se dækning og manglende målinger i tabellen, før du tolker en ændring.'),
      plotOutput('year_series',height='400px'),
      DTOutput('year_comparison')),
    tabPanel('Datastatus',h2('Dækning og observationer'),
      p(textOutput('latest_observation',inline=TRUE)),
      p('Forespørgselsdækning viser, hvilke ATC-afgrænsninger der er indlæst. Populationsdækning kræver særskilt kontrol. Hele Danmark bruger sit eget udtræk.'),
      DTOutput('coverage'),
      tags$details(tags$summary('Import- og revisionshistorik'),uiOutput('publication_details'),
        p(class='note','Kildeafgrænsning: DDD, Flag T3 = Ja, kroniske sygdomme = Ingen valg.'),DTOutput('extracts'))),
    tabPanel('Takster og kobling',h2('Historisk kobling'),
      p('Beregnede historiske priser kræver dokumenteret gyldighed og egnet pris pr. DDD. Dagsvægtningen antager jævnt salg over måneden.'),
      uiOutput('historical_status'),DTOutput('historical'),
      tags$details(tags$summary('Takstobservationer'),DTOutput('tariffs')),
      tags$details(tags$summary('Produkt- og prisændringer'),DTOutput('changes'))),
    tabPanel('Fremskrivning og scenarie',
      tabsetPanel(
        tabPanel('Tilskud',h2('Fremskrivning af observeret tilskud'),
          p(class='note','Fremskrivningen starter efter sidste afsluttede måned med observationer i den valgte periode. Den kan derfor omfatte måneder, der allerede er gået.'),
          verbatimTextOutput('forecast'),
          h3('Estimat fra observerede månedssummer'),
          verbatimTextOutput('observed_estimate'),uiOutput('estimate_plot_ui')),
        tabPanel('Pris ved observeret DDD',h2('Pris ved observeret DDD'),
          p('De valgte måneders observerede DDD værdisættes med den senest observerede tilskudspris pr. DDD. Anvendelse før prisdatoen er et kontrafaktisk scenarie. Afledt DDD kan være påvirket af kildeafrunding.'),
          verbatimTextOutput('scenario'),
          tags$details(tags$summary('Varenumre, prisdato og prisgrundlag'),DTOutput('current'))),
        tabPanel('Produktbaseret prisscenarie',h2(textOutput('future_heading')),
          p('Mængderne fremskrives pr. varenummer med senest observerede pris pr. DDD. Resultatet er et beregnet prisgrundlag under de valgte antagelser.'),
          verbatimTextOutput('future_scenario'),
          tags$details(tags$summary('Beregnede og udeladte produkter'),DTOutput('future_products'))))
    )
   )
  )
 ))
server <- function(input,output,session) {
 # One immutable object per read. A failed refresh keeps the last validated object.
 cache <- reactiveVal(NULL); problem <- reactiveVal('Ingen publicerede data læst endnu.');stamp <- NULL
 observe({
   invalidateLater(30000,session)
   tryCatch({
     info <- file.info(publication);if(is.na(info$size)) stop('Datafilen findes ikke eller kan ikke læses af Shiny.')
     next_stamp <- list(mtime=as.numeric(info$mtime),ctime=as.numeric(info$ctime),size=info$size)
     if(!identical(stamp,next_stamp)) {
       x <- readRDS(publication);validate_analysis_bundle(x);cache(x);stamp <<- next_stamp
     }
     problem('')
   },error=function(e) problem(paste('Dataopdatering kunne ikke læses. Seneste gyldige visning bevares.',conditionMessage(e))))
 })
 bundle <- reactive({x<-cache();validate(need(!is.null(x),'Afventer administratorens første datapublicering.'));x})
 region <- reactive({
   code<-if(is.null(input$region)) 'DK' else input$region
   validate(need(length(code)==1L && code %in% names(analysis_regions),'Vælg gyldig bopælsregion.'))
   code
 })
 output$health <- renderUI({
   if(nzchar(problem())) p(class='quality',role='alert',problem())
 })
 output$publication_details <- renderUI({
   x<-bundle();e<-analysis_region_rows(x$tables$extracts,region());t<-x$tables$tariffs
   tags$ul(
     tags$li(paste('Publiceret (UTC)',x$published_at)),
     tags$li(paste('Seneste import for geografien (UTC)',if(nrow(e)) max(as.character(e$imported_at)) else 'ingen')),
     tags$li(paste('Seneste takstobservation (UTC)',if(nrow(t)) max(as.character(t$observed_at)) else 'ingen')))
 })
 output$atc_ui <- renderUI({
   d<-bundle()$tables$consumption;e<-bundle()$tables$extracts
   codes<-sort(unique(c(e$atc_prefix,unlist(lapply(c(1,3,4,5,7),function(n)substr(d$atc5,1,n))))))
   common<-c(N='Nervesystemet',N06='Psykoanaleptika',N05='Psykoleptika',N02='Analgetika',
     C='Hjerte og kredsløb',C09='Midler med virkning på renin-angiotensinsystemet',
     A10='Midler mod diabetes',B01='Antitrombotiske midler',R03='Midler mod obstruktiv lungesygdom')
   labels<-ifelse(codes %in% names(common),paste(codes,common[codes],sep=' · '),codes)
   selected<-isolate(input$prefix);if(is.null(selected)) selected<-if('N06' %in% codes)'N06'else head(codes,1)
   selectizeInput('prefix','ATC-gruppe',choices=setNames(codes,labels),selected=selected,
     options=list(create=TRUE,placeholder='Søg fx N06 eller psykoanaleptika'))
 })
 output$dates_ui <- renderUI({
   e<-bundle()$tables$extracts;d<-bundle()$tables$consumption
   months<-if(nrow(e)) sort(unique(format(as.Date(e$month),'%Y-%m'))) else format(Sys.Date(),'%Y-%m')
   choices<-format(seq(as.Date(paste0(months[1],'-01')),as.Date(paste0(max(substr(tail(months,1),1,4),format(Sys.Date(),'%Y')),'-12-01')),by='month'),'%Y-%m')
   observed<-sort(unique(format(as.Date(d$month[!is.na(d$ddd)|!is.na(d$regionalt_tilskud_kr)]),'%Y-%m')))
   last<-if(length(observed))tail(observed,1)else tail(months,1)
   from<-isolate(input$from);to<-isolate(input$to)
   if(is.null(from)) from<-max(months[1],paste0(substr(last,1,4),'-01'))
   if(is.null(to)) to<-last
   tagList(selectInput('from','Fra måned',choices,selected=from,selectize=FALSE),selectInput('to','Til måned',choices,selected=to,selectize=FALSE))
 })
 filters <- reactive({
   p<-toupper(trimws(input$prefix));a<-input$from;b<-input$to;v<-trimws(input$product)
   validate(need(grepl('^[A-Z]([0-9]{2}([A-Z]([A-Z]([0-9]{2})?)?)?)?$',p),'Vælg gyldigt ATC-præfiks.'),
     need(length(a)==1 && grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',a),'Fra: brug ÅÅÅÅ-MM.'),
     need(length(b)==1 && grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',b),'Til: brug ÅÅÅÅ-MM.'))
   validate(need(a<=b,'Fra skal være før til.'),need(v==''||grepl('^[0-9]{6}$',v),'Varenummer skal være seks cifre.'))
   validate(need(as.numeric(as.Date(paste0(b,'-01'))-as.Date(paste0(a,'-01')))<36600,'Vælg højst 100 år.'))
   list(prefix=p,from=a,to=b,product=v,region=region())
 })
 subset_rows <- function(d,atc='atc5',period=TRUE,geographic=TRUE) {
   f<-filters()
   if(geographic) d<-analysis_region_rows(d,f$region)
   keep<-startsWith(d[[atc]],f$prefix);keep[is.na(keep)]<-FALSE
   if(period) keep<-keep & format(as.Date(d$month),'%Y-%m')>=f$from & format(as.Date(d$month),'%Y-%m')<=f$to
   if(nzchar(f$product))keep<-keep & d$varenummer==f$product
   d[keep,,drop=FALSE]
 }
 monthly <- reactive({f<-filters();analysis_months(bundle(),f$prefix,f$from,f$to,f$product,f$region)})
 output$scope <- renderText({
   f<-filters()
   paste(f$prefix,'·',analysis_regions[[f$region]],'·',f$from,'til',f$to,
     if(nzchar(f$product)) paste('· Varenummer',f$product))
 })
 output$overview <- renderUI({
   d<-monthly()
   metric <- function(label,values,missing,format_value) {
     div(class='metric',span(label),strong(format_value(observed_sum(values))),
       tags$small(paste(sum(!is.na(values)),'af',nrow(d),'måneder med måling')),
       if(any(missing>0)) tags$small(paste(format_da(sum(missing)),'manglende produktmålinger')))
   }
   complete <- sum(d$status %in% c('query_complete_population_unverified','population_verified'))
   verified <- sum(d$status=='population_verified')
   tagList(
     div(class='metrics',
       metric('Observeret forbrug (DDD)',d$ddd,d$missing_ddd,format_da),
       metric('Observeret tilskud',d$actual_subsidy_kr,d$missing_subsidy,money_da)),
     p(class='quality',paste(complete,'af',nrow(d),'måneder har fuld forespørgselsdækning.',
       paste0(verified,' af ',nrow(d),' har verificeret populationsdækning.'),
       'Ukendte målinger indgår ikke i summerne. Se Datastatus.')))
 })
 table_out <- function(id,fun) output[[id]] <- renderDT({
   d<-fun()
   private<-c('extract_id','price_snapshot_id','snapshot_id','previous_id','source','extracted_at','imported_at','assumptions')
   d<-d[setdiff(names(d),c(private,'region'))]
   labels<-c(previous_status='Forespørgselsstatus året før',observation_status='Observationer',previous_ddd='DDD året før',previous_subsidy_kr='Tilskud året før (kr.)',ddd_change_pct='DDD-ændring (%)',subsidy_change_pct='Tilskudsændring (%)',previous_missing_ddd='Manglende DDD året før',previous_missing_subsidy='Manglende tilskud året før',month='Måned',scope='ATC-afgrænsning',status='Dækning',captures='Antal udtræk',
     ddd='Observeret DDD',actual_subsidy_kr='Observeret regionalt tilskud (kr.)',regionalt_tilskud_kr='Observeret regionalt tilskud (kr.)',
     products='Varenumre',missing_ddd='Manglende DDD-måling',missing_subsidy='Manglende tilskudsmåling',
     atc='ATC',atc5='ATC5',varenummer='Varenummer',atc_prefix='ATC-afgrænsning',row_count='Målingsrækker',population_verified='Population verificeret',
     observed_at='Pris observeret (UTC)',nominal_from='Prisperiodens start',nominal_to='Prisperiodens slut',
     validity_kind='Gyldighedsgrundlag',complete='Fuldstændighed bekræftet i indsamling',effective_from='Verificeret pris fra',effective_to='Verificeret pris til',
     month_days='Kalenderdage',price_days='Dage med takstmatch',compatible_tsp_days='Dage med pris/DDD og ATC-match',atc_mismatch_days='Dage med ATC-uoverensstemmelse',
     calendar_weighted_tsp_per_ddd='Dagsvægtet tilskudspris/DDD (kr.)',calendar_weighted_esp_per_ddd='Dagsvægtet ekspeditionspris/DDD (kr.)',
     actual_regional_subsidy_kr='Observeret regionalt tilskud (kr.)',modelled_subsidy_basis_kr='Historisk prisgrundlag (kr.)',
     price_observed_at='Anvendt pris observeret (UTC)',tariff_atc='Takstens ATC',tsp_per_ddd='Tilskudspris/DDD (kr.)',ddd_provenance='DDD-grundlag',
     current_price_subsidy_basis_kr='Prisgrundlag ved observeret DDD (kr.)',linkage_status='Koblingsstatus',ddd_observed='DDD-måling findes',subsidy_observed='Tilskudsmåling findes',
     recent_ddd='Observeret DDD i tre måneder',future_ddd='Scenarie-DDD i angivet periode',price_per_ddd='Anvendt tilskudspris/DDD (kr.)',modelled_future_basis_kr='Prisgrundlag i scenarieperioden (kr.)',esp='Ekspeditionspris (kr.)',tsp='Tilskudspris (kr.)',previous_esp='Forrige ekspeditionspris (kr.)',previous_tsp='Forrige tilskudspris (kr.)',change='Ændring')
   if('validity_kind' %in% names(d)) d$validity_kind<-ifelse(d$validity_kind=='source_interval','Dokumenteret prisinterval','Kun observeret pris')
   if('change' %in% names(d)) d$change<-unname(c(added='Tilkommet',removed='Udgået',changed='Ændret',unchanged='Uændret')[d$change])
   if('linkage_status' %in% names(d)) d$linkage_status<-unname(c(estimated_from_price_ratio='Estimat fra prisforhold · kildeafrunding',missing_product='Intet produktmatch',atc_mismatch='ATC-uoverensstemmelse',no_valid_ddd_price='Intet verificeret DDD-prisgrundlag',review_ddd_definition_and_price_date='Kontrollér DDD-definition og prisdato')[d$linkage_status])
   if('ddd_provenance' %in% names(d)) d$ddd_provenance<-unname(c(unverified_ratio='Afledt af prisforhold · estimat',source_or_verified='Kilde/verificeret',unavailable='Intet dokumenteret prisgrundlag',source='Kilde',verified='Verificeret',missing='Mangler')[d$ddd_provenance])
   names(d)<-ifelse(names(d) %in% names(labels),unname(labels[names(d)]),names(d))
   table<-datatable(d,rownames=FALSE,options=list(pageLength=12,lengthMenu=c(12,25,50,100),scrollX=TRUE,
     language=list(search='Søg:',lengthMenu='Vis _MENU_ rækker',info='_START_–_END_ af _TOTAL_ rækker',infoEmpty='Ingen rækker',
       infoFiltered='(filtreret fra _MAX_)',zeroRecords='Ingen match',emptyTable='Ingen data i afgrænsningen',paginate=list(previous='Forrige','next'='Næste'))),escape=TRUE)
   amounts<-names(d)[vapply(d,is.numeric,logical(1)) & grepl('DDD|kr[.]|[(]%[)]',names(d))]
   if(length(amounts))table<-formatRound(table,columns=amounts,digits=2,mark='.',dec.mark=',')
   table
 })
 table_out('coverage',function(){
   d<-monthly()[c('month','status','observation_status','products','missing_ddd','missing_subsidy')]
   d$status<-unname(status_da[d$status]);d
 })
 table_out('monthly',function(){
   d<-monthly()[c('month','ddd','actual_subsidy_kr','missing_ddd','missing_subsidy',
     'products','observation_status','status')]
   d$status<-unname(status_da[d$status]);d
 })
 table_out('extracts',function(){
   f<-filters();e<-analysis_region_rows(bundle()$tables$extracts,f$region)
   e<-e[(startsWith(e$atc_prefix,f$prefix)|startsWith(f$prefix,e$atc_prefix)) & format(as.Date(e$month),'%Y-%m')>=f$from & format(as.Date(e$month),'%Y-%m')<=f$to,,drop=FALSE]
   e[['Udtrækstid (UTC)']]<-as.character(e$extracted_at);e[['Indlæst (UTC)']]<-as.character(e$imported_at)
   e[c('atc_prefix','month','Udtrækstid (UTC)','Indlæst (UTC)','row_count','population_verified')]
 })
 table_out('products',function()subset_rows(bundle()$tables$consumption))
 table_out('details',function(){
   d<-subset_rows(bundle()$tables$consumption);if(!nrow(d))return(d)
   d$atc<-substr(d$atc5,1,as.integer(input$level))
   # aggregate() would silently drop rows missing either measure; split explicitly.
   groups<-split(d,interaction(d$atc,d$month,drop=TRUE))
   do.call(rbind,lapply(groups,function(z)data.frame(atc=z$atc[1],month=z$month[1],products=nrow(z),
     ddd=observed_sum(z$ddd),actual_subsidy_kr=observed_sum(z$regionalt_tilskud_kr),missing_ddd=sum(is.na(z$ddd)),missing_subsidy=sum(is.na(z$regionalt_tilskud_kr)))))
 })
 table_out('tariffs',function()bundle()$tables$tariffs)
 historical <- reactive(subset_rows(bundle()$tables$historical_prices))
 table_out('historical',historical)
 output$historical_status <- renderUI({
   d<-historical()
   if(!nrow(d)) return(p(class='quality','Ingen produktmålinger i den valgte periode.'))
   matched<-if('modelled_subsidy_basis_kr' %in% names(d)) sum(!is.na(d$modelled_subsidy_basis_kr)) else 0L
   p(class='quality',paste(format_da(matched),'af',format_da(nrow(d)),
     'produktmåneder har et beregnet historisk prisgrundlag. Tabellen viser også det observerede tilskud.'))
 })
 table_out('current',function()subset_rows(bundle()$tables$current_prices))
 table_out('changes',function(){
   # Change rows lack ATC: restrict keys using products in either snapshot.
   f<-filters();products<-subset_rows(bundle()$tables$tariff_products,'atc',FALSE,geographic=FALSE)$varenummer
   d<-bundle()$tables$tariff_changes;d[d$varenummer %in% products & d$change!='unchanged',,drop=FALSE]
 })
 month_axis <- function(months) {
   ticks<-unique(round(seq(1,length(months),length.out=min(12,length(months)))))
   axis(1,at=ticks,labels=months[ticks],las=2,cex.axis=.75)
 }
 plot_units <- function(values,unit) {
   maximum<-max(c(0,abs(values)),na.rm=TRUE)
   if(maximum>=1e6) return(list(divisor=1e6,label=paste('mio.',unit)))
   if(maximum>=1e3) return(list(divisor=1e3,label=paste('tusind',unit)))
   list(divisor=1,label=unit)
 }
 output$series <- renderPlot({
   d<-monthly();par(mfrow=c(2,1),mar=c(5,5,1,1));x<-seq_len(nrow(d))
   for(k in c('ddd','actual_subsidy_kr')) {
     is_ddd<-k=='ddd'
     units<-plot_units(d[[k]],if(is_ddd)'DDD' else 'kr.')
     y<-d[[k]]/units$divisor
     ylim<-range(c(0,y),na.rm=TRUE);if(diff(ylim)==0)ylim<-c(0,1)
     plot(x,y,type='o',pch=16,col='#0033a0',xaxt='n',xlab='',las=1,
       ylab=paste0(if(is_ddd)'Forbrug' else 'Tilskud',' (',units$label,')'),ylim=ylim)
     month_axis(d$month)
     partial<-d$status %in% c('partial_subscopes','missing') | if(k=='ddd')d$missing_ddd>0 else d$missing_subsidy>0
     points(x[partial],y[partial],pch=1,col='#b45309',cex=1.8)
   }
 },alt='Observeret DDD og observeret tilskud pr. måned')
 output$latest_observation <- renderText({
   f<-filters();d<-subset_rows(bundle()$tables$consumption,period=FALSE)
   d<-d[!is.na(d$ddd)|!is.na(d$regionalt_tilskud_kr),,drop=FALSE]
   paste('Seneste måned med observationer i afgrænsningen:',if(nrow(d))max(format(as.Date(d$month),'%Y-%m'))else 'ingen',
     '· Tomme svar dokumenterer en udført forespørgsel, men er ikke anvendelige datamåneder.')
 })
 output$observed_estimate <- renderText({
   f<-filters();if(nzchar(f$product))return('Estimat vises kun for ATC-afgrænsningen uden varenummerfilter.')
   x<-observed_subsidy_estimate(monthly(),as.integer(substr(f$to,1,4)))
   if(!x$available)return(x$reason)
   paste(x$period,'\n',x$reason,'\nSeneste observation:',x$last_month,
     '\nObserveret sum januar–seneste datamåned:',money_da(x$observed),
     '\nGennemsnit pr. måned',paste(x$recent,collapse=', '),':',money_da(x$monthly_rate),
     '\nEstimeret tilskud i scenarieperioden:',money_da(x$future),
     '\nObserveret sum + scenarie (helår):',money_da(x$estimate),
     '\nManglende produktmålinger af tilskud:',format_da(x$missing),'· heraf i de seneste tre måneder:',format_da(x$recent_missing),
     '\nAntallet beskriver kun kendte produktrækker; eventuelt helt uobserverede produkter kan ikke tælles.')
 })
 output$estimate_plot_ui <- renderUI({
   f<-filters();x<-observed_subsidy_estimate(monthly(),as.integer(substr(f$to,1,4)))
   if(x$available && !nzchar(f$product))plotOutput('estimate_series',height='300px')
 })
 output$estimate_series <- renderPlot({
   f<-filters();d<-monthly();year<-as.integer(substr(f$to,1,4))
   x<-observed_subsidy_estimate(d,year)
   if(!x$available || nzchar(f$product)) {plot.new();text(.5,.5,'Intet beregneligt tilskudsestimat i afgrænsningen.');return()}
   m<-d[substr(d$month,1,4)==as.character(year) & d$month<=x$last_month,,drop=FALSE]
   last<-as.integer(substr(x$last_month,6,7));actual<-rep(NA_real_,12);future<-actual
   actual[as.integer(substr(m$month,6,7))]<-m$actual_subsidy_kr
   if(last<12L) future[seq.int(last+1L,12L)]<-x$monthly_rate
   units<-plot_units(c(actual,future),'kr.')
   y<-cbind(actual,future)/units$divisor
   ylim<-range(c(0,y),na.rm=TRUE);ylim[2]<-max(1,ylim[2]*1.2)
   par(mar=c(5,5,1,1));matplot(1:12,y,type='o',pch=c(16,1),lty=c(1,2),
     col=c('#0033a0','#b45309'),xaxt='n',xlab='',las=1,ylab=paste(units$label,'pr. måned'),ylim=ylim)
   month_axis(sprintf('%d-%02d',year,1:12))
   abline(v=last+.5,col='#667085',lty=3)
   legend('topleft',c('Observerede summer · kan være ufuldstændige','Estimat · seneste tre måneders gennemsnit'),
     col=c('#0033a0','#b45309'),lty=c(1,2),bty='n',cex=.75)
 },alt='Observerede tilskudssummer og estimat til årets udgang')
 comparison <- reactive({
   f<-filters();d<-monthly();previous<-analysis_months(bundle(),f$prefix,
     sprintf('%d%s',as.integer(substr(f$from,1,4))-1,substr(f$from,5,7)),
     sprintf('%d%s',as.integer(substr(f$to,1,4))-1,substr(f$to,5,7)),f$product,f$region)
   compare_months(d,previous)
 })
 table_out('year_comparison',function(){
   d<-comparison()[c('month','ddd','previous_ddd','ddd_change_pct','actual_subsidy_kr','previous_subsidy_kr',
     'subsidy_change_pct','status','previous_status','missing_ddd','previous_missing_ddd',
     'missing_subsidy','previous_missing_subsidy')]
   d$status<-unname(status_da[d$status]);d$previous_status<-unname(status_da[d$previous_status]);d
 })
 output$year_series <- renderPlot({
   d<-comparison();par(mfrow=c(2,1),mar=c(5,5,1,1));x<-seq_len(nrow(d))
   for(k in c('ddd','actual_subsidy_kr')) {
     previous<-d[[if(k=='ddd')'previous_ddd'else'previous_subsidy_kr']]
     values<-cbind(d[[k]],previous)
     units<-plot_units(values,if(k=='ddd')'DDD' else 'kr.')
     y<-values/units$divisor
     ylim<-range(c(0,y),na.rm=TRUE);if(diff(ylim)==0)ylim<-c(0,1)
     ylim[2]<-ylim[2]+diff(ylim)*.2
     matplot(x,y,type='o',pch=c(16,1),lty=c(1,2),col=c('#0033a0','#667085'),xaxt='n',xlab='',
       ylab=units$label,las=1,ylim=ylim)
     month_axis(d$month)
     legend('topleft',c('Valgte måneder','Samme måneder året før'),col=c('#0033a0','#667085'),lty=c(1,2),bty='n',cex=.8)
   }
 },alt='Observeret DDD og tilskud sammenlignet med samme måneder året før')
 output$forecast <- renderText({
   f<-filters();if(nzchar(f$product))return('Utilstrækkeligt datagrundlag: produktfremskrivning er ikke aktiveret.')
   ans<-subsidy_forecast(monthly(),as.integer(substr(f$to,1,4)))
   if(!ans$available)return(ans$reason)
   paste(ans$period,'\n',ans$reason,'\nObserveret:',format_da(ans$observed),'kr.\nHelår:',format_da(ans$estimate),'kr.\nFremskrivningen vedrører den valgte ATC-afgrænsning.')
 })
 output$scenario <- renderText({
   d<-subset_rows(bundle()$tables$current_prices)
   ok<-!is.na(d$current_price_subsidy_basis_kr)
   paste('Rækker med prisgrundlag:',sum(ok),'/',nrow(d),
     '\nObserveret DDD med prisgrundlag:',format_da(observed_sum(d$ddd[ok])),'/',format_da(observed_sum(d$ddd)),
     '\nDDD-andel med prisgrundlag:',if(is.na(observed_sum(d$ddd)) || observed_sum(d$ddd)<=0)'Ikke tilgængelig'else paste0(format_da(100*sum(d$ddd[ok],na.rm=TRUE)/observed_sum(d$ddd),1),' %'),
     '\nAndelen gælder kun observeret DDD. Vurder også DDD-definitionen og prisdatoen.',
     '\nPrisgrundlag på matchede rækker:',money_da(observed_sum(d$current_price_subsidy_basis_kr)),
     '\nHeraf rækker med afledt DDD (estimat):',sum(ok & d$ddd_provenance=='unverified_ratio',na.rm=TRUE),
     '\nUdeladte rækker:',sum(!ok),'· manglende/ukendt DDD eller pris, ATC-uoverensstemmelse eller manglende produkt.',
     '\nSe kolonnen Anvendt pris observeret (UTC) for prisens observationsdato. Ingen automatisk antagelse om, at prisen fortsat gælder.')
 })
 future <- reactive({f<-filters();price_projection(subset_rows(bundle()$tables$current_prices),monthly(),as.integer(substr(f$to,1,4)))})
 output$future_heading <- renderText({x<-future();if(x$available)x$period else 'Prisscenarie for resten af året'})
 output$future_scenario <- renderText({x<-future();if(!x$available)return(x$reason)
   d<-x$products;ok<-!is.na(d$modelled_future_basis_kr)
   paste(x$reason,'\nProdukter beregnet:',sum(ok),'/',nrow(d),
     '\nObserveret DDD i de tre måneder for beregnede produkter:',format_da(observed_sum(d$recent_ddd[ok])),'/',format_da(observed_sum(d$recent_ddd)),
     '\nModelberegnet prisgrundlag i scenarieperioden:',money_da(observed_sum(d$modelled_future_basis_kr)),
     '\nUdeladte produkter indgår ikke i beløbet.')
 })
 table_out('future_products',function(){x<-future();if(x$available)x$products else data.frame(Besked=x$reason)})
 output$export <- downloadHandler(filename=function()paste0('monitorering_',filters()$prefix,'_',filters()$region,'.csv'),
   content=function(file){d<-subset_rows(bundle()$tables$consumption);d$population_scope<-rep(filters()$prefix,nrow(d))
     d$region<-rep(filters()$region,nrow(d))
     d$publication_time<-rep(bundle()$published_at,nrow(d))
     write.table(d,file,sep=';',row.names=FALSE,na='',fileEncoding='UTF-8')})
}
shinyApp(ui,server)
