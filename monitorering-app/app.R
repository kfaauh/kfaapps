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
   body{padding:28px} pre{white-space:pre-wrap;overflow-wrap:anywhere} .container-fluid{max-width:1500px;margin:auto}
   h1{color:#0033a0;font-weight:700} .well{background:white;border:1px solid #e1e7ef;border-radius:14px}
   .tab-content{background:white;border:1px solid #e1e7ef;border-radius:0 14px 14px 14px;padding:22px}
   .scope{padding:15px;border-left:4px solid #0033a0;background:#edf2ff;margin:15px 0}
   .note{color:#667085} table{font-size:13px}'))),
 h1('Monitorering'),p('Lægemiddelforbrug, faktiske regionale tilskud og taksthistorik'),
 sidebarLayout(
  sidebarPanel(width=3,
   uiOutput('atc_ui'),
   uiOutput('dates_ui'),selectInput('level','Tabellens ATC-niveau',choices=setNames(c(1,3,4,5,7),paste('ATC',1:5)),selected=7),
   textInput('product','Varenummer · valgfrit (6 cifre)',''),
   downloadButton('export','Hent filtrerede analysedata'),hr(),
   p(class='note','Alle fem bopælsregioner · DDD · Flag T3: Ja · kroniske sygdomme: Ingen valg.'),
   p(class='note','Import udføres af administrator uden for appen.')),
  mainPanel(width=9,uiOutput('health'),div(class='scope',textOutput('scope')),
   tabsetPanel(
    tabPanel('Datastatus',textOutput('latest_observation'),p('Forespørgselsdækning dokumenterer indlæst afgrænsning; fuld populationsdækning kræver uafhængig kontrol.'),DTOutput('coverage'),tags$details(tags$summary('Import- og revisionshistorik'),DTOutput('extracts'))),
    tabPanel('Forbrug og tilskud',plotOutput('series',height='440px'),DTOutput('monthly'),h4('Samme måneder året før'),p('Sammenligningen viser observerede summer. Manglende produktmålinger og ændret dækning kan påvirke forskellen.'),plotOutput('year_series',height='360px'),DTOutput('year_comparison'),h4('ATC / varenummer'),DTOutput('details'),h4('Varenumre'),DTOutput('products')),
    tabPanel('Takster og kobling',p('Takster med ukendt gyldighed vises i historikken, men bruges ikke som historiske priser. Dagsvægtning antager jævnt salg over måneden.'),DTOutput('tariffs'),h4('Historisk kobling'),DTOutput('historical'),h4('Produkt- og prisændringer'),DTOutput('changes')),
    tabPanel('Fremskrivning og scenarie',h4('Fremskrivning af faktisk regionalt tilskud'),verbatimTextOutput('forecast'),h4('Estimat fra observerede tilskudssummer · ufuldstændighed mulig'),verbatimTextOutput('observed_estimate'),plotOutput('estimate_series',height='300px'),
      h4('Prisbaseret scenarie · observeret DDD × senest observeret tilskudspris/DDD'),
      p('Dette er et prisgrundlag ved observeret mængde, ikke faktisk regionalt tilskud og ikke en valideret prognose. Prisens observationsdato bevares. Anvendelse på tidligere måneder er et kontrafaktisk scenarie. Afledt DDD = ekspeditionspris / ekspeditionspris pr. DDD kan bruges som estimat; kildeafrunding kan påvirke resultatet. Manglende grundlag udelades.'),
      p('Matchandelene nedenfor gælder en tilgængelig pris/DDD og samme ATC; DDD-definitionens sammenlignelighed skal stadig vurderes.'),
      verbatimTextOutput('scenario'),tags$details(tags$summary('Produktmålinger og prisgrundlag'),DTOutput('current')),h4(textOutput('future_heading')),verbatimTextOutput('future_scenario'),tags$details(tags$summary('Beregnede og udeladte produkter'),DTOutput('future_products')))))))
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
 output$health <- renderUI({
   x<-cache();tagList(if(nzchar(problem())) p(role='alert',problem()),if(!is.null(x)) {
     e<-x$tables$extracts;t<-x$tables$tariffs
     div(p(paste('Publiceret:',x$published_at)),p(paste('Seneste import:',if(nrow(e)) max(as.character(e$imported_at)) else 'ingen')),
       p(paste('Seneste indlæste takstobservation:',if(nrow(t)) max(as.character(t$observed_at)) else 'ingen')))
   })
 })
 output$atc_ui <- renderUI({
   d<-bundle()$tables$consumption;e<-bundle()$tables$extracts
   codes<-sort(unique(c(e$atc_prefix,unlist(lapply(c(1,3,4,5,7),function(n)substr(d$atc5,1,n))))))
   common<-c(N='Nervesystemet',N06='Psykoanaleptika',N05='Psykoleptika',N02='Analgetika',
     C='Hjerte og kredsløb',C09='Midler med virkning på renin-angiotensinsystemet',
     A10='Midler mod diabetes',B01='Antitrombotiske midler',R03='Midler mod obstruktiv lungesygdom')
   labels<-ifelse(codes %in% names(common),paste(codes,common[codes],sep=' · '),codes)
   selected<-isolate(input$prefix);if(is.null(selected)) selected<-if('N06' %in% codes)'N06'else head(codes,1)
   selectizeInput('prefix','Søg ATC-gruppe eller indtast kode',choices=setNames(codes,labels),selected=selected,
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
   tagList(selectInput('from','Fra måned',choices,selected=from),selectInput('to','Til måned',choices,selected=to))
 })
 filters <- reactive({
   p<-toupper(trimws(input$prefix));a<-input$from;b<-input$to;v<-trimws(input$product)
   validate(need(grepl('^[A-Z]([0-9]{2}([A-Z]([A-Z]([0-9]{2})?)?)?)?$',p),'Vælg gyldigt ATC-præfiks.'),
     need(length(a)==1 && grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',a),'Fra: brug ÅÅÅÅ-MM.'),
     need(length(b)==1 && grepl('^[0-9]{4}-(0[1-9]|1[0-2])$',b),'Til: brug ÅÅÅÅ-MM.'))
   validate(need(a<=b,'Fra skal være før til.'),need(v==''||grepl('^[0-9]{6}$',v),'Varenummer skal være seks cifre.'))
   validate(need(as.numeric(as.Date(paste0(b,'-01'))-as.Date(paste0(a,'-01')))<36600,'Vælg højst 100 år.'))
   list(prefix=p,from=a,to=b,product=v)
 })
 subset_rows <- function(d,atc='atc5',period=TRUE) {
   f<-filters();keep<-startsWith(d[[atc]],f$prefix);keep[is.na(keep)]<-FALSE
   if(period) keep<-keep & format(as.Date(d$month),'%Y-%m')>=f$from & format(as.Date(d$month),'%Y-%m')<=f$to
   if(nzchar(f$product))keep<-keep & d$varenummer==f$product
   d[keep,,drop=FALSE]
 }
 monthly <- reactive({f<-filters();analysis_months(bundle(),f$prefix,f$from,f$to,f$product)})
 output$scope <- renderText({f<-filters();paste('Afgrænsning:',f$prefix,'·',f$from,'til',f$to,if(nzchar(f$product))paste('· varenummer',f$product),'· alle fem regioner. Summer er observerede værdier; manglende målinger er ikke nul.')})
 table_out <- function(id,fun) output[[id]] <- renderDT({
   d<-fun()
   private<-c('extract_id','price_snapshot_id','snapshot_id','previous_id','source','extracted_at','imported_at','assumptions')
   d<-d[setdiff(names(d),private)]
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
 table_out('coverage',function(){d<-monthly()[c('month','scope','status','observation_status','products','captures')];d$status<-unname(status_da[d$status]);d})
 table_out('monthly',function(){d<-monthly();d$status<-unname(status_da[d$status]);d})
 table_out('extracts',function(){
   f<-filters();e<-bundle()$tables$extracts
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
 table_out('historical',function()subset_rows(bundle()$tables$historical_prices))
 table_out('current',function()subset_rows(bundle()$tables$current_prices))
 table_out('changes',function(){
   # Change rows lack ATC: restrict keys using products in either snapshot.
   f<-filters();products<-subset_rows(bundle()$tables$tariff_products,'atc',FALSE)$varenummer
   d<-bundle()$tables$tariff_changes;d[d$varenummer %in% products & d$change!='unchanged',,drop=FALSE]
 })
 output$series <- renderPlot({
   d<-monthly();par(mfrow=c(2,1),mar=c(4,5,2,1));x<-seq_len(nrow(d))
   for(k in c('ddd','actual_subsidy_kr')) {
     y<-d[[k]]/1e6;ylim<-if(all(is.na(y)))c(0,1)else range(c(0,y),na.rm=TRUE);if(diff(ylim)==0)ylim<-ylim+c(0,1)
     plot(x,y,type='o',pch=16,col='#0033a0',xaxt='n',xlab='',ylab=if(k=='ddd')'Observeret mio. DDD'else'Observeret tilskud (mio. kr.)',ylim=ylim)
     axis(1,at=x,labels=d$month,las=2,cex.axis=.75)
     partial<-d$status %in% c('partial_subscopes','missing') | if(k=='ddd')d$missing_ddd>0 else d$missing_subsidy>0
     points(x[partial],y[partial],pch=1,col='#b45309',cex=1.8)
     title(sub='Orange ring: delvis dækning eller manglende måling. Brud: ingen observeret værdi.',cex.sub=.8)
   }
 })
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
 output$estimate_series <- renderPlot({
   f<-filters();d<-monthly();year<-as.integer(substr(f$to,1,4))
   x<-observed_subsidy_estimate(d,year)
   if(!x$available || nzchar(f$product)) {plot.new();text(.5,.5,'Intet beregneligt tilskudsestimat i afgrænsningen.');return()}
   m<-d[substr(d$month,1,4)==as.character(year),,drop=FALSE]
   last<-as.integer(substr(x$last_month,6,7));actual<-rep(NA_real_,12);future<-actual
   actual[as.integer(substr(m$month,6,7))]<-m$actual_subsidy_kr/1e6
   if(last<12L) future[seq.int(last+1L,12L)]<-x$monthly_rate/1e6
   par(mar=c(4,5,2,1));matplot(1:12,cbind(actual,future),type='o',pch=c(16,1),lty=c(1,2),
     col=c('#0033a0','#b45309'),xaxt='n',xlab='',ylab='Mio. kr. pr. måned',ylim=range(c(0,actual,future),na.rm=TRUE))
   axis(1,at=1:12,labels=sprintf('%d-%02d',year,1:12),las=2,cex.axis=.75)
   abline(v=last+.5,col='#667085',lty=3)
   legend('topleft',c('Observerede summer · kan være ufuldstændige','Estimat · seneste tre måneders gennemsnit'),
     col=c('#0033a0','#b45309'),lty=c(1,2),bty='n',cex=.75)
 })
 comparison <- reactive({
   f<-filters();d<-monthly();previous<-analysis_months(bundle(),f$prefix,
     sprintf('%d%s',as.integer(substr(f$from,1,4))-1,substr(f$from,5,7)),
     sprintf('%d%s',as.integer(substr(f$to,1,4))-1,substr(f$to,5,7)),f$product)
   compare_months(d,previous)
 })
 table_out('year_comparison',function(){d<-comparison();d$status<-unname(status_da[d$status]);d$previous_status<-unname(status_da[d$previous_status]);d})
 output$year_series <- renderPlot({
   d<-comparison();par(mfrow=c(2,1),mar=c(4,5,2,1));x<-seq_len(nrow(d))
   for(k in c('ddd','actual_subsidy_kr')) {
     previous<-d[[if(k=='ddd')'previous_ddd'else'previous_subsidy_kr']]
     y<-cbind(d[[k]],previous)/1e6
     ylim<-if(all(is.na(y)))c(0,1)else range(c(0,y),na.rm=TRUE);if(diff(ylim)==0)ylim<-c(0,1)
     matplot(x,y,type='o',pch=c(16,1),lty=c(1,2),col=c('#0033a0','#667085'),xaxt='n',xlab='',
       ylab=if(k=='ddd')'Mio. DDD'else'Mio. kr.',ylim=ylim)
     axis(1,at=x,labels=d$month,las=2,cex.axis=.75)
     legend('topleft',c('Valgte måneder','Samme måneder året før'),col=c('#0033a0','#667085'),lty=c(1,2),bty='n',cex=.8)
   }
 })
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
     '\nAndelen gælder observeret DDD, ikke dokumenteret populationsdækning.',
     '\nPrisgrundlag på matchede rækker:',money_da(observed_sum(d$current_price_subsidy_basis_kr)),
     '\nHeraf rækker med afledt DDD (estimat):',sum(ok & d$ddd_provenance=='unverified_ratio',na.rm=TRUE),
     '\nUdeladte rækker:',sum(!ok),'· manglende/ukendt DDD eller pris, ATC-uoverensstemmelse eller manglende produkt.',
     '\nSe kolonnen Anvendt pris observeret (UTC) for prisens observationsdato. Ingen automatisk antagelse om, at prisen fortsat gælder.')
 })
 future <- reactive({f<-filters();price_projection(subset_rows(bundle()$tables$current_prices),monthly(),as.integer(substr(f$to,1,4)))})
 output$future_heading <- renderText({x<-future();if(x$available)x$period else 'Produktbaseret prisscenarie'})
 output$future_scenario <- renderText({x<-future();if(!x$available)return(x$reason)
   d<-x$products;ok<-!is.na(d$modelled_future_basis_kr)
   paste(x$reason,'\nProdukter beregnet:',sum(ok),'/',nrow(d),
     '\nObserveret DDD i de tre måneder for beregnede produkter:',format_da(observed_sum(d$recent_ddd[ok])),'/',format_da(observed_sum(d$recent_ddd)),
     '\nModelberegnet prisgrundlag i scenarieperioden:',money_da(observed_sum(d$modelled_future_basis_kr)),
     '\nDette er ikke en prognose for faktisk regionalt tilskud. Udeladte produkter tæller ikke som nul.')
 })
 table_out('future_products',function(){x<-future();if(x$available)x$products else data.frame(Besked=x$reason)})
 output$export <- downloadHandler(filename=function()paste0('monitorering_',filters()$prefix,'.csv'),
   content=function(file){d<-subset_rows(bundle()$tables$consumption);d$population_scope<-filters()$prefix
     d$publication_time<-bundle()$published_at
     write.table(d,file,sep=';',row.names=FALSE,na='',fileEncoding='UTF-8')})
}
shinyApp(ui,server)
