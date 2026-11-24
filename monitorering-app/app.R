library(shiny)
library(bslib)
library(DT)
source('analysis.R',local=TRUE)
publication <- Sys.getenv('MONITOR_PUBLICATION','/var/lib/monitorering-public/analysis.rds')
status_da <- c(population_verified='Udtræk gennemført · produktdækning kontrolleret',
  query_complete_population_unverified='Udtræk gennemført · produktdækning ikke særskilt kontrolleret',
  partial_subscopes='Kun dele af ATC-gruppen er indlæst',missing='Intet indlæst udtræk')
atc_names <- c(N06='Psykoanaleptika',C09='Midler med virkning på renin-angiotensinsystemet',
  A10='Midler mod diabetes',B01='Antitrombotiske midler')
inventory_states <- c(observed='Målinger findes',missing_measure='Produktrækker uden målinger',
  empty='Udtræk uden målinger for gruppen',partial='Deludtræk uden målinger',missing='Intet udtræk')
month_names <- c('Jan','Feb','Mar','Apr','Maj','Jun','Jul','Aug','Sep','Okt','Nov','Dec')
metric_card <- function(label,value,detail=NULL) {
  div(class='metric',span(label),strong(value),tags$small(detail))
}
ui <- fluidPage(
 theme=bs_theme(version=5,bg='#F6F8FB',fg='#1F2933',primary='#0033A0',
   base_font="-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif"),
 tags$head(tags$title('Monitorering'),tags$style(HTML('
   body{padding:clamp(8px,2vw,26px)}
   .table-scroll{overflow-x:auto;max-width:100%}
   .coverage-grid{min-width:660px;text-align:center;table-layout:fixed}
   .coverage-grid th:first-child{text-align:left;width:170px;position:sticky;left:0;background:#f6f8fb}
   .coverage-grid td{vertical-align:middle}
   .coverage-grid tr>th{vertical-align:middle}
   .mobile-scroll-note{display:none}
   @media(max-width:767px){
     .mobile-scroll-note{display:block;font-size:.85rem}
     .coverage-grid th:first-child{width:124px}
     .bslib-sidebar-layout > .main{padding-inline:.75rem}
     .nav-tabs .nav-link{font-size:.93rem;padding-inline:.45rem}
     .bslib-sidebar-layout button[aria-controls=filters_panel]{width:auto;padding-inline:.55rem}
     .bslib-sidebar-layout button[aria-controls=filters_panel]::after{content:"Vælg data";margin-left:.45rem;font-size:.92rem;white-space:nowrap}
   }
   .sidebar .shiny-input-container{margin-bottom:.4rem}
   .sidebar .note{font-size:.88rem;margin:0 0 .35rem}
   .sidebar details{margin:.3rem 0}
   .sidebar .btn{align-self:start}
   .coverage-grid td span,.coverage-key span{display:inline-flex;align-items:center;justify-content:center;min-width:1.8em;min-height:1.8em;border-radius:5px;font-weight:700}
   .coverage-key{display:flex;flex-wrap:wrap;gap:.5rem 1rem;font-size:.85rem;margin-bottom:.75rem}
   .data-observed{background:#d9f1e6;color:#126440}
   .data-missing_measure,.data-partial{background:#fff0da;color:#884609}
   .data-empty{background:#e3edff;color:#244d9e}
   .data-missing{background:#eef1f5;color:#536176}
   .price-point{min-height:2.6em;margin:.4rem 0}
   .analysis-section{margin:1.4rem 0 .9rem;padding-top:1.1rem;border-top:1px solid #e1e7ef}
   .inventory-controls{display:flex;flex-wrap:wrap;gap:0 .8rem}
   .inventory-controls .shiny-input-container{width:auto;min-width:8rem;max-width:100%}
   .coverage-grid .btn-link{padding:0;text-align:left}
   .nav-tabs .nav-link{padding:.5rem .65rem}
   .nav-tabs{row-gap:.25rem}
   @media(max-width:575px){.metrics{gap:.4rem}.metric{padding:.5rem .7rem}.metric strong{font-size:1.15rem}}
    pre{white-space:pre-wrap;overflow-wrap:anywhere}
   .container-fluid{max-width:1500px;margin:auto}
   h1{color:#0033a0;font-weight:700;margin-bottom:1rem}
   h2{font-size:1.25rem;margin:0 0 .8rem}
   h3{font-size:1.05rem;margin:1.1rem 0 .6rem}
   .tab-content{min-width:0;background:white;border:1px solid #e1e7ef;border-radius:12px;padding:clamp(12px,2vw,22px)}
   .tab-content{margin-top:.65rem}
   .dataTables_wrapper{max-width:100%}
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
     document.addEventListener('DOMContentLoaded', function() {
       const toggle = document.querySelector('[aria-controls=filters_panel]');
       if (toggle) toggle.setAttribute('aria-label', 'Vælg data');
     });
     document.addEventListener('toggle', function(event) {
       if (event.target.tagName === 'DETAILS' && window.jQuery) {
         window.jQuery(event.target).trigger(event.target.open ? 'shown' : 'hidden');
       }
     }, true);
   "))),
 h1('Monitorering'),
 layout_sidebar(fillable=FALSE,fill=FALSE,
  sidebar=sidebar(id='filters_panel',title='Vælg data',width=270,open='desktop',gap='.55rem',
   uiOutput('atc_ui'),
   uiOutput('atc_hint'),
   uiOutput('dates_ui'),
   selectInput('region','Bopælsregion',choices=setNames(names(analysis_regions),analysis_regions),selected='DK',selectize=FALSE),
   actionLink('suggest_dates','Vælg perioden med seneste data'),
   tags$details(tags$summary('Flere filtre'),
     textInput('product','Varenummer (6 cifre)','')),
   downloadButton('export','Hent CSV')),
  uiOutput('health'),div(class='scope',textOutput('scope')),
   tabsetPanel(id='view',
    tabPanel('Overblik',value='inventory',
      h2('Forbrugsdata i databasen'),
      uiOutput('inventory_controls'),uiOutput('inventory'),
      div(class='analysis-section',h2('Data for dit valg'),uiOutput('selection_status'))),
    tabPanel('Forbrug og tilskud',value='consumption',
      h2('Forbrug og tilskud i den valgte periode'),
      uiOutput('overview'),
      plotOutput('series',height='390px'),
      p(class='note','En orange ring viser en ufuldstændig månedssum. Huller i kurverne viser måneder uden målinger. DDD står for definerede døgndoser.'),
      tags$details(tags$summary('Månedstal'),DTOutput('monthly')),
      div(class='analysis-section',h2('Sammenligning med samme måneder året før'),
        p(class='note','Procentændringer vises, når begge månedssummer har målinger for alle produktrækker, udtrækkene dækker den valgte ATC-kode, og året før har en værdi forskellig fra nul.'),
        plotOutput('year_series',height='390px'),DTOutput('year_comparison')),
      div(class='analysis-section',
        tags$details(tags$summary('Forbrug og tilskud fordelt på ATC-koder'),DTOutput('details')),
        tags$details(tags$summary('Forbrug og tilskud fordelt på varenumre'),DTOutput('products')))),
    tabPanel('Takstpriser',value='tariff',
      h2('Historiske lægemiddelpriser'),
      selectizeInput('tariff_product','Søg varenummer, lægemiddelnavn eller ATC-kode',choices=NULL,
        width='100%',options=list(placeholder='Vælg et produkt i taksthistorikken')),
      p(class='note','Grafen viser hele taksthistorikken for det valgte varenummer. Hvert punkt viser en pris registreret på den viste dato. Hvad prisen var mellem datoerne, fremgår ikke af observationerne.'),
      p(class='note','Takstens tilskudspris er en pris pr. pakning. De observerede tilskudsudgifter vises under Forbrug og tilskud.'),
      uiOutput('tariff_history_ui'),
      tags$details(tags$summary('Historisk kobling til forbrug'),
        p('En måned kan kun værdisættes, hvis der findes priser med dokumenteret gyldighed, brugbar pris pr. DDD og samme ATC-kode for hver dag. Ved beregning antages det, at salget fordeler sig jævnt over måneden.'),
        uiOutput('historical_status'),DTOutput('historical')),
      tags$details(tags$summary('Sammenligning af takstudtræk'),
        p('Her sammenlignes komplette takstudtræk. En tom tidligere pris betyder, at produktet manglede i forrige udtræk, at der ikke var et forrige udtræk, eller at prisen manglede. At et produkt optræder i et udtræk siger ikke, hvornår det kom på markedet.'),
        DTOutput('changes'))),
    tabPanel('Fremskrivning',value='forecast',
      uiOutput('forecast_summary'),
      uiOutput('forecast_chart'),
      tags$details(tags$summary('Avanceret: Beregning med lægemiddelpriser'),
        p('Appen ganger DDD med den senest observerede tilskudspris pr. DDD. Beløbet viser, hvad de dækkede produkter ville koste ved de anvendte takstpriser. Offentlige tilskudsudgifter kan afvige.'),
        h3('Pris ved observeret DDD'),uiOutput('scenario'),
        tags$details(tags$summary('Varenumre, prisdato og prisgrundlag'),
          p('Priserne bruges også på forbrug fra før prisens observationsdato. DDD afledt af priser kan være påvirket af afrunding i kilden.'),
          DTOutput('current')),
        h3('Prisgrundlag for årets resterende måneder'),uiOutput('future_scenario'),
        tags$details(tags$summary('Beregningsgrundlag og udeladte produkter'),
          uiOutput('future_method'),DTOutput('future_products')))),
    tabPanel('Datastatus',value='status',h2('Datadækning og revisionshistorik'),
      p(textOutput('latest_observation',inline=TRUE)),
      p('Et gennemført udtræk kan stadig mangle enkelte målinger. Kontrol af produktdækning er en særskilt gennemgang af, om alle relevante produkter indgår. Hele Danmark bruger sit eget udtræk.'),
      p(class='note','Kolonnerne om målingsmangler tæller produktrækker uden en DDD- eller tilskudsmåling. En målt værdi på 0 tæller som en måling.'),
      DTOutput('coverage'),
      tags$details(tags$summary('Import- og revisionshistorik'),uiOutput('publication_details'),
        p(class='note','Kildeafgrænsning: DDD, Flag T3 = Ja, kroniske sygdomme = Ingen valg.'),DTOutput('extracts')))
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
   d <- bundle()$tables$consumption
   e <- bundle()$tables$extracts
   codes <- sort(unique(c(e$atc_prefix,unlist(lapply(c(1,3,4,5,7),function(n) substr(d$atc5,1,n))))))
   main <- analysis_main_groups(bundle())
   group_labels <- paste(main,atc_names[main],sep=' · ')
   group_labels[!main %in% names(atc_names)] <- main[!main %in% names(atc_names)]
   choices <- list('Indlæste ATC-grupper'=setNames(main,group_labels),
     'Andre koder med indlæste data eller udtræk'=setdiff(codes,main))
   selected <- isolate(input$prefix)
   if(is.null(selected)) selected <- if('N06' %in% main) 'N06' else head(c(main,codes),1)
   if(length(selected) && !selected %in% c(codes,main)) {
     choices[['Selv indtastet']] <- setNames(selected,paste(selected,'· uden kendte data'))
   }
   tagList(selectizeInput('prefix','ATC-kode',choices=choices,selected=selected,
     options=list(create=TRUE,placeholder='Søg fx A10 eller diabetes')),
     p(class='note','Vælg en gruppe eller søg efter en underkode. Du kan også indtaste en anden ATC-kode.'))
 })
 output$atc_hint <- renderUI({
   req(input$prefix)
   prefix <- toupper(trimws(input$prefix))
   d <- bundle()$tables$consumption
   if(!any(startsWith(d$atc5,prefix) & (!is.na(d$ddd) | !is.na(d$regionalt_tilskud_kr))))
     p(class='note','Der er ingen publicerede forbrugsmålinger for denne ATC-kode.')
 })
 output$dates_ui <- renderUI({
   x <- bundle()
   months <- analysis_extract_months(x)
   if(!length(months)) months <- format(Sys.Date(),'%Y-%m')
   last_year <- max(substr(tail(months,1),1,4),format(Sys.Date(),'%Y'))
   choices <- format(seq(as.Date(paste0(months[1],'-01')),
     as.Date(paste0(last_year,'-12-01')),by='month'),'%Y-%m')
   prefix <- isolate(input$prefix)
   if(is.null(prefix)) prefix <- if('N06' %in% analysis_main_groups(x)) 'N06' else head(c(analysis_main_groups(x),''),1)
   defaults <- analysis_default_period(x,prefix,isolate(region()))
   from <- isolate(input$from)
   to <- isolate(input$to)
   tagList(selectInput('from','Fra måned',choices,selected=if(length(from) && from %in% choices) from else defaults[['from']],selectize=FALSE),
     selectInput('to','Til måned',choices,selected=if(length(to) && to %in% choices) to else defaults[['to']],selectize=FALSE))
 })
 outputOptions(output,'atc_ui',suspendWhenHidden=FALSE)
 outputOptions(output,'dates_ui',suspendWhenHidden=FALSE)
 set_period <- function(prefix,geo) {
   period <- analysis_default_period(bundle(),prefix,geo)
   updateSelectInput(session,'from',selected=period[['from']])
   updateSelectInput(session,'to',selected=period[['to']])
 }
 observeEvent(input$suggest_dates,{
   req(input$prefix)
   set_period(toupper(trimws(input$prefix)),region())
 })
 inventory_geographies <- reactive({
   x <- bundle()
   published <- unique(c(x$tables$extracts$region,x$tables$consumption$region))
   if(x$version==1L && length(analysis_extract_months(x))) published <- 'DK'
   analysis_regions[names(analysis_regions) %in% published]
 })
 output$inventory_controls <- renderUI({
   years <- rev(unique(substr(analysis_extract_months(bundle()),1,4)))
   geos <- inventory_geographies()
   if(!length(years) || !length(geos)) return(NULL)
   div(class='inventory-controls',
     selectInput('inventory_year','År',years,selected=isolate(input$inventory_year),selectize=FALSE),
     selectInput('inventory_region','Geografi',setNames(names(geos),geos),
       selected=isolate(input$inventory_region),selectize=FALSE))
 })
 observeEvent(input$inventory_group,{
   group <- input$inventory_group
   req(group %in% analysis_main_groups(bundle()),input$inventory_year,input$inventory_region %in% names(inventory_geographies()))
   updateSelectizeInput(session,'prefix',selected=group)
   updateSelectInput(session,'region',selected=input$inventory_region)
   updateTextInput(session,'product',value='')
   year_months <- sprintf('%s-%02d',input$inventory_year,1:12)
   data <- inventory_data()
   selected <- data$month[data$group==group & data$month %in% year_months & data$state!='missing']
   if(length(selected)) {
     updateSelectInput(session,'from',selected=min(selected))
     updateSelectInput(session,'to',selected=max(selected))
   }
   updateTabsetPanel(session,'view',selected='consumption')
 })
 inventory_data <- reactive({
   req(input$inventory_region %in% names(inventory_geographies()))
   analysis_data_inventory(bundle(),input$inventory_region)
 })
 output$inventory <- renderUI({
   geos <- inventory_geographies()
   if(!length(geos) || !length(analysis_main_groups(bundle()))) return(p(class='quality','Der er endnu ingen publicerede ATC-grupper med forbrugsudtræk.'))
   req(input$inventory_year,input$inventory_region)
   d <- inventory_data()
   months <- sprintf('%s-%02d',input$inventory_year,1:12)
   symbols <- setNames(c('✓','!','○','◐','–'),names(inventory_states))
   cell <- function(state) span(class=paste0('data-',state),title=inventory_states[[state]],
     `aria-label`=inventory_states[[state]],symbols[[state]])
   rows <- lapply(unique(d$group),function(group) {
     z <- d[d$group==group,]
     state <- z$state[match(months,z$month)]
     state[is.na(state)] <- 'missing'
     label <- if(group %in% names(atc_names)) paste(group,atc_names[[group]],sep=' · ') else group
     action <- sprintf("Shiny.setInputValue('inventory_group','%s',{priority:'event'})",group)
     label <- if(any(state!='missing')) tags$button(type='button',class='btn btn-link',onclick=action,label) else label
     tags$tr(tags$th(scope='row',label),lapply(state,function(s) tags$td(cell(s))))
   })
   tagList(
     p(class='note','Vælg år og geografi, og klik på en ATC-gruppe for at analysere data.'),
     p(class='note mobile-scroll-note','Rul vandret for at se flere måneder.'),
     div(class='table-scroll',tabindex='0',role='region',`aria-label`='Månedlig datadækning',
       tags$table(class='table table-sm coverage-grid',
         tags$thead(tags$tr(tags$th(scope='col','ATC-gruppe'),lapply(month_names,function(m) tags$th(scope='col',m)))),
         tags$tbody(rows))),
     div(class='coverage-key',lapply(names(inventory_states),function(s)
       div(cell(s),' ',inventory_states[[s]]))))
 })
 filters <- reactive({
   req(input$prefix,input$from,input$to)
   p<-toupper(trimws(input$prefix));a<-input$from;b<-input$to
   v<-if(is.null(input$product)) '' else trimws(input$product)
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
 measurement_quality <- function(ddd,subsidy) {
   ifelse(ddd & subsidy,'DDD og tilskud',ifelse(ddd,'DDD',ifelse(subsidy,'Tilskud','—')))
 }
 month_quality <- function(d) {
   missing <- measurement_quality(d$missing_ddd>0,d$missing_subsidy>0)
   ifelse(d$products==0,d$observation_status,
     ifelse(d$status=='partial_subscopes','Kun dele af ATC-gruppen er indlæst',
       ifelse(missing=='—','—',paste('Mangler målinger af',missing))))
 }
 output$selection_status <- renderUI({
   d <- monthly()
   state <- analysis_month_state(d)
   observed <- d$month[state=='observed']
   ddd_months <- d$month[!is.na(d$ddd)]
   describe <- function(key,label) {
     months <- d$month[state==key]
     if(length(months)) tags$li(paste0(label,': ',paste(months,collapse=', ')))
   }
   tagList(
     div(class='metrics',
       metric_card('Måneder med målinger',paste(length(observed),'af',nrow(d))),
       metric_card('Seneste måned med målt forbrug (DDD)',if(length(ddd_months)) tail(ddd_months,1) else 'Ingen i perioden')),
     tags$ul(describe('missing','Intet udtræk'),describe('empty','Udtræk uden målinger for valget'),
       describe('partial','Deludtræk uden målinger'),describe('missing_measure','Produktrækker uden målinger')),
     if(any(d$missing_ddd>0 | d$missing_subsidy>0)) p(class='quality',paste(
       'Nogle produktrækker mangler en måling.',
       if(any(d$missing_ddd>0)) paste(format_da(sum(d$missing_ddd)),'mangler DDD.'),
       if(any(d$missing_subsidy>0)) paste(format_da(sum(d$missing_subsidy)),'mangler tilskud.'))),
     if(all(state=='missing')) p(class='note','Prøv en anden periode, bopælsregion eller ATC-kode.'),
     if(any(state=='observed' & d$status=='partial_subscopes'))
       p(class='quality','Nogle måneder har kun udtræk for en del af den valgte ATC-gruppe. Se hvilke under Datastatus.'),
     actionLink('open_status','Se detaljer i Datastatus'))
 })
 observeEvent(input$open_status,updateTabsetPanel(session,'view',selected='status'))
 output$overview <- renderUI({
   d <- monthly()
   metric <- function(label,values,missing,format_value) {
     partial <- anyNA(values) || any(missing>0) || any(!d$status %in% analysis_complete_status)
     metric_card(label,format_value(observed_sum(values)),
       paste(sum(!is.na(values)),'af',nrow(d),'måneder med måling',
         if(partial) '· summen kan være ufuldstændig'))
   }
   div(class='metrics',
     metric('Observeret forbrug (DDD)',d$ddd,d$missing_ddd,format_da),
     metric('Observerede tilskudsudgifter',d$actual_subsidy_kr,d$missing_subsidy,money_da))
 })
 table_out <- function(id,fun) output[[id]] <- renderDT({
   d<-fun()
   private<-c('extract_id','price_snapshot_id','snapshot_id','previous_id','source','extracted_at','imported_at','assumptions')
   d<-d[setdiff(names(d),c(private,'region'))]
   labels<-c(previous_status='Forespørgselsstatus året før',observation_status='Observationer',previous_ddd='DDD året før',previous_subsidy_kr='Tilskud året før (kr.)',ddd_change_pct='DDD-ændring (%)',subsidy_change_pct='Tilskudsændring (%)',previous_missing_ddd='Manglende DDD året før',previous_missing_subsidy='Manglende tilskud året før',month='Måned',scope='ATC-afgrænsning',status='Dækning',captures='Antal udtræk',
     ddd='Observeret DDD',actual_subsidy_kr='Observeret regionalt tilskud (kr.)',regionalt_tilskud_kr='Observeret regionalt tilskud (kr.)',
     products='Varenumre',missing_ddd='Manglende DDD-måling',missing_subsidy='Manglende tilskudsmåling',
     atc='ATC',atc5='ATC5',varenummer='Varenummer',atc_prefix='ATC-afgrænsning',row_count='Målingsrækker',population_verified='Kontrol af produktdækning',
     observed_at='Pris observeret (UTC)',nominal_from='Prisperiodens start',nominal_to='Prisperiodens slut',
     validity_kind='Gyldighedsgrundlag',complete='Fuldstændighed bekræftet i indsamling',effective_from='Verificeret pris fra',effective_to='Verificeret pris til',
     month_days='Kalenderdage',price_days='Dage med takstmatch',compatible_tsp_days='Dage med pris/DDD og ATC-match',atc_mismatch_days='Dage med ATC-uoverensstemmelse',
     calendar_weighted_tsp_per_ddd='Dagsvægtet tilskudspris/DDD (kr.)',calendar_weighted_esp_per_ddd='Dagsvægtet ekspeditionspris/DDD (kr.)',
     actual_regional_subsidy_kr='Observeret regionalt tilskud (kr.)',modelled_subsidy_basis_kr='Historisk prisgrundlag (kr.)',
     price_observed_at='Anvendt pris observeret (UTC)',tariff_atc='Takstens ATC',tsp_per_ddd='Tilskudspris/DDD (kr.)',ddd_provenance='DDD-grundlag',
     current_price_subsidy_basis_kr='Prisgrundlag ved observeret DDD (kr.)',linkage_status='Koblingsstatus',ddd_observed='DDD-måling findes',subsidy_observed='Tilskudsmåling findes',
     recent_ddd='Observeret DDD i tre måneder',future_ddd='Scenarie-DDD i angivet periode',price_per_ddd='Anvendt tilskudspris/DDD (kr.)',modelled_future_basis_kr='Prisgrundlag i scenarieperioden (kr.)',esp='Ekspeditionspris (kr./pakning)',tsp='Tilskudspris (kr./pakning)',previous_esp='Forrige ekspeditionspris (kr.)',previous_tsp='Forrige tilskudspris (kr.)',change='Ændring')
   if('population_verified' %in% names(d)) d$population_verified <- ifelse(d$population_verified,
     'Kontrolleret','Ikke særskilt kontrolleret')
   if('validity_kind' %in% names(d)) d$validity_kind<-ifelse(d$validity_kind=='source_interval','Dokumenteret prisinterval','Kun observeret pris')
   if('change' %in% names(d)) d$change<-unname(c(first='Første komplette udtræk',
     added='Kun i dette udtræk',removed='Kun i forrige udtræk',changed='Produktdata ændret',unchanged='Uændret')[d$change])
   if('linkage_status' %in% names(d)) d$linkage_status<-unname(c(estimated_from_price_ratio='Estimat fra prisforhold · kildeafrunding',missing_product='Intet produktmatch',atc_mismatch='ATC-uoverensstemmelse',no_valid_ddd_price='Intet verificeret DDD-prisgrundlag',review_ddd_definition_and_price_date='Kontrollér DDD-definition og prisdato')[d$linkage_status])
   if('ddd_provenance' %in% names(d)) d$ddd_provenance<-unname(c(unverified_ratio='Afledt af prisforhold · estimat',source_or_verified='Kilde/verificeret',unavailable='Intet dokumenteret prisgrundlag',source='Kilde',verified='Verificeret',missing='Mangler')[d$ddd_provenance])
   names(d)<-ifelse(names(d) %in% names(labels),unname(labels[names(d)]),names(d))
   table<-datatable(d,rownames=FALSE,options=list(order=list(),pageLength=12,lengthMenu=c(12,25,50,100),scrollX=TRUE,
     language=list(search='Søg:',lengthMenu='Vis _MENU_ rækker',info='_START_–_END_ af _TOTAL_ rækker',infoEmpty='Ingen rækker',
       infoFiltered='(filtreret fra _MAX_)',zeroRecords='Ingen match',emptyTable='Ingen data i afgrænsningen',paginate=list(previous='Forrige','next'='Næste'))),escape=TRUE)
   amounts<-names(d)[vapply(d,is.numeric,logical(1)) & grepl('DDD|kr[.]|[(]%[)]',names(d))]
   if(length(amounts)) table <- formatRound(table,columns=amounts,digits=2,mark='.',dec.mark=',')
   table
 })
 table_out('coverage',function(){
   d <- monthly()
   measures <- c('missing_ddd','missing_subsidy')
   show <- measures[vapply(d[measures],function(x) any(x>0),logical(1))]
   d$status <- unname(status_da[d$status])
   d[c('month','status','observation_status','products',show)]
 })
 table_out('monthly',function(){
   d <- monthly()
   d[['Måling og dækning']] <- month_quality(d)
   d[c('month','ddd','actual_subsidy_kr','Måling og dækning')]
 })
 table_out('extracts',function(){
   f<-filters();e<-analysis_region_rows(bundle()$tables$extracts,f$region)
   e<-e[(startsWith(e$atc_prefix,f$prefix)|startsWith(f$prefix,e$atc_prefix)) & format(as.Date(e$month),'%Y-%m')>=f$from & format(as.Date(e$month),'%Y-%m')<=f$to,,drop=FALSE]
   e[['Udtrækstid (UTC)']]<-as.character(e$extracted_at);e[['Indlæst (UTC)']]<-as.character(e$imported_at)
   e[c('atc_prefix','month','Udtrækstid (UTC)','Indlæst (UTC)','row_count','population_verified')]
 })
 detail_rows <- function(keys) {
   d <- subset_rows(bundle()$tables$consumption)
   d$ddd_incomplete <- is.na(d$ddd)
   d$subsidy_incomplete <- is.na(d$regionalt_tilskud_kr)
   columns <- c(keys,'ddd','regionalt_tilskud_kr','ddd_incomplete','subsidy_incomplete')
   d <- d[columns]
   if(nrow(d)) {
     groups <- split(d,interaction(d[keys],drop=TRUE))
     d <- do.call(rbind,lapply(groups,function(z) {
       row <- z[1,keys,drop=FALSE]
       cbind(row,ddd=observed_sum(z$ddd),regionalt_tilskud_kr=observed_sum(z$regionalt_tilskud_kr),
         ddd_incomplete=any(z$ddd_incomplete),subsidy_incomplete=any(z$subsidy_incomplete))
     }))
   }
   d[['Manglende produktmålinger']] <- measurement_quality(d$ddd_incomplete,d$subsidy_incomplete)
   d[setdiff(names(d),c('ddd_incomplete','subsidy_incomplete'))]
 }
 table_out('products',function() detail_rows(c('atc5','varenummer')))
 table_out('details',function() detail_rows('atc5'))
 tariff_choices <- reactive(tariff_catalog(bundle()$tables))
 observeEvent(tariff_choices(),{
   d <- tariff_choices()
   selected <- isolate(input$tariff_product)
   updateSelectizeInput(session,'tariff_product',choices=c('',setNames(d$varenummer,d$label)),
     selected=if(length(selected) && selected %in% d$varenummer) selected else '',server=TRUE)
 })
 price_history <- reactive({
   req(input$tariff_product)
   tariff_product_history(bundle()$tables,input$tariff_product)
 })
 format_observed_at <- function(x) format(x,'%d.%m.%Y %H:%M:%S',tz='Europe/Copenhagen')
 output$tariff_history_ui <- renderUI({
   if(!nrow(tariff_choices())) return(p(class='quality','Der er endnu ingen publicerede produktpriser.'))
   if(!isTruthy(input$tariff_product)) return(p(class='quality','Vælg et varenummer for at se observationerne.'))
   d <- price_history()
   if(!nrow(d)) return(p(class='quality','Der er ingen observationer for varenummeret.'))
   latest <- d[nrow(d),,drop=FALSE]
   label <- tariff_catalog_label(input$tariff_product)
   describe_change <- function(field) {
     if(!is.finite(latest[[field]])) return('Pris mangler ved seneste observation')
     change <- latest[[paste0(field,'_change')]]
     if(is.na(change)) return('Ingen tidligere sammenlignelig pris')
     previous <- latest[[paste0(field,'_previous_at')]]
     paste0(if(change>0) '+' else '',format_da(change,2),' kr. siden ',format_observed_at(previous))
   }
   tagList(
     h3(label),
     p(class='note',paste('Senest registreret',format_observed_at(latest$observed_at),'(dansk tid)')),
     div(class='metrics',
       metric_card('Ekspeditionspris pr. pakning',price_da(latest$esp),describe_change('esp')),
       metric_card('Tilskudspris pr. pakning',price_da(latest$tsp),describe_change('tsp'))),
     if(any(is.finite(d$esp) | is.finite(d$tsp))) tagList(
       plotOutput('price_plot',height='300px',hover=hoverOpts('price_hover',delay=70,nullOutside=TRUE)),
       div(class='price-point',uiOutput('price_point')))
     else p(class='quality','De registrerede observationer har ingen pakningspriser.'),
     h3('Prisobservationer'),
     downloadButton('price_export','Hent prisobservationer (CSV)'),
     DTOutput('prices'))
 })
 tariff_catalog_label <- function(product) {
   d <- tariff_choices()
   d$label[match(product,d$varenummer)]
 }
 price_da <- function(value) if(is.finite(value)) paste(format_da(value,2),'kr.') else 'Ikke tilgængelig'
 price_points <- reactive({
   d <- price_history()
   points <- rbind(data.frame(row=seq_len(nrow(d)),date=as.numeric(d$observed_at),price=d$esp,series=rep('esp',nrow(d))),
     data.frame(row=seq_len(nrow(d)),date=as.numeric(d$observed_at),price=d$tsp,series=rep('tsp',nrow(d))))
   points[is.finite(points$price),,drop=FALSE]
 })
 output$price_plot <- renderPlot({
   d <- price_points()
   req(nrow(d)>0)
   colours <- c(esp='#0033a0',tsp='#087e65')
   ylim <- range(c(0,d$price)); ylim[2] <- max(1,ylim[2]*1.22)
   par(mar=c(3,5,1,1))
   plot(d$date,d$price,type='n',xaxt='n',xlab='',ylab='Kr. pr. pakning',ylim=ylim,las=1,
     yaxt='n')
   at <- pretty(range(d$date),n=4)
   if(diff(range(d$date))<86400) at <- unique(d$date)[1]
   axis(1,at=at,labels=format(as.POSIXct(at,origin='1970-01-01',tz='UTC'),'%d.%m.%y',tz='Europe/Copenhagen'),cex.axis=.8)
   amount_axis(ylim)
   points(d$date,d$price,pch=ifelse(d$series=='esp',16,1),col=colours[d$series],cex=ifelse(d$series=='esp',.9,1.35))
   legend('topleft',c('Ekspeditionspris','Tilskudspris'),col=colours,pch=c(16,1),bty='n',cex=.85)
 },alt='Punkter viser ekspeditionspris og tilskudspris pr. pakning på de enkelte observationsdatoer')
 output$price_point <- renderUI({
   d <- price_points()
   h <- input$price_hover
   if(is.null(h) || !nrow(d)) return(p(class='note','Hold markøren over et punkt for at se begge priser. Se også tabellen.'))
   hit <- nearPoints(d,h,xvar='date',yvar='price',threshold=14,maxpoints=1)
   if(!nrow(hit)) return(p(class='note','Hold markøren over et punkt for at se begge priser. Se også tabellen.'))
   selected <- price_history()[hit$row,,drop=FALSE]
   div(class='quality',strong(paste(format_observed_at(selected$observed_at),'· Varenummer',selected$varenummer)),
     tags$br(),paste('Ekspeditionspris',price_da(selected$esp),'· Tilskudspris',price_da(selected$tsp)))
 })
 price_export_rows <- function(d) {
   d[['Observation (dansk tid)']] <- format_observed_at(d$observed_at)
   d[c('Observation (dansk tid)','varenummer',intersect(c('atc','name','strength','form','pack'),names(d)),
     'esp','tsp','esp_change','tsp_change')]
 }
 table_out('prices',function(){
   d <- price_export_rows(price_history())
   d <- d[rev(seq_len(nrow(d))),,drop=FALSE]
   d[c('Observation (dansk tid)','esp','tsp')]
 })
 output$price_export <- downloadHandler(
   filename=function() paste0('takstpriser_',input$tariff_product,'.csv'),
   content=function(file) write.table(price_export_rows(price_history()),file,sep=';',dec=',',
     row.names=FALSE,na='',fileEncoding='UTF-8'))
 historical <- reactive({
   d <- subset_rows(bundle()$tables$historical_prices)
   if(isTruthy(input$tariff_product)) d <- d[d$varenummer==input$tariff_product,,drop=FALSE]
   d
 })
 table_out('historical',historical)
 output$historical_status <- renderUI({
   d<-historical()
   if(!nrow(d)) return(p(class='quality','Ingen produktrækker for kombinationen af varenummer, ATC-kode, periode og geografi.'))
   matched<-if('modelled_subsidy_basis_kr' %in% names(d)) sum(!is.na(d$modelled_subsidy_basis_kr)) else 0L
   p(class='quality',paste(format_da(matched),'af',format_da(nrow(d)),
     'produktmåneder har et beregnet historisk prisgrundlag. Måneder uden match kan stadig have registrerede pakningspriser på enkelte datoer.'))
 })
 table_out('current',function()subset_rows(bundle()$tables$current_prices))
 table_out('changes',function(){
   req(input$tariff_product)
   tables <- bundle()$tables
   d <- tables$tariff_changes
   d <- d[d$varenummer==input$tariff_product & d$change!='unchanged',,drop=FALSE]
   idx <- match(d$snapshot_id,tables$tariffs$snapshot_id)
   previous <- match(d$previous_id,tables$tariffs$snapshot_id)
   d[['Observation (dansk tid)']] <- format_observed_at(as.POSIXct(tables$tariffs$observed_at[idx],tz='UTC'))
   d[['Forrige observation (dansk tid)']] <- format_observed_at(as.POSIXct(tables$tariffs$observed_at[previous],tz='UTC'))
   d$change[is.na(d$previous_id)] <- 'first'
   d <- d[order(tables$tariffs$observed_at[idx],decreasing=TRUE),,drop=FALSE]
   d[intersect(c('Observation (dansk tid)','Forrige observation (dansk tid)','change','esp','previous_esp','tsp','previous_tsp'),names(d))]
 })
 month_axis <- function(months) {
   ticks<-unique(round(seq(1,length(months),length.out=min(12,length(months)))))
   axis(1,at=ticks,labels=months[ticks],las=2,cex.axis=.75)
 }
 amount_axis <- function(limits) {
   ticks <- pretty(limits)
   step <- if(length(ticks)>1) min(diff(ticks)) else 1
   digits <- if(step<1) ceiling(-log10(step)) else 0
   axis(2,at=ticks,labels=format_da(ticks,digits),las=1,cex.axis=.8)
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
     plot(x,y,type='o',pch=16,col='#0033a0',xaxt='n',yaxt='n',xlab='',las=1,
       ylab=paste0(if(is_ddd)'Forbrug' else 'Tilskud',' (',units$label,')'),ylim=ylim)
     month_axis(d$month)
     amount_axis(ylim)
     partial<-d$status %in% c('partial_subscopes','missing') | if(k=='ddd')d$missing_ddd>0 else d$missing_subsidy>0
     points(x[partial],y[partial],pch=1,col='#b45309',cex=1.8)
   }
 },alt='Observeret DDD og observeret tilskud pr. måned')
 output$latest_observation <- renderText({
   f<-filters();d<-subset_rows(bundle()$tables$consumption,period=FALSE)
   d<-d[!is.na(d$ddd)|!is.na(d$regionalt_tilskud_kr),,drop=FALSE]
   paste('Seneste måned med målinger for den valgte ATC-kode og geografi:',
     if(nrow(d)) max(format(as.Date(d$month),'%Y-%m')) else 'ingen')
 })
 forecast_result <- reactive({
   f <- filters()
   subsidy_forecast(monthly(),as.integer(substr(f$to,1,4)))
 })
 output$forecast_summary <- renderUI({
   f <- filters()
   year <- substr(f$to,1,4)
   ans <- forecast_result()
   title <- h2(paste('Forventede tilskudsudgifter i',year))
   if(!ans$available) return(tagList(title,p(class='quality',ans$reason)))
   d <- ans$series
   missing_months <- d$month[!is.na(d$estimated)]
   first <- if(ans$annualised) 'Annualisering med manglende måneder' else
     if(!ans$remaining) 'Observeret helår' else 'År til dato med estimat til december'
   tagList(
     title,
     p(strong(first),if(ans$retrospective) '· Retrospektiv beregning ud fra de valgte historiske observationer.'),
     if(substr(f$from,1,4)!=year) p(class='note',paste('Beregningen bruger kun de valgte måneder i',year)),
     div(class='metrics',
       metric_card('Observerede tilskudsudgifter',money_da(ans$observed),
         paste(ans$months,'måneder med målinger',if(any(d$incomplete)) '· summen er ufuldstændig')),
       metric_card('Beregnet tilskud for øvrige måneder',money_da(ans$future),
         paste(ans$remaining,'måneder',if(ans$annualised) paste0('· ',ans$remaining-ans$after_last,' før eller mellem observationerne'))),
       metric_card(if(ans$remaining) 'Estimeret helårsudgift' else 'Observeret helårsudgift',money_da(ans$estimate))),
     if(ans$remaining) p(paste0('Månedsskønnet på ',money_da(ans$monthly_rate),' er gennemsnittet af de seneste ',
       length(ans$recent),if(length(ans$recent)==1) ' brugbare måned (' else ' brugbare måneder (',
       paste(ans$recent,collapse=', '),'). Appen bruger det for ',paste(missing_months,collapse=', '),
       '. Der korrigeres ikke for sæson eller prisændringer.')),
     if(any(d$incomplete)) p(class='quality',paste(format_da(ans$missing),
       'produktrækker mangler tilskudsmålinger. Skønnet bruger de beløb, der er målt.',
       if(ans$recent_missing>0 && ans$remaining) 'Manglende beløb kan også påvirke månedsskønnet.')),
     if(length(ans$excluded)) p(class='quality',paste0('Målinger fra deludtræk er udeladt af beregningsgrundlaget (',
       paste(ans$excluded,collapse=', '),'). De viste beløb for disse måneder er skøn.')),
     if(!ans$retrospective && ans$remaining>0 && any(missing_months<format(Sys.Date(),'%Y-%m')))
       p(class='note','Nogle beregnede måneder ligger før dags dato og mangler i den valgte afgrænsning.'))
 })
 output$forecast_chart <- renderUI({
   if(forecast_result()$available) tagList(
     plotOutput('estimate_series',height='300px'),
     p(class='note','Blå punkter viser observerede månedssummer. Orange cirkler viser beregnede måneder. En orange ring om et blåt punkt markerer manglende produktmålinger.'))
 })
 output$estimate_series <- renderPlot({
   ans <- forecast_result()
   req(ans$available)
   d <- ans$series
   units <- plot_units(c(d$observed,d$estimated),'kr.')
   values <- cbind(d$observed,d$estimated)/units$divisor
   ylim <- range(c(0,values),na.rm=TRUE)
   if(diff(ylim)==0) ylim <- c(0,1)
   par(mar=c(3,5,1,1))
   matplot(seq_len(nrow(d)),values,type='o',pch=c(16,1),lty=c(1,2),
     col=c('#0033a0','#b45309'),xaxt='n',yaxt='n',xlab='',las=1,ylab=paste(units$label,'pr. måned'),ylim=ylim)
   axis(1,at=1:12,labels=month_names,cex.axis=.75)
   amount_axis(ylim)
   points(which(d$incomplete),values[d$incomplete,1],pch=1,col='#b45309',cex=1.8)
 },alt='Observerede tilskudsudgifter og beregnede tilskud for årets øvrige måneder')
 comparison <- reactive({
   f<-filters();d<-monthly();previous<-analysis_months(bundle(),f$prefix,
     sprintf('%d%s',as.integer(substr(f$from,1,4))-1,substr(f$from,5,7)),
     sprintf('%d%s',as.integer(substr(f$to,1,4))-1,substr(f$to,5,7)),f$product,f$region)
   compare_months(d,previous)
 })
 table_out('year_comparison',function(){
   d <- comparison()
   quality <- function(status,missing) ifelse(is.na(status) | status=='missing','Intet udtræk',
     ifelse(status=='partial_subscopes','Deludtræk',ifelse(missing,'Målinger mangler','—')))
   d[['Datamangler i valgte måneder']] <- quality(d$status,d$incomplete)
   d[['Datamangler året før']] <- quality(d$previous_status,d$previous_incomplete)
   d[c('month','ddd','previous_ddd','ddd_change_pct','actual_subsidy_kr','previous_subsidy_kr',
     'subsidy_change_pct','Datamangler i valgte måneder','Datamangler året før')]
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
     matplot(x,y,type='o',pch=c(16,1),lty=c(1,2),col=c('#0033a0','#667085'),xaxt='n',yaxt='n',xlab='',
       ylab=units$label,las=1,ylim=ylim)
     month_axis(d$month)
     amount_axis(ylim)
     partial <- if(k=='ddd') cbind(d$partial_ddd,d$previous_partial_ddd) else cbind(d$partial_subsidy,d$previous_partial_subsidy)
     for(i in 1:2) points(x[partial[,i]],y[partial[,i],i],pch=1,col='#b45309',cex=1.8)
     legend('topleft',c('Valgte måneder','Samme måneder året før','Ufuldstændig månedssum'),
       col=c('#0033a0','#667085','#b45309'),lty=c(1,2,NA),pch=c(16,1,1),bty='n',cex=.75)
   }
 },alt='Observeret DDD og tilskud sammenlignet med samme måneder året før')
 price_coverage <- function(ddd,matched) {
   total <- observed_sum(ddd)
   if(!is.finite(total) || total<=0 || any(ddd<0,na.rm=TRUE)) return('Ikke tilgængelig')
   paste0(format_da(100*sum(ddd[matched],na.rm=TRUE)/total,1),' %')
 }
 output$scenario <- renderUI({
   d <- subset_rows(bundle()$tables$current_prices)
   matched <- is.finite(d$current_price_subsidy_basis_kr)
   if(!any(matched)) return(p(class='quality','Ingen af de valgte produktrækker har et brugbart prisgrundlag.'))
   div(class='metrics',
     metric_card('Beregnet prisgrundlag',money_da(observed_sum(d$current_price_subsidy_basis_kr))),
     metric_card('Andel af observerede DDD med prisgrundlag',price_coverage(d$ddd,matched),
       paste(sum(matched),'af',nrow(d),'produktrækker kunne værdisættes')))
 })
 future <- reactive({
   f <- filters()
   price_projection(subset_rows(bundle()$tables$current_prices),monthly(),as.integer(substr(f$to,1,4)))
 })
 output$future_scenario <- renderUI({
   x <- future()
   if(!x$available) return(p(class='quality',x$reason))
   if(x$remaining==0) return(p(class='quality','Alle årets måneder er med i beregningsperioden.'))
   d <- x$products
   matched <- is.finite(d$modelled_future_basis_kr)
   if(!any(matched)) return(p(class='quality','Ingen produkter har både de nødvendige DDD-målinger og en brugbar pris.'))
   tagList(p(x$period),div(class='metrics',
     metric_card('Beregnet prisgrundlag',money_da(observed_sum(d$modelled_future_basis_kr))),
     metric_card('Andel af observerede DDD med prisgrundlag',price_coverage(d$recent_ddd,matched),
       paste(sum(matched),'af',nrow(d),'produkter kunne fremskrives'))))
 })
 output$future_method <- renderUI({
   x <- future()
   if(x$available) p(x$reason)
 })
 table_out('future_products',function(){
   x <- future()
   if(x$available) x$products else data.frame(Besked=x$reason)
 })
 output$export <- downloadHandler(filename=function()paste0('monitorering_',filters()$prefix,'_',filters()$region,'.csv'),
   content=function(file){d<-subset_rows(bundle()$tables$consumption);d$population_scope<-rep(filters()$prefix,nrow(d))
     d$region<-rep(filters()$region,nrow(d))
     d$publication_time<-rep(bundle()$published_at,nrow(d))
     write.table(d,file,sep=';',row.names=FALSE,na='',fileEncoding='UTF-8')})
}
shinyApp(ui,server)
