
mod_ASXUI = function (id, section){
  ns = NS(id)
  switch(section,
         'overview' = {
           fluidPage(
             theme = shinytheme('cerulean'),
             titlePanel("Load Data and Symbols"),
             h6("Source: ASX.com"),
             actionButton(ns('bt_loadsym'), 'Load Real-time Symbols'),
             br(),
             plotOutput(ns('pt_loadsym'), height = '650px'),
             wellPanel(
               fluidRow(
                 column(3, wellPanel(
                   h5('Load From Disk'),
                   actionButton(ns('bt_loadFin'), 'Load Financials'),
                   actionButton(ns('bt_loadPrice'), 'Load Price'))
                 ),
                 column(6, wellPanel(
                   h5('Download Newest Quote'),
                   uiOutput(ns('ui_selectInd')),
                   actionButton(ns('bt_dwdFin'), 'Download Financials'), 
                   actionButton(ns('bt_dwdPrice'), 'Download Stock Price'))
                 )
               )
             )
           )
         },
         'industry' ={
           fluidPage(
             theme = shinytheme('cerulean'),
             titlePanel("Industry Financials Check-ups"),
             uiOutput(ns('ui_selectInd2')),
             uiOutput(ns('ui_selectFin')),
             uiOutput(ns('ui_selectPrice')),
             br(),
             
             DT::dataTableOutput(ns('dt_industry'))
           )
         },
         'clustering' = {
           fluidPage(
             theme = shinytheme('cerulean'),
             titlePanel("Cluster Companies on Selected Financials"),
             uiOutput(ns('ui_selectedFin')),
             fluidRow(
               column(6, plotOutput(ns('avgClustPlt'))),
               column(6, plotOutput(ns('maxClustPlt')))
             ),
             br(),
             wellPanel(
               fluidRow(
                 column(4, numericInput(ns('Ncluster'), 'Number of Clusters', value = 2),
                        actionButton(ns('update'), 'Show Symbol(s) below Cluster Average')
                 ),
                 column(5, h5('Average Cluster PE'), 
                        verbatimTextOutput(ns('summary'))
                 ),
                 column(3, 
                        checkboxInput(ns('rule1'), '0 < PEG < 1', value = T),
                        checkboxInput(ns('rule2'), 'Conservative Target Price', value = T)
                 )
               )
             )
             ,
             br(),
             DT::dataTableOutput(ns('targetSymbols')),
             br(),
             DT::dataTableOutput(ns('targetDT'))
           )
         },
         'validate' = {
           fluidPage(
             theme = shinytheme('cerulean'),
             titlePanel("Stock Price Validation"),
             br(),
             plotOutput(ns('pt_pca')),
             fluidRow(
               column(9,  plotlyOutput(ns('pt_price'))),
               column(3,  
                      radioButtons(ns("bt_options"), "Options",
                                          c("Original Price" = "op","Log Difference" = "log")),
                      sliderInput(ns('sld_ma'), label = "Apply MA (1 = no MA)", value = 40, min = 1, max = 60, step = 5))
             )
           )
         }
  )
}


mod_ASX = function(input, output, session){
  ns= session$ns
  asx = reactiveValues()
  
  #######################
  # Overview
  #######################
  # Load readay
  loadSymbol = eventReactive(input$bt_loadsym,{
    withProgress(message = 'Loading ASX Data', value = 0, {
      url <- 'http://www.asx.com.au/asx/research/ASXListedCompanies.csv'
      setProgress(value = 1/3)
      csv <- rawToChar(getURLContent(url, binary=TRUE))
      raw <- read.csv(textConnection(csv), skip = 2)
      colnames(raw) = c('company', 'ASX', 'industry')
      # symbols$ASX = paste0(df$ASX, '.AX')
      symbols = data.table(sapply(raw, function(x) as.character(x)), stringsAsFactors = 'FALSE')
      # change a obvious mistake
      symbols[ASX == 'BUB', industry:= 'Food, Beverage & Tobacco']
      # delete all not applicable category
      symbols = symbols[industry != 'Not Applic',]
      
      asx$symbols = symbols
      return(symbols)
      setProgress(value = 3/3)
      
    })
  })
  # Industry distribution
  output$pt_loadsym = renderPlot({
    symbols = loadSymbol()
    count = symbols[,.N, by=.(industry)]
    ggplot(count, aes(x = reorder(industry, N), N, label = N)) +
      geom_text(hjust = - 0.1, size = 3)+
      geom_col()+
      coord_flip() +
      labs(x = 'ASX Industry', title = 'ASX Industries Distribution')
  })
  
  # Select Industry and Download Financials
  output$ui_selectInd = renderUI({
    symbols = loadSymbol()
    selectInput(ns('sel_industry'), 'Select Industry', width = '100%',
                choices = sort(unique(symbols$industry)),
                selected = sort(unique(symbols$industry))[1])
  })
  
  dwdFin = eventReactive(input$bt_dwdFin,{
    codes.industry = loadSymbol()[industry == input$sel_industry, ASX]
    codes.industry = paste0('XASX:', codes.industry)
    print(codes.industry)
    
    withProgress(message = 'Download Financials...', value = 0, {
      length = length(codes.industry)
      lst_raw <- lapply(codes.industry, function(x){
        url = paste0("http://financials.morningstar.com/ajax/exportKR2CSV.html?&t=", x)
        tryCatch(
          data.table(read.csv(url, header = T, stringsAsFactors = FALSE, skip = 2)),
          error=function(e){cat('Error in loading: ', x, '\n')}
        )
        setProgress(value = which(codes.industry == x)/length)
      })
    })
   
  })
  
  observe({dwdFin()})
  
  
  
  
  
  #########################
  # Industry
  #########################
  # Select a industry to cluster on
  output$industrySelectUI = renderUI({
    selectInput(ns('industrySelect'), 'Select Industries', width = '100%',
                   choices = sort(unique(global$symbols$industry)),
                   selected = sort(unique(global$symbols$industry))[1])
  })
  
  # SHow industry table
  showIndustry = eventReactive(input$show, {
    # delelte all companys with no current yr PE
    showdf = global$financials[!is.na(global$financials$`Price/EPS Estimate Current Year`),]
    # dont pick company, industry and trade time columns
    showdf[showdf$industry == input$industrySelect, c(4:16)]
  })
  
  output$industryDT = DT::renderDataTable({
    DT:: datatable(
      showIndustry(),
      options = list(# default sort on current yr PE
                     order = list(list(6, 'asc'))
      )
    )
  })
  
  # prepare the clusters
  avgClust = reactive({
    df = showIndustry()
    print(names(df))
    hclust(dist(df[,'Price/EPS Estimate Current Year', drop = FALSE]),method = 'average')
  })
  
  maxClust = reactive({
    df = showIndustry()
    hclust(dist(df[,'Price/EPS Estimate Current Year', drop = FALSE]),method = 'complete')
  })
  
  # plot the clusters
  output$avgClustPlt = renderPlot({
    plot(avgClust(), main = 'Average Linkage Cluster')
  })
  
  output$maxClustPlt = renderPlot({
    plot(maxClust(), main = 'Complete Linkage Cluster')
  })
  
  # calculate cluster average PE and filter target Symbols
  filterSymbol = eventReactive(input$update,{
    df = showIndustry()
    cluster = avgClust()
    n = input$Ncluster
    
    df$cluster = cutree(cluster, n)
    
    avgPE = data.frame(summarise(group_by(df, cluster), 
                                 PE = mean(`Price/EPS Estimate Current Year`), 
                                 count = n(),
                                 min = min(`Price/EPS Estimate Current Year`),
                                 max = max(`Price/EPS Estimate Current Year`)))
    
    # find the target price based on cluster average PE
    df$targetPrice = round(df[['P. Close']] / df[['Price/EPS Estimate Current Year']] * 
                             sapply(df$cluster, function(x) avgPE[avgPE$cluster == x, 'PE']) * 0.9, 2)

    targets = data.frame()
    
    for(i in c(1:n)){
      target = df[(df$cluster == i) & (df$`Price/EPS Estimate Current Year` < avgPE$PE[i]), 
                  c("P. Close", "Change & % Change", 'Price/EPS Estimate Current Year', 
                    'cluster','PEG Ratio', 'targetPrice', 'Market Capitalization')]
      targets = rbind(targets,target)
    }
    
    # apply rules
    if (input$rule1 == T){
      output = targets[targets$`PEG Ratio` < 1 & targets$`PEG Ratio` > 0,]
    } else {
      output = targets
    }
    
    return(list(avgPE, output))
  })
  
  output$summary = renderPrint({
    filterSymbol()[[1]]
  })
  
  output$targetSymbols = DT::renderDataTable({
    filterSymbol()[[2]]
  })
}