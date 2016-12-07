## Chapter 4
library(plotly)
source("./R/Chp_4_NowRisk.R")

FailureForecast_generator_UI <- function(id){
  ns <- NS(id)
  tagList(
   
     column(width = 12,
      box(title = "Introduction",   width = NULL, 
          solidHeader = F, collapsible=T,  collapsed = T, 
          includeMarkdown("./text/text_Failure_forecast_generator.md"))
      ), 
    sidebarLayout(position = "right",
      sidebarPanel(
        numericInput(inputId = ns("beta"), 
                     label   = HTML("&beta;:","Beta of the failure mode"),
                     value   = 1 ) ,
        numericInput(inputId = ns("eta"),
                     label =  HTML("&eta;:", "Characteristic life"), 
                     value = 500),
        numericInput(inputId = ns("usage"),
                     label =  HTML("&alpha;:", "Use per unit of time/cicle"), 
                     value = 25),
        numericInput(inputId = ns("cicles"),
                     label =  "Number of cicles or units of time", 
                     value = 60),
        
        sliderInput( inputId = ns("number.suspensions") ,
                     label = "Number of suspensions", 
                     min = 1, max = 100, value = 1, 
                     animate = T),
        sliderInput( inputId = ns("time.suspension") ,
                     label = "Time of the suspensions", 
                     min = 1, max = 10000, value = 50, 
                     animate = T)
      ),
      mainPanel(
        box(title = "Failure forecast ", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotlyOutput( ns("failure_forecast_plot") ) , 
            footer = "How many failure is possible to expect? (no replacement)"), 
        
        box(title = "Failure at time/cicle", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ( ns('failure_forecast_timed')) ) ), 
        
        box(title = "Download report", 
            solidHeader = T, collapsible = T, collapsed = T,  background = "blue",
            radioButtons(ns('format'), label = '',choices =  c('PDF', 'HTML', 'Word'),
                         inline = TRUE),
            downloadButton(ns('downloadReport') ) ), 
        
        box(title = "Table of failures", width = 8,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            dataTableOutput( ( ns('failure_forecast_table'))) ) 
        
        )
      )
    )
  
}

FailureForecast_generator_server <- function(input, output, session){
 # Generate the data for the suspensions
  forecast.data <- reactive({
    number.units <- input$number.suspensions
    time.suspensions <- input$time.suspension
    
    suspensions <- data.frame( 
      part_id= rep("s", sum(number.units)), 
      time= rep(time.suspensions, number.units), 
      event= rep(0, sum(number.units)) )
    
    return(suspensions)
  })
  
  output$failure_forecast_timed <- renderPrint({
    
    FailuresForecast(u = input$cicles * input$usage,
                     df = forecast.data(),
                     eta= input$eta,
                     beta= input$beta)
  
    
  
    
  })
  
  output$failure_forecast_plot <- renderPlotly({
    library(ggplot2)
    data("diamonds")
    library(plotly)
    set.seed(100)
    d <- diamonds[sample(nrow(diamonds), 1000), ]
    plot_ly(d, x = ~carat, y = ~price, color = ~carat,
            size = ~carat, text = ~paste("Clarity: ", clarity)) %>% config(displayModeBar=F, displaylogo=F)
    
    # forecast_data()
    # input$usage
    # input$beta
    # input$eta
    
    
  })
  
  output$failure_forecast_table <- renderDataTable({
    
    
    
  })
  
  output$downloadReport <- downloadHandler(
    filename= function(){
      paste('Report Zero test failure', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      src <- normalizePath('./reports/report_Zero_test_fix_time.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
       params <- list(dem_eta = input$dem_eta, 
                      test_eta= input$test_eta, 
                      beta = input$beta, 
                      data= data.forPlot(), 
                      confi= input$confi, 
                      result= fix.time.calculation())

      library(rmarkdown)
      out <- render('report.Rmd', 
                    output_format = switch(input$format,
                                           PDF = pdf_document(), 
                                           HTML = html_document(),
                                           Word = word_document()), 
                    params = params,
                    envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    } )
  
}






















