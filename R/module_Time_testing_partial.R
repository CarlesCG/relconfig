## Chapter 6-9
library(plotly)
source("./R/Chp_6_Zero_tests.R")

Zerofailure_fix_time_UI <- function(id){
  ns <- NS(id)
  tagList(
   
     column(width = 12,
      box(title = "Introduction",   width = NULL, 
          status="info", solidHeader = F, collapsible=T,  collapsed = T, 
          includeMarkdown("./text/intro_text.md"))
      ), 
    sidebarLayout(position = "right",
      sidebarPanel(
        numericInput(inputId = ns("beta"), 
                     label   = HTML("&beta;:","Beta of the failure mode to test"),
                     value   = 1 ) ,
        numericInput(inputId = ns("dem_eta"),
                     label =  HTML("&eta;:", "Characteristic life to demonstrate"), 
                     value = 500),
        numericInput(inputId = ns("test_eta"),
                     label =  HTML("&eta;:", "Time available to test"), 
                     value = 300),
        sliderInput( inputId = ns("confi") ,
                     label = "Confidence", 
                     min = .7, max = .99, value = .1, 
                     animate = T)
      ),
      mainPanel(
        box(title = "___ ", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotlyOutput( ns("fix_time_plot") ) , 
            footer = "How much time will it be necessary to pass the test? (for
            different number of components in the test and different levels of 
            confidence)"), 
        box(title = "Zero failure test", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ns( "fix_time_result") ), 
            footer =  textOutput( ns('fix_time_footer'))), 
        box(title = "Download report", 
            solidHeader = T, collapsible = T, collapsed = T,  background = "black",
            radioButtons(ns('format'), label = '',choices =  c('PDF', 'HTML', 'Word'),
                         inline = TRUE),
            downloadButton(ns('downloadReport') ) )
        )
      ),
    
      box(title = "Introduction",   width = NULL, 
          status="info", solidHeader = F, collapsible=T,  collapsed = T, 
          includeMarkdown("./text/intro_text.md"))
    
    
  
    )
  
}

Zerofailure_fix_time_server <- function(input, output, session){
  
  ################################################################ --
  print("I am at the Server side of the Time testing partial!")
  ###### Correct to have in range numeric inputs ################ --
  # Something here!!
  
  fix.time.calculation <- reactive({
    return(
        samples_needed(dem_eta = input$dem_eta, test_eta= input$test_eta , 
                   beta= input$beta, conf= input$confi )
    )
  })
  output$fix_time_result <- renderPrint({
    fix.time.calculation()
  })
  
  # text.for.footer <- 
  output$fix_time_footer <- renderText({
    paste0(    
      "Number of components needed to test without failure to show that the failure mode was either eleminated or significantly improved (with a ",
      input$confi*100,
      " % confidence)."
    )
  })

  data.forPlot <- reactive({
  # seq(.01, .1,by = .01)
  df <- expand.grid(
    test_eta= c( seq(.2, 1, by = .1) )  * input$dem_eta, 
    beta = input$beta, 
    dem_eta = input$dem_eta,
    conf=  seq(0.7, .99, by=.01) )
  
  df$components.needed <- samples_needed(
    dem_eta = df$dem_eta,
    test_eta = df$test_eta, 
    beta = df$beta, 
    conf = df$conf)
  return(df) 
  
})
  output$fix_time_plot<- renderPlotly({
      df <- data.forPlot()
    # df  <- df %>% filter(components.needed < 1500)
    
    p <- ggplot(df, 
                aes(x=components.needed, y=conf, 
                    group= test_eta, col= as.factor( test_eta))) +
      geom_line() + 
      geom_hline(aes(yintercept= input$confi)) +
      ylab("Confidence") + xlab("Number of components to test") + 
      ggtitle(paste0("Total of ","______", " testing components")) + 
      theme_minimal()
    
    ggplotly(p)
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
