## Chapter 6-9
library(plotly)
source("./R/Chp_6_Zero_tests.R")

Zerofailure_fix_time_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
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
        box(title = "___ plot", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotlyOutput( ns("fix_time_plot") ) , 
            footer = "How much time will it be necessary to pass the test? (for
            different number of components in the test and different levels of 
            confidence)"), 
        box(title = "Zero failure test", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ns( "fix_time_result") ), 
            footer =  textOutput( ns('fix_component_footer')))
        )
      )
    )
}

Zerofailure_fix_time_server <- function(input, output, session){
  
  ################################################################
  print("I am at the Server side of the Time testing partial!")
  ###### Correct to have in range numeric inputs ################
  # Something here!!
  
  output$fix_time_result <- renderPrint({
    samples_needed(dem_eta = input$dem_eta, test_eta= input$test_eta , 
                   beta= input$beta, conf= input$confi )
  })
  
  output$fix_time_footer <- renderText({
    paste0(    
      "Time needed without a failure to show that the failure mode was either eleminated or significantly improved (with a ",
      input$confi*100,
      " % confidence)."
    )
  })

  output$fix_time_plot<- renderPlotly({
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
  
}
