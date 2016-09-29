## Chapter 6-9
library(plotly)
source("./R/Chp_6_Zero_tests.R")

zeroFailure_test_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(inputId = ns("obs"),
                     label = "Number of components testing",
                     min = 1, value =  3),
        numericInput(inputId = ns("beta"), 
                     label   = HTML("&beta;:","Beta of the failure mode to test"),
                     value   = 1 ) ,
        numericInput(inputId = ns("dem_eta"),
                     label =  HTML("&eta;:", "Characteristic life to demonstrate"), 
                     value = 500),
        sliderInput( inputId = ns("confi") ,
                     label = "Confidence", 
                     min = .6, max = .99, value = .1, 
                     animate = T)
        
      ),
      mainPanel(
        box(title = "Failure test plot", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotlyOutput( ns("fixComponents_plot") ) , 
            footer = "How much time will it be necessary to pass the test? (for
            different number of components in the test and different levels of 
            confidence)"), 
        box(title = "Zero failure test", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ns( "fixComponents_result") ), 
            footer =  textOutput( ns('fix_component_footer'))), 
        box(title = "Characteristic life multiplyer (k)", width = 6,# height = 820, 
                status="info", solidHeader = T, collapsible=F,  
                verbatimTextOutput( ns( "lifeMultiplyer") ), 
                footer = "To increase or decrease the test time")
        )
      )
    )
}

zeroFailure_test_server <- function(input, output, session){

  print("I am at the Server side Zero test module!")

  output$fixComponents_result <- renderPrint({
    time_Pass_Zero_failure(n = input$obs, beta = input$beta, 
                           dem_eta = input$dem_eta, conf = input$confi) 
    
  })
  
  output$fix_component_footer <- renderText({
    paste0(    
      "Time needed without a failure to show that the failure mode was either eleminated or significantly improved (with a ",
      input$confi*100,
      " % confidence)."
    )
  })
  output$lifeMultiplyer <- renderPrint({
    round(
      x = k_At_Nconf(n = input$obs, beta = input$beta, 
                     conf = input$confi), 
      digits = 2)
    
  })
  output$fixComponents_plot<- renderPlotly({
    nComponents <- input$obs
    
    
    df <- expand.grid(
      obs= seq(1, input$obs* 1.5, by = 1 ), 
      beta = input$beta, 
      conf=  seq(0.6, .99, by=.01), 
      dem_eta = input$dem_eta)
    
    df$ttest <- time_Pass_Zero_failure(n = df$obs, 
                                       beta = df$beta, 
                                       dem_eta = df$dem_eta, 
                                       conf = df$conf) 
    
    
    p <- ggplot(df, 
                aes(x=ttest, y=conf, group=obs) ) +
      geom_line(col="grey") +
      geom_line(data = df %>% dplyr::filter(obs %in% input$obs), 
                aes(x=ttest, y=conf), size=1.5, col="red") + 
    geom_hline(aes(yintercept= input$confi)) + 
      ylab("Confidence") + xlab("Duration testing time") + 
      ggtitle(paste0("Total of ",nComponents, " testing components")) + 
      theme_minimal()
    
    
    ggplotly(p)
  })
  
  # output$fixComponents_plot<- renderPlotly({
  #  plot <- fixComponents_plot_calculate() +  geom_hline(aes(yintercept= input$confi))  
  #  
  #  ggplotly(plot)
  # })
  
}