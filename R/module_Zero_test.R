library(plotly)

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
                     min = .6, max = .99, value = .1)
       
      ),
      mainPanel(
        box(title = "Failure test plot", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotlyOutput( ns("fixComponents_plot") ) ), 
        box(title = "Zero failure test", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ns( "fixComponents_result") ), 
            footer = "Time needed without failure to pass test"), 
        box(title = "Characteristic life multiplyer (k)", width = 6,# height = 820, 
            status="info", solidHeader = T, collapsible=F,  
            verbatimTextOutput( ns( "lifeMultiplyer") ), 
            footer = "To increase or decrease the test time")
      )

    )
  )
}

"The results are based on the null hypothesis, that the new design is no better than the old"

zeroFailure_test_server <- function(input, output, session){
  ###############################################################
  ## Create functions
  # Characteristic Life Multiplier {Section 6.1 New Weibull}
  k_At_Nconf <- function(n, beta, conf){
    (-(1/n) * log(1-conf) ) ^ (1/beta) 
  }
  # Zerp test failure
  time_Pass_Zero_failure  <- function(n, beta, dem_eta, conf){
    # k_At_Nconf Characteristic Life Multiplier at a confidence level
    k_At_Nconf <- function(n, beta, conf){
      (-(1/n) * log(1-conf) ) ^ (1/beta) 
    }
    
    time <- ( k_At_Nconf(n, beta, conf) * dem_eta ) %>% round(., 2)
    return( time ) 
  }
  
  ################################################################
  print("I am at the Server side Zero test module!")
  
  output$fixComponents_result <- renderPrint({
    time_Pass_Zero_failure(n = input$obs, beta = input$beta, 
                           dem_eta = input$dem_eta, conf = input$confi) 
    
  })
  output$lifeMultiplyer <- renderPrint({
      k_At_Nconf(n = input$obs, beta = input$beta, 
                 conf = input$confi) %>% 
      round(., 2)

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
      geom_line(data = df %>% filter(obs %in% input$obs), 
                aes(x=ttest, y=conf), size=1.5, col="red") + 
      ylab("Confidence") + xlab("Duration testing time") + 
      ggtitle(paste0("Total of ",nComponents, " testing components")) + 
      theme_minimal()
    
    ggplotly(p)
  })
  
}