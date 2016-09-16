library(ggplot2)
library(plotly)
library(dplyr)

theme_set(theme_minimal())


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("obs", "Number of components testing",min = 1, value =  3),
      numericInput(inputId = "beta", 
                   label   = HTML("&beta;:","Beta of the failure mode to test"),
                   value   = 1 ) ,
      numericInput("dem_eta", HTML("&eta;:", "Characteristic life to demonstrate"), 500),
      sliderInput("confi", "Confidence", min = .6, max = .99, value = .1) ,
      helpText(""),
      h3("Time needed without failure to pass test"),
      verbatimTextOutput("fixComponents_result"), 
      h3("that is a characteristic life multiplyer (k) of:"),
      verbatimTextOutput(  "lifeMultiplyer") 
    ),
    mainPanel(plotlyOutput("fixComponents_plot"))
  )
)



server <- function(input, output) {
  
  # Create foo and data
  k_At_Nconf <- function(n, beta, conf){
    (-(1/n) * log(1-conf) ) ^ (1/beta) 
  }
  time_Pass_Zero_failure  <- function(n, beta, dem_eta, conf){
    # k_At_Nconf Characteristic Life Multiplier at a confidence level
    k_At_Nconf <- function(n, beta, conf){
      (-(1/n) * log(1-conf) ) ^ (1/beta) 
    }
    
    time <- ( k_At_Nconf(n, beta, conf) * dem_eta ) %>% round(., 2)
    return( time ) 
  }
  time_Pass_Zero_failure(n=3, beta = 2, dem_eta = 500, conf=.9)
  
  df <-expand.grid(
    n= c(1:10, 12, 14, 16, 18, 20, 25, 30, 40, 50),
    beta = c(seq(.5, 5, by=.5)), 
    conf=  c( 0.5, 0.6, 0.7, 0.8, 0.9, .92, .95, .97, .99))
  
  df$k <- k_At_Nconf(n = df$n, beta = df$beta, df$conf )
  data <- df %>% mutate( facet = as.factor(n)) 
  
  
  output$fixComponents_result <- renderPrint({
    time_Pass_Zero_failure(n = input$obs, beta = input$beta, dem_eta = input$dem_eta, conf = input$confi) 
     
   })
  output$lifeMultiplyer <- renderPrint({
    k_At_Nconf(n = input$obs, beta = input$beta, 
               conf = input$confi)
    
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



shinyApp(ui = ui, server = server)