weibullCalculate_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mainPanel(
        box(title = "Weibull plot generator", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            textOutput( ns("test") ) )
      ), 
      sidebarPanel(
        sliderInput( inputId = "confcalc", animate = TRUE,
                     label= "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8 ), 
        radioButtons('fit.selector', 'Method Fit',selected = 'mle', 
                     c("Rank Regression"="RRxony",
                       "MLE"='mle', 
                       "MLE-rba" = 'mle-rba') ), 
        radioButtons('method.conf', 'Method confidence',selected = 'lrb',
                     c("Likelihood Ratio"='lrb', 
                       "Beta Binomial"='bbb' ) )

        
      )
    )
  )
}


weibullCalculate_server <- function(input, output, session){
  
  output$test <- renderText( paste0("This is a test!") )
  
}