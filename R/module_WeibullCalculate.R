weibullCalculate_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mainPanel(
        box(title = "Weibull plot generator", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("abremplot") ) ), 
        box(title = "Weibull summary", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            verbatimTextOutput( ns("test") ) )
      ), 
      sidebarPanel(
        sliderInput( inputId = ns( "confcalc") , animate = TRUE,
                     label= "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8 ), 
        radioButtons(inputId = ns('fit.selector'), 
                     label = 'Method Fit', selected = 'mle', 
                     choices = c("Rank Regression"="RRxony",
                                 "MLE"='mle', 
                                 "MLE-rba" = 'mle-rba') ), 
        radioButtons(inputId = ns('method.conf'), 
                     label = 'Method confidence',selected = 'lrb',
                     choices = c("Likelihood Ratio"='lrb', 
                                 "Beta Binomial"='bbb' ) )

        
      )
    )
  )
}


weibullCalculate_server <- function(input, output, session, data){
  
  output$test <- renderPrint( summary(data()) )
  
  source("./R/foo_make_abrem_plot.R")
  output$abremplot <- renderPlot({
    dummy2 <- read.csv("./data/Weibull_template.csv")
    make_abrem_plot(df = dummy, input, title = "This is my plot")
  })
  
  # df <- callModule(uploadData_server, "page_uploadData2")
  # output$table <- renderDataTable({ df() })
  # 
}