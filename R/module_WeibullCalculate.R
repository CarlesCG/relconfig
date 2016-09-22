# library(abernethy)
library(ggplot2)
library(dplyr)

css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: hidden;
content: 'An error occurred. Please contact the admin.'; }
}
"


weibullCalculate_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type="text/css", css),
    sidebarLayout(
      mainPanel(
        box(title = "Weibull plot generator", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("abremplot") ) ), 
        box(title = "Weibull summary", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            verbatimTextOutput( ns("test") ) ), 
        box(title = "Events Density ", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("ggdensity") ) )
      ), 
      sidebarPanel(
        # sliderInput( inputId = ns( "confcalc") , animate = TRUE,
        #              label= "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8 ), 
        radioButtons(inputId = ns('fit.selector'), 
                     label = 'Method Fit', selected = 'mle', 
                     choices = c("Rank Regression"="RRxony",
                                 "MLE"='mle', 
                                 "MLE-rba" = 'mle-rba') ), 
        br(),
        # Confident Intervals option
        checkboxInput(inputId = ns("confinter"), label = "Plot confident intervals?", value = F),
        uiOutput( ns( "conditional_confinter"))
        


        
      )
    )
  )
}


weibullCalculate_server <- function(input, output, session, data){
  source("./R/make_abrem.R")
  
  ## To test the plots
  # data <- reactive({
  #  read.csv("./data/Weibull_template.csv")
  # })
  
  # Confidence Intervals selector
  # Add slider for the conf.inter if the check box is pressed
  output$conditional_confinter <- renderUI({
    ns <- session$ns
    if(input$confinter == T){
      tagList(
        sliderInput(ns("conflevel_generator"), animate = TRUE,
                    "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8),
        radioButtons(inputId = ns('method.conf'), 
                     label = 'Method confidence',selected = 'lrb',
                     choices = c("Likelihood Ratio"='lrb', 
                                 "Beta Binomial"='bbb' ) )
      )
    }
  })
  
  output$test <- renderPrint({ 
    # summary(data()) 
    test()
  })
  
  output$abremplot <- renderPlot({
    # make_abrem_plot(df = data(), input, title = "This is my plot")
    abrem.object <- make_abrem(df = data(), input)
    plot_abrem( dfa = abrem.object, input,  title = "This is my plot" )
  })
  
  output$ggdensity <- renderPlot({
    # TO IMPROVE:
    # Does not generalize to more than One failure mode!
   ggplot(data(), aes(x= time, fill=as.factor(event))) + 
      geom_density(alpha=.2) +
      scale_fill_manual(name=" ", 
                        values=c("green","red"), 
                        labels=c("Survived", "Failed")) +
      geom_vline(
        data= data() %>% 
          filter(event ==1) %>% 
          summarise( stat = median(time, na.rm = T) ), 
                 aes(xintercept=stat),
                 linetype="dashed", size=1) +
      theme_bw() +
      ggtitle("Where did the events take place?")
  })
  
  
  # df <- callModule(uploadData_server, "page_uploadData2")
  # output$table <- renderDataTable({ df() })
  # 
}