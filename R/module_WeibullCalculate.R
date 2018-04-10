# library(abernethy)
library(ggplot2)
library(dplyr)
source("./R/make_abrem.R")


## To avoid the red error screen
css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
visibility: hidden;
content: 'An error occurred. Please contact the admin.'; }
}
"
###

weibullCalculate_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type="text/css", css),
    sidebarLayout(
      mainPanel(
        box(title = "Weibull Curve(s)", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("abremplot") ) ), 
        valueBoxOutput(ns( "param_fits" ), width = 12), 
        
        # div(id = ns('placeholder')), 
        
        # box(title = "Weibull summary", width = 12,# height = 820, 
        #     status="info", solidHeader = T, collapsible=T,  
        #     verbatimTextOutput( ns("test") ) ), 
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
        checkboxInput(inputId = ns("confinter"), label = "Plot confident intervals", value = F),
        uiOutput( ns( "conditional_confinter")), 
        checkboxInput(inputId = ns("print.legend"), label = "Print legend", value = T)
        


        
      )
    )
  )
}


weibullCalculate_server <- function(input, output, session, data){

  
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
        sliderInput(ns("conflevel.generator"), animate = TRUE,
                    "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8),
        radioButtons(inputId = ns('method.conf'), 
                     label = 'Method confidence',selected = 'lrb',
                     choices = c("Likelihood Ratio"='lrb', 
                                 "Beta Binomial"='bbb' ) )
      )
    }
  })
  
  abrem.list.calculated <- reactive({
    # Make my abrem
    abrem.list <- make_several_abrems(df= data(), 
                                      fit.selector= input$fit.selector,
                                      conf.intervals= input$confinter, 
                                      conf.method= input$method.conf, 
                                      conf.level= input$conflevel.generator)
    return(abrem.list)
  })
  
  output$abremplot <- renderPlot({
    # Note: this can be done in a reactive outside this foo
    # so we only need to calculate the abrems once and 
    # share them. 
    
    ## Make my abrem
    # abrem.list <- make_several_abrems(df= data(), 
    #                            fit.selector= input$fit.selector,
    #                            conf.intervals= input$confinter, 
    #                            conf.method= input$method.conf, 
    #                            conf.level= input$conflevel.generator)
    # 
    # browser()
    # Plot my abrem object
    plot_abrem( dfa = abrem.list.calculated(), 
                conf.intervals = input$confinter, 
                conf.method = input$method.conf, 
                conf.level = input$conflevel.generator,
                is.plot.legend = input$print.legend, 
                title = "" )
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
  
  output$param_fits <- renderValueBox({
    lenght.abrem <- length(abrem.list.calculated())
    values.box <- paste0( "Weibull fits calculated: ",  lenght.abrem )
    valueBox(
      value = values.box ,
      subtitle = "subtitle fit",
      icon =  icon("thumbs-up", lib = "glyphicon"),
      color = "green")
  })
  
  # observe({
  #   # browser()
  #   # Donde ponemos el # en el div??? 
  #   # id <- length(abrem.list.calculated())
  #   id <- 2
  #   insertUI('#page_calculate-placeholder',
  #            ui = plotOutput(id))
  # 
  #   output[[id]] <- renderPlot({
  #     plot(isolate(id) )
  #          })
  # 
  # })
  
  
  # df <- callModule(uploadData_server, "page_uploadData2")
  # output$table <- renderDataTable({ df() })
  # 
}