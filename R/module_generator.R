generator_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mainPanel(
        box(title = "Weibull plot generator", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("logplot") ) ), 
        box(title = "Technicall info", 
            status="info", solidHeader = T, collapsible=T,  
            dataTableOutput(ns("legendPlot")  ) )
        
        ), 
      sidebarPanel(
        # width = 2, 
        tabName="Variables", 
        sliderInput( ns("npoints"),
                     "Total sample", min = 5, max = 100 , value = 50, step=1), 
        sliderInput( ns("nFailures"),
                     "Number of failures", min = 1, max = 100, value = 21, step=1), 
        sliderInput( ns("shape"),
                     "Shape", min = 0.1, max = 7, value = 3,  step = 0.1), 
        sliderInput( ns("scale"),
                     "Scale", min = 10, max = 500, value = 100, step = 50), 
        br(),
        # Confident Intervals option
        checkboxInput(inputId = ns("confinter"), label = "Plot confident intervals?", value = F),
        uiOutput( ns( "conditional_confinter"))
        #
        # Download button
        # downloadButton( ns('download_weibullGenerator'), 'Download Plot')
      ), fluid = T, position = "left" )
    
    
  )
  
  
}


generator_Server <- function(input, output, session){
  # 0. Censored and Uncensored
  # Number of failures < Num. points xa Update slider Input with correction
  observe({
    if(input$nFailures > input$npoints){
      updateSliderInput(session, "nFailures", value = input$npoints)
    }
  })
  
  output$conditional_confinter <- renderUI({
    ns <- session$ns
    if(input$confinter == T){
      sliderInput(ns("conflevel_generator"), animate = TRUE,
                  "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8)
    }
  })
  
  
  ## 1. Plots --------------------------------------------
  ## 1.1 Weibull Plot
  # How the data will be generated
  data.generator <- reactive({ 
    set.seed(1234)
    # Time & Event data 
    df <- data.frame(time = rweibull(n = input$npoints, shape = input$shape, scale = input$scale) )
    df$event <- c( rep(1, input$nFailures), rep(0, input$npoints - input$nFailures) )
    
    data.frame(time=df$time, event=df$event)
    
  })
  weibull.generator <- reactive({
    # Data greneration 
    df <- data.generator()
    
    # Fit abrem 
    dfa <- Abrem(df, col="black", pch=2)
    dfa <- abrem.fit(dfa, col="blue")
    
    if(input$confinter == T){
      dfa <- abrem.conf(dfa, method.conf.blives="lrb", cl=input$conflevel_generator,
                        unrel.n=25,
                        lty=1, lwd=3, col="red")
    }
    return(dfa)
  })
  
  output$logplot <- renderPlot({
    # Aesthetics: subtitle will be modify when the IF stament is tru
    title <- ""
    subtitle <- ""
    
    
    # Adding confident intervals
    if(input$confinter == T){
      subtitle <- paste0("Likelihood Ratio confidence level ",
                         input$conflevel_generator*100," %", "(the red line)")
    }
    
    
    # Plot
    plot(weibull.generator(), 
         main=title, sub=subtitle, is.plot.legend=T,
         xlab="Time to Failure", 
         ylab="Occurrence CDF %")
  })
  output$legendPlot <- renderDataTable({
    source("R/foo_abremLegend.R")
    WeibullLegend(weibull.generator())
  }, options= list(
    paging = F, 
    searching = FALSE
  ))
  
  # output$download_weibullGenerator <- downloadHandler(
  #   filename = "Plot_Weibull_generated.pdf",
  #   content = function(file) {
  #     observe(data.toWeibull())
  #     dev.off()
  #   }
  # )
  
}