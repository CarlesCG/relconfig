# library(abernethy)
suppressMessages(library(abrem))
library(rintrojs)
library(readxl)

generator_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Tutorial intro.js
    introjsUI(),
    
    # Text Introduction to the module
    column(width = 12, id= ns("intro"),
           box(title = "Introduction",   width = NULL, 
               solidHeader = F, collapsible=T,  collapsed = T, 
               withMathJax ( includeMarkdown("./text/text_Failure_forecast_generator.md")) ) ) , 
    
    # Outputs frm the MainPanel
    sidebarLayout(
      mainPanel( id=ns("mainPanel"), 
        box( # id=ns("test"), 
          title = "Weibull plot generator", width = 12,# height = 820,
            status="info", solidHeader = T, collapsible=T,  
            plotOutput( ns("logplot") ) ),   
        valueBoxOutput(ns("beta"), width = 6), 
        valueBoxOutput(ns("eta"), width = 6), 
        valueBoxOutput(ns( "facts" ), width = 12), 
        box(title = "Countour plot", width = 12,
            status="info", solidHeader = T, collapsible=T,  
            plotOutput(ns("contour_plot")  ) ) #, 
        # box(title = "Technicall info", 
        #     status="info", solidHeader = T, collapsible=T,  
        #     dataTableOutput(ns("legendPlot")  ) ) 
      ), 
    
    # Inputs from the sidebar   
      sidebarPanel(
        id=ns("sidebar"), 
        # width = 2, 
        tabName="Variables", 
        sliderInput( inputId = ns("npoints"), "Total sample", 
                     min = 4, max = 100, value = 50, step=1)   %>% div(id=ns("tut_npoints")), 
        sliderInput( inputId = ns("nFailures"), "Number of failures", 
                     min = 3, max = 100, value = 21, step=1)   %>% div(id=ns("tut_nFailures")), 
        sliderInput( ns("shape"), HTML("&beta;:","Shape / Slope of the failure mode"), 
                     min = 0.1, max = 7, value = 3,  step = 0.1)   %>% div(id=ns("tut_beta")),  
        sliderInput( ns("scale"), HTML("&eta;:", "Scale / Characteristic life"),  
                     min = 10, max = 500, value = 100, step = 50)  %>% div(id=ns("tut_scale")),
        br(),
        
        # Confident Intervals option
        checkboxInput(inputId = ns("confinter"), label = "Plot confident intervals?", 
                      value = F) %>% div(id=ns("tut_confinter")),
        uiOutput( ns( "conditional_confinter")), 
        
        # Download button
        # downloadButton( ns('download_weibullGenerator'), 'Download Plot')
        
        # Help button
        actionButton(ns("help"), "Step-by-step guide")
        
      ), fluid = T, position = "left" )
    
  )
}


generator_Server <- function(input, output, session){
  # 0. INPUT SLIDER CONDITIONS  -----------------------------
  # Number of failures > Num. points xa Update slider Input with correction
  observe({
    if(input$nFailures > input$npoints){
      updateSliderInput(session, "nFailures", value = input$npoints)
    }
  })
  
  # Add slider for the conf.inter if the check box is pressed
  output$conditional_confinter <- renderUI({
    ns <- session$ns
    if(input$confinter == T){
      sliderInput(ns("conflevel_generator"), animate = TRUE,
                  "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8)
    }
  })
  
  ## 1. Plots --------------------------------------------
  # How the data will be generated
  data.generator <- reactive({ 
    # CHECK INPUTS
    # Check inputs before calculation
    validate(
      need( try( input$nFailures < input$npoints + 1), 
            message = "You can not have more failures than the sample population! Correcting the values...") )
    
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
    dfa <- Abrem(df , col="black", pch=2)
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
    
    
    # # Adding confident intervals
    # if(input$confinter == T){
    #   subtitle <- paste0("Likelihood Ratio confidence level ",
    #                      input$conflevel_generator*100," %", "(the red line)")
    # }
    # 
    
    # Plot
    plot(weibull.generator(), 
         main=title, sub=subtitle, is.plot.legend=T,
         xlab="Time to Failure", 
         ylab="Occurrence CDF %")
  })
  
  output$legendPlot <- renderDataTable({
    source("./R/foo_abremLegend.R")
    # Here we would like thi tech. info legend
    #
    # Becouse weibull.generator will be cashed!
    # WeibullLegend(weibull.generator())
    
    
  }, options= list(paging = F, searching = FALSE) )
  
  output$beta <- renderValueBox(({
    # Shorten the variable name
    # browser()
    beta <- weibull.generator()$fit[[1]][["beta"]]
    beta <- signif(beta, 3)
    valueBox(
      value =  beta, 
      subtitle = HTML("&beta;:","Shape / Slope of the failure mode"), 
      icon = icon("calculator")
      )
  }))
  output$eta <- renderValueBox(({
    # Shorten the variable name
    # browser()
    eta <- weibull.generator()$fit[[1]][["eta"]]
    eta <- signif(eta, 4 )
    valueBox(
      value =  eta, 
      subtitle = HTML("&eta;:", "Scale / Characteristic life"), 
      icon = icon("calculator"))
  }))
  output$facts <- renderValueBox({
    
    # Shorten the variable name
    fit <- weibull.generator()$fit[[1]]
    # options <- fit$options
    
    # Significant digits
    si <- function(number) signif(number, 3)
    
    # Select the icon 
    icon.fit <- ifelse(fit$gof$r2 >= fit$gof$ccc2, "thumbs-up", "thumbs-down")
    subtitle.fit <- paste0("r^2 | CCC^2 = ", si(fit$gof$r2), " | ", si(fit$gof$ccc2),
                           ifelse(fit$gof$r2 >= fit$gof$ccc2, " (good)", " (BAD)"))
    # subtitle.fit <- "_"
    
    valueBox(
      value = ifelse(fit$gof$r2 >= fit$gof$ccc2, "Good fit", "Bad fit"), 
      subtitle = subtitle.fit,
      icon =  icon(icon.fit, lib = "glyphicon"),
      color = ifelse(fit$gof$r2 >= fit$gof$ccc2, "green", "orange")
    )
    
    # valueBox(
    #   value = paste0("r^2 | CCC^2 = ", 
    #                  si(fit$gof$r2), " | ", si(fit$gof$ccc2), 
    #                  ifelse(fit$gof$r2 >= fit$gof$ccc2, " (good)", 
    #                         " (BAD)")), 
    #   subtitle = ifelse(fit$gof$r2 >= fit$gof$ccc2, " (good)", " (BAD)"),
    #   icon = ifelse(fit$gof$r2 >= fit$gof$ccc2, 
    #                 icon("thumbs-up", lib = "glyphicon"), 
    #                 icon("thumbs-down", lib = "glyphicon")),
    #   color = ifelse(fit$gof$r2 >= fit$gof$ccc2, "green", "red")
    #   )
  })
  
  output$contour_plot <- renderPlot({
    # If there is confident intervals plot the contour plot
    # otherwise just a plot with a dot
    
    if(input$confinter == T){
      levels.confidence <- function(weibull.data, conf.levels= c(.5, .8, .95) ){
        levels.calc <- as.list(conf.levels)
        dfa <- lapply(levels.calc, FUN = function(X){
          abrem.conf(weibull.data, method.conf.blives="lrb", cl=X,
                     unrel.n=25, S=10000, is.plot.cb=T,  in.legend.blives=F, 
                     lty=1, lwd=3, col="red") })
        return(dfa)
        
      }
      list.abrems.confidence <- levels.confidence(weibull.generator())
      
      list.data.plot <- lapply(list.abrems.confidence, function(X){
        data.frame(
          eta = X$fit[[1]]$conf$blives[[2]]$MLEXContour[[1]]$Eta, 
          beta = X$fit[[1]]$conf$blives[[2]]$MLEXContour[[1]]$Beta, 
          Plevel =  paste0("P", X$fit[[1]]$conf$blives[[2]]$options$cl*100) 
        )
      })
      
      # Change list to a data frame
      df.plot <- data.table::rbindlist(list.data.plot)
      
      # Get the dot
      df.dot <- data.frame(
        eta= weibull.generator()$fit[[1]]$eta, 
        beta= weibull.generator()$fit[[1]]$beta)
      
      # Construct the plot
      ggplot() +
       geom_polygon(data=df.plot, aes(x = eta,y = beta,fill = Plevel, color=Plevel),alpha = .05, lty=5) + 
       geom_point(data = df.dot, aes(x=eta, y=beta), lwd=2, color='red')
    
    }else{
      df.dot <- data.frame(
        eta= weibull.generator()$fit[[1]]$eta, 
        beta= weibull.generator()$fit[[1]]$beta
      )
      v <- ggplot(df.dot, aes(x=eta, y=beta))
      v + geom_point(lwd=2,color='red')
    }
  })
  
  ## 2. The help function
  
  steps <- readxl::read_excel(path = "./text/tutorials/Tutoriales_introJS.xlsx", 
                              sheet = "page_generator")
  
  # Initiate hints on startup with custom button and event
  # hintjs(session, options = list("hintButtonLabel"="Hope this hint was helpful"),
  #        events = list("onhintclose"='alert("Wasn\'t that hint helpful")'))
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,{
               introjs(session, options = list(steps= steps,
                                               "nextLabel"="Next",
                                               "prevLabel"="Previous",
                                               "skipLabel"="End"))
    })
  
  ## 3. Download the plot ?
  # output$download_weibullGenerator <- downloadHandler(
  #   filename = "Plot_Weibull_generated.pdf",
  #   content = function(file) {
  #     observe(data.toWeibull())
  #     dev.off()
  #   }
  # )
  
}
