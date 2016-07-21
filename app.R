#####################
## Reliability App ##
#####################

### Libraries -----
library(shinydashboard)
library(shiny)
# library(abrem) # Is this needed here?
# library(ggvis) # Whenever doing ggvis, not yet...

### UI PART ----

## Sidebar content ---------------------------
sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Intro", tabName = "intro", icon = icon("info"), selected = F), 
                menuItem("Generator", tabName = "generator", icon = icon("dashboard"), selected = T )
    )
    )
}
### Body content --------------------------
body <- function() {
  dashboardBody(
    tabItems(
      ## Intro tab content ----
      tabItem(tabName = "intro",
              includeMarkdown("./text/intro_text.md")
      ),
      tabItem(tabName = "generator",
              sidebarLayout(
                sidebarPanel(
                  width = 2, tabName="Variables", 
                  sliderInput("npoints",
                              "Total sample", min = 5, max = 100 , value = 50, step=1), 
                  sliderInput("nFailures",
                              "Number of failures", min = 1, max = 100, value = 21, step=1), 
                  sliderInput("shape",
                              "Shape", min = 0.1, max = 7, value = 3,  step = 0.1), 
                  sliderInput("scale",
                              "Scale", min = 10, max = 500, value = 100, step = 50), 
                  br(),
                  # Confident Intervals option
                  checkboxInput(inputId = "confinter", label = "Plot confident intervals?", value = F),
                  uiOutput("conditional_confinter"),
                  
                  # Download button
                  downloadButton('download_weibullGenerator', 'Download Plot')
                ), 
                mainPanel(      
                  box(title = "Weibull plot generator",  height = 750,  
                      status="info", solidHeader = T, collapsible=F,  
                      plotOutput("logplot") )
                  
                )
              )
      )
    )
  )
}
## Bind ui together ----
ui <- dashboardPage(
  dashboardHeader(title = "Reliability dashboard"),
  sidebar(),
  body())

### SERVER PART ----
library(markdown)
library(abrem)
library(ggplot2)
library(ggvis)
library(dplyr)
source("./R/foo_make_abrem_plot.R")

## Server foo
server <- function(input, output, session){
  
  # 0. Censored and Uncensored
  # Number of failures update slider Input with correction
  observe({
    if(input$nFailures > input$npoints){
      updateSliderInput(session, "nFailures", value = input$npoints)
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
  output$conditional_confinter <- renderUI({
    if(input$confinter == T){
      sliderInput("conflevel_generator", animate = TRUE,
                  "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8) 
    }
  })
  output$logplot <- renderPlot({
    # Aesthetics
    title <- ""
    subtitle <- ""
    # Data greneration 
    df <- data.generator()
    
    # Fit abrem 
    dfa <- Abrem(df, col="black", pch=2)
    dfa <- abrem.fit(dfa, col="blue")
    
    # Adding confident intervals
    if(input$confinter == T){
      subtitle <- paste0("Likelihood Ratio confidence level ", 
                         input$conflevel_generator*100," %", "(the red line)")
    
      dfa <- abrem.conf(dfa, method.conf.blives="lrb", cl=input$conflevel_generator, 
                        unrel.n=25, 
                        lty=1, lwd=3, col="red")
    }
  
    
    # Plot
    plot(dfa, main=title, sub=subtitle, is.plot.legend=T,
         xlab="Time to Failure", 
         ylab="Occurrence CDF %")
  } , height = 650) 
  output$download_weibullGenerator <- downloadHandler(
    filename = "Plot_Weibull_generated.pdf",
    content = function(file) {
      observe(data.toWeibull()
      dev.off()
    }
  )
  
  

}

### Bind the app together ----
shinyApp(ui, server)