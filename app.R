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
                menuItem("Intro", tabName = "intro", icon = icon("info"), selected = T), 
                menuItem("Generator", tabName = "generator", icon = icon("dashboard"), selected = F )
    ), 
    ## 1. Generator  ----
    conditionalPanel(
      condition = "input.menu1 == 'generator'",
      sliderInput("npoints",
                  "Total sample", min = 5, max = 100 , value = 50, step=1), 
      sliderInput("nFailures",
                  "Number of failures", min = 1, max = 100, value = 21, step=1), 
      sliderInput("shape",
                  "Shape", min = 0.1, max = 7, value = 3,  step = 0.1), 
      sliderInput("scale",
                  "Scale", min = 10, max = 500, value = 100, step = 50), 
      sliderInput("conflevel_generator", animate = TRUE,
                  "Level confidence Interval", min = 0.5,  max = 0.99, step= 0.05, value = 0.8)
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
              fluidRow(
                box(title = "Weibull plot generator", height = 750,  
                    status="info", solidHeader = T, collapsible=F,  
                    plotOutput("logplot") ),
                #               box(title = "Density plot",  status = "info",solidHeader = TRUE,  collapsible=TRUE,
                #                   ggvisOutput("ggDensity"), uiOutput("ggDensity_ui"))
                tabBox(
                  title =  tagList(shiny::icon("line-chart"), "Other plots"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = 650 #,
                  #  tabPanel("Density", ggvisOutput("ggDensity"), uiOutput("ggDensity_ui") ),
                  #  tabPanel("Histogram", ggvisOutput("ggHistogram"), uiOutput("ggHistogram_ui"))
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
  output$logplot <- renderPlot({
    # Aesthetics
    title <- ""
    subtitle <- paste0("Likelihood Ratio confidence level ", 
                       input$conflevel_generator*100," %", "(the red line)")
    
    # Data greneration 
    df <- data.generator()
    
    # Fit abrem 
    dfa <- Abrem(df, col="black", pch=2)
    dfa <- abrem.fit(dfa, col="blue")
    dfa <- abrem.conf(dfa, method.conf.blives="lrb", cl=input$conflevel_generator, 
                      unrel.n=25, 
                      lty=1, lwd=3, col="red")
    
    # Plot
    plot(dfa, main=title, sub=subtitle,is.plot.legend=T,
         xlab="Time to Failure", 
         ylab="Occurrence CDF %")
  } , height = 650) 

}

### Bind the app together ----
shinyApp(ui, server)