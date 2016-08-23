#####################
## Reliability App ##
#####################

### Libraries -----
library(shinydashboard)
library(shiny)
# library(abrem) # Is this needed here?
# library(ggvis) # Whenever doing ggvis, not yet...

options(shiny.maxRequestSize=30*1024^2)

# Source modules
# The code in ui.R is run once, when the Shiny app is started and it generates an HTML 
# file which is cached and sent to each web browser that connects.
source("./R/module_generator.R")
source("./R/module_UploadData.R")
source("./R/module_WeibullCalculate.R")

### UI PART ----

## Sidebar content ---------------------------
sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Intro", tabName = "intro", icon = icon("info"), selected = F), 
                menuItem("Weibull generator", tabName = "generator", icon = icon("dashboard"), selected = F ), 
                menuItem("Weibull Modeler", tabName = "modeler", icon = icon("bar-chart"), selected = F, 
                          menuSubItem("Upload data", icon = icon("gears"),tabName = "uploadData", selected = T),
                          menuSubItem("Calculate", icon = icon("check-circle"), tabName = "calculateModel")), 
                menuItem("Forecast Modeler", icon = icon("line-chart"),
                         menuSubItem("Train Models", icon = icon("gears"),tabName = "trainModels"),
                         menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "compareModels")), 
                menuItem("Help", tabName = "help", icon = icon("question-circle"),
                         menuSubItem("About shinyHome", icon = icon("user"),tabName = "helpAbout"),
                         menuSubItem("Welcome", icon = icon("coffee"),tabName = "helpWelcome") )
    )
    )
}
### Body content --------------------------
body <- function(){
  dashboardBody(
    tabItems(
      ## Intro tab content ----
      tabItem(tabName = "intro",          includeMarkdown("./text/intro_text.md") ),
      tabItem(tabName = "generator",      generator_UI("page_generator") ), 
    # tabItem(tabName = "calculateModel", weibullCalculate_UI("page_calculate") ), 
      tabItem(tabName = "uploadData",     uploadData_UI("page_uploadData") )
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
# source("./R/foo_make_abrem_plot.R")

## Server foo
server <- function(input, output, session){
  source("./R/module_generator.R")
  source("./R/module_UploadData.R")
  source("./R/module_WeibullCalculate.R")
  
  callModule(generator_Server,        "page_generator")
  callModule(weibullCalculate_server, "page_calculate")
  callModule(uploadData_server,           "page_uploadData")

}

### Bind the app together ----
shinyApp(ui, server)