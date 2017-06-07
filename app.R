#####################
## Reliability App ##
#####################

### Libraries -----
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)

options(shiny.maxRequestSize=30*1024^2)
ggplot2::theme_set(theme_minimal())  


# Source modules
# The code in ui.R is run once, when the Shiny app is started and it generates an HTML 
# file which is cached and sent to each web browser that connects.
# This is a candidate to go the global.R
# Caveat: Since I'm using functions to create the UI, functions inside global.R
# will not be available in that enviroment. Need to source
source("./global.R")
source("./R/module_generator.R")
source("./R/module_UploadData.R")
source("./R/module_WeibullCalculate.R")
source("./R/module_Time_testing_complete.R")
source("./R/module_Time_testing_partial.R")
source("./R/module_Failure_forecast_generator.R")

### UI PART ----

## Sidebar content ---------------------------
sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Intro", tabName = "intro", icon = icon("info"),                                         selected = F), 
                menuItem("Weibull generator", tabName = "generator", icon = icon("dashboard"),                    selected = T), 
                menuItem("Weibull Modeler", tabName = "modeler", icon = icon("bar-chart"),                        selected = F, 
                          menuSubItem("Upload data", icon = icon("gears"),tabName = "uploadData",                 selected = F),
                          menuSubItem("Calculate", icon = icon("check-circle"), tabName = "calculateModel",       selected = F) 
                         ), 
                menuItem("Zero Failure testing", tabName = "Ztest", icon = icon("flash", lib='glyphicon'),             selected = F, 
                         menuSubItem("Time testing complete", icon = icon("time", lib = "glyphicon"),tabName = "ttesting" ),
                         menuSubItem("Time testing partial", icon = icon("gears"), tabName = "Ntesting", selected = F) 
                         ), 
                menuItem("Forecast Modeler", icon = icon("line-chart"),
                         menuSubItem("Model generator", icon = icon("gears"),tabName = "forecast_generator", selected = F),
                         menuSubItem("Train Models", icon = icon("gears"),tabName = "trainModels"),
                         menuSubItem("Compare Models", icon = icon("check-circle"), tabName = "compareModels")
                         ), 
                menuItem("Help", tabName = "help", icon = icon("question-circle"),
                         menuSubItem("About", icon = icon("user"),tabName = "helpAbout"),
                         menuSubItem("Welcome", icon = icon("coffee"),tabName = "helpWelcome") 
                         )
    )
  )
}

### Body content --------------------------
body <- function(){
  dashboardBody(
    RdynamicsHeader(), 
    tabItems(
      ## Intro tab content ----
      tabItem(tabName = "intro",          includeMarkdown("./text/intro_text.md") ),
      tabItem(tabName = "generator",      generator_UI("page_generator") ), 
      tabItem(tabName = "uploadData",     uploadData_UI("page_uploadData") ), 
      tabItem(tabName = "calculateModel", weibullCalculate_UI("page_calculate") ), 
      tabItem(tabName = "ttesting",       zeroFailure_test_UI("page_ttest")),
      tabItem(tabName = "Ntesting",       Zerofailure_fix_time_UI("page_Ntest")), 
      tabItem(tabName = "forecast_generator", FailureForecast_generator_UI("page_forecastGen")), 
      tabItem(tabName = "helpAbout",           includeMarkdown("./text/text_comingSoon.md"))
      )
    )
}

header <- function(){
  dashboardHeader(title = "Life Data Analysis", 
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = " "),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2017-05-14")
                  ), 
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 16%",
                                 icon = icon("exclamation-triangle"),
                                 status = "success"
                               )
                  ), 
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
  )
}

## Bind ui together ----
ui <- dashboardPage(
  header(),
  sidebar(),
  body())

### SERVER PART ----
library(markdown)
# source("./R/foo_make_abrem_plot.R")

## Server foo
server <- function(input, output, session){
  
                callModule(generator_Server,        "page_generator")
  data  <-      callModule(uploadData_server,       "page_uploadData")
                callModule(weibullCalculate_server, "page_calculate", data)
                callModule(zeroFailure_test_server, "page_ttest")
                callModule(Zerofailure_fix_time_server, "page_Ntest")
                callModule(FailureForecast_generator_server, "page_forecastGen")
}

### Bind the app together ----
shinyApp(ui, server)