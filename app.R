#####################
## Reliability App ##
#####################

### Libraries -----
library(shinydashboard)
library(shiny)
# library(abrem) # Is this needed here?
# library(ggvis) # Whenever doing ggvis, not yet...

# Source modules
source("./R/module_generator.R")


### UI PART ----

## Sidebar content ---------------------------
sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Intro", tabName = "intro", icon = icon("info"), selected = F), 
                menuItem("Weibull Generator", tabName = "generator", icon = icon("dashboard"), selected = T )
    )
    )
}
### Body content --------------------------
body <- function(){
  dashboardBody(
    tabItems(
      ## Intro tab content ----
      tabItem(tabName = "intro",
              includeMarkdown("./text/intro_text.md")
      ),
      tabItem(tabName = "generator",
              generator_UI("page_generator")
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
# source("./R/foo_make_abrem_plot.R")

## Server foo
server <- function(input, output, session){
  source("./R/module_generator.R")
  callModule(generator_Server, "page_generator")

}

### Bind the app together ----
shinyApp(ui, server)