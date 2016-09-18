library(ggplot2)
library(dplyr)
library(shinydashboard)

theme_set(theme_minimal())
source("../R/module_UploadData.R")

## Sidebar content ---------------------------
sidebar <- function(){
  dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Debug Name", tabName = "debug", icon = icon("info"), selected = T)  
                )
  )
}
### Body content --------------------------
body <- function(){
  dashboardBody(
    tabItems(
      ## Intro tab content ----
      tabItem(tabName = "debug",      
              uploadData_UI("uploadData"), 
              verbatimTextOutput("summary"))
    )
  )
}

## Bind ui together ----
ui <- dashboardPage(
  dashboardHeader(title = "R dashboard"),
  sidebar(),
  body())

### SERVER PART ----
## Server foo
server <- function(input, output, session){
  
  dataframe <- callModule(uploadData_server, "uploadData")
  
  output$summary <- renderPrint({
    summary(dataframe())
    
  })
}

### Bind the app together ----
shinyApp(ui, server)