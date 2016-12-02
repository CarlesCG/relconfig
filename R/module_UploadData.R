library(rintrojs)

uploadData_UI <- function(id){
  ns <- NS(id)
  tagList(
    # Tutorial intro.js
    introjsUI(),
    
    # Panel with the data uploaded
    sidebarLayout(
      mainPanel(
        box(title = "Data Uploaded", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            dataTableOutput(ns("test") ) )
      ),
      
      # Options to upload the data
      sidebarPanel(
        id=ns("sidebar"), 
        
        # Stuff in the side bar
        # Calling the Upload file Module
        fileInput( ns('file'), 'Choose CSV File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv')) %>% 
          div(id = ns("tut_choosefile")),
        
        tags$hr(),
        checkboxInput( ns('header'), 'Header', TRUE) %>% 
          div(id = ns("tut_header")),
        
        radioButtons( ns('sep'), 'Separator',
                      c(Comma=',', Semicolon=';', Tab='\t'),
                      ',') %>% 
          div(id = ns("tut_separator")),
        radioButtons(ns( 'quote' ), 'Quote',
                     c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                     '"') %>% 
          div(id = ns("tut_quote")),
        
        ## Esto moverlo a otr side bar panel
        h4("Need a template?"),
        downloadButton(ns('downloadData'), 'Download'), 
        
        # Help button
        br(),
        br(),
        actionButton(ns("help"), "Guide")
      )
    )
  )
}


uploadData_server <- function(input, output, session){
  
  # Upload a csv file  
  userFile  <- reactive({
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # Pass the data as data frame
  dataframe <- reactive({
    # inFile formated
    if (is.null(userFile ))
      return(NULL)
    read.csv(file = userFile ()$datapath, header=input$header, 
             sep=input$sep, quote=input$quote) 
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  output$test <- renderDataTable({ 
    if (is.null(userFile ))
      return(NULL)
    dataframe()
    
    })
  
  output$downloadData <- downloadHandler(
    filename = "Weibull_template.csv", content = function(file){
      write.csv(x = read.csv("./data/sample_df.csv"), row.names = F, file)
    },
    contentType = "text/csv"
  )
  
  ## 2. The help function
  steps <- readxl::read_excel("./text/tutorials/Tutoriales_introJS.xls", sheet = "page_uploadData")
  
  # start introjs when button is pressed with custom options and events
  observeEvent(input$help,{
    introjs(session, options = list(steps= steps,
                                    "nextLabel"="Next",
                                    "prevLabel"="Previous",
                                    "skipLabel"="End"))
  })
  
  
  return(dataframe)
  
}