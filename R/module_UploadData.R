uploadData_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mainPanel(
        box(title = "Data Uploaded", width = 12,# height = 820, 
            status="info", solidHeader = T, collapsible=T,  
            dataTableOutput(ns("test") ) )
      ),
      sidebarPanel(
        # Stuff in the side bar
        # Calling the Upload file Module
        fileInput( ns('file'), 'Choose CSV File',
                   accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv')),
        
        tags$hr(),
        checkboxInput( ns('header'), 'Header', TRUE),
        radioButtons( ns('sep'), 'Separator',
                      c(Comma=',', Semicolon=';', Tab='\t'),
                      ','),
        radioButtons(ns( 'quote' ), 'Quote',
                     c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                     '"'),
        ## Esto moverlo a otr side bar panel
        h4("Need a template?"),
        downloadButton(ns('downloadData'), 'Download')
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
  
  return(dataframe)
  
}