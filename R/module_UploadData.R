uploadData_UI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      mainPanel(
        # Stuff in the main panel
        # dataTableOutput(ns('tablecsv') )
        dataTableOutput(ns("test") )
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
  
  # Calculations with output$whatever <- 
  inFile <- reactive({
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  inFileFormated <- reactive({
    # inFile formated
    if (is.null(inFile))
      return(NULL)
    read.csv(file = inFile()$datapath, header=input$header, 
             sep=input$sep, quote=input$quote) 
  })
  
  output$test <- renderDataTable({ 
    if (is.null(inFile))
      return(NULL)
    inFileFormated()
    
    })
  
  output$downloadData <- downloadHandler(
    filename = "Weibull_template.csv", content = function(file){
      write.csv(x = read.csv("./data/sample_df.csv"), row.names = F, file)
    },
    contentType = "text/csv"
  )
  
  
  
}