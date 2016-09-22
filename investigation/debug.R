mainPanel(
  box(title = "Failure test plot", width = 12,# height = 820, 
      status="info", solidHeader = T, collapsible=T,  
      plotOutput( ns("fixComponents_plot") ) ), 
  box(title = "Time needed without failure to pass test", width = 6,# height = 820, 
      status="info", solidHeader = T, collapsible=F,  
      verbatimTextOutput( ns( "fixComponents_result") ) ), 
  box(title = "characteristic life multiplyer (k)", width = 6,# height = 820, 
      status="info", solidHeader = T, collapsible=F,  
      verbatimTextOutput( ns( "lifeMultiplyer") ) )
)