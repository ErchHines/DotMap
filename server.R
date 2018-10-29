

function(input, output, session){
  
  observe({
    x <- input$State
    
    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      choices = state_counties$County[state_counties$State == input$State]
    )
  })
  
  output$map <- renderLeaflet({
    get_P9(input$inSelect, input$State)
  })
}