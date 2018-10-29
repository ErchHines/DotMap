

navbarPage("Block level dot maps for US Census", id="nav", 
  
  tabPanel("Dot Density Map",
    div(class="outer",
        
        tags$head(
          # Include the custom CSS
          includeCSS("styles.css")
        ),
        
        leafletOutput("map", width="100%", height="100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 100, left = "auto", right = 0, bottom = "auto",
                      width = 450, height = "auto",
                      
                      selectInput(
                        "State", "Select a State below:", state_counties$State, 
                        multiple = FALSE, selected = 1
                      ),
                      
                      selectInput(
                        "inSelect", "Select a County", ""
                      )
        )
    )
  )
)
      
                      
                      
                      
  
                        
                        