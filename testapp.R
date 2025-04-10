library(shiny)
source('utils/create_intext_dropdown_selector.R')
source('utils/selectParagraph.R')

# ui <- fluidPage(
#   tags$head(tags$script(src="textInput_dropdown.js")),
#   p("I want to explore how ",style='display:inline;'),
#   intextInput(inputId = 'condition_1', label = 'All', selected = 'All', choices = c("All","Mussel-fouled","High-Risk","Commercially-Hauled")),
#   p("inspections vary by ",style='display:inline;'),
#   intextInput(inputId = 'x_axis_var',label = 'Year', selected = 'Year', styleArgs = 'color: purple; background-color: lightblue; border-radius: 10%; padding: 2px', choices = c("Year","Station")),
#   p(".", style = 'display:inline;')
#   # p(", where ", style = 'display:inline;'),
#   # p(" inspections are included", style = 'display:inline;')
# )

ui <- fluidPage(
  tags$head(tags$script(src="textInput_dropdown.js")),
  selectParagraph(
    text = list("I want to explore how",
                "inspections vary by",
                "."),
    inputs = list(
      intextInput(inputId = 
                    'condition_1', 
                  label = 'All', 
                  selected = 'All', 
                  choices = c("All","Mussel-fouled","High-Risk","Commercially-Hauled")),
      intextInput(inputId = 'x_axis_var',
                  label = 'Year', 
                  selected = 'Year', 
                  choices = c("Year","Station"))
    )
  ))

server <- function(input, output, session) {
  output$choice = renderText({
    input$x_axis_var
  })
}

shinyApp(ui, server)