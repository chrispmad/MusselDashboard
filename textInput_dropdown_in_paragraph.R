library(shiny)

intextInput = function(inputId, label = NULL, textColor='blue', choices = c('Arnold','cats',4), selected = 'A'){
  if(is.null(label)) stop("Label must not be NULL")
  
  # Collapse vector of options into a single string separated by commas.
  choices_as_str <- paste(
    choices,
    collapse = ',')
  
  # Output is a barely formatted chunk of HTML. But it works!
  HTML(paste0("<div id = '",inputId,"', choices = ",choices_as_str," label = ",label,"class = 'expandable-word', style = 'color:",textColor,";display:-webkit-inline-box;'>",choices[1],"</div>"))
}

ui <- fluidPage(
  tags$head(tags$script(src="textInput_dropdown.js")),
  
  h1("Expandable Word List Example"),
  
  fluidRow(
    column(width = 8, offset = 2,
           p("Hello, how are you doing? Here is ",style='display:inline;'),
           intextInput('test_two'),
           p(" but watch out!",style='display:inline;')
           ),
    verbatimTextOutput("selected_word_output")
  )
)

server <- function(input, output, session) {
  output$selected_word_output <- renderPrint({
    input$test_two
  })
}

shinyApp(ui, server)
