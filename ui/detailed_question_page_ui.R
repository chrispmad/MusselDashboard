# query choices
query_field = c("total_inspections","blowbys","highrisk","musselfouled")
query_period = c("hour","day","week","month","year")
query_op = c("average","total","max","min")

# query input names
query_inputs = c('qit','qp','qst')

# query labels
query_prompts = c("Type of Inspection","Time Period", "Summary Type")

queries_tbl = tidyr::tibble(
  inputs = query_inputs,
  prompts = query_prompts,
  choices = list(query_field,query_period,query_op)
) |> 
  dplyr::group_by(inputs) |> 
  dplyr::group_split()

query_bits = sidebar(
  queries_tbl |> 
  lapply(\(x){
    selectInput(x$inputs, x$prompts, choices = x$choices[[1]])
  })
)

detailedQuestionUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      plotlyOutput('det_q_plotly'),
      sidebar = query_bits
    )
  )
}

detailedQuestionServer <- function(id, dat) {
  moduleServer(
    id,
    function(input, output, session) {
      dat_f = reactive({
        dat |> 
          dplyr::select(input$qit, TimeOfInspection)
      })
      
      observe({
        print(dat_f())
      })
    }
  )
}

detailed_question_page = nav_panel(
  title = "Detailed Question",
  # detailedQuestionUI('dq')
)