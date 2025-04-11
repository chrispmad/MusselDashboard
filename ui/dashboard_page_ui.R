year_picker_bit = pickerInput('year_filt',
                              'Year Filter',
                              multiple = T,
                              choices = c(2015:2023), 
                              selected = c(2015:2023),
                              options = list(
                                title = 'Year Filter',
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 3"
                              )
)

month_picker_bit = pickerInput('month_filt',
                               'Month Filter',
                               multiple = T,
                               choices = month.abb,
                               selected = month.abb,
                               options = list(
                                 title = 'Month Filter',
                                 `actions-box` = TRUE,
                                 `selected-text-format` = "count > 3"
                               )
)

station_picker_bit = uiOutput('stat_filt_ui')

q_var_picker = selectInput('q_var',"Type of Inspection",choices = c("total_inspections","highrisk","musselfouled"), selected = 'total_inspections')
q_period_picker = selectInput('q_per',"Time Period",choices = c("hour","day","week","month","year","all time"), selected = "all time")
q_wday_picker = shinyWidgets::pickerInput('q_wday',"Days to Include",choices = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), selected = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), multiple = TRUE)
q_op_picker = selectInput('q_op',"Summary Type",choices = c("average","total","max","min"), selected = "total")
q_date_picker = shinyWidgets::airDatepickerInput(
  inputId = "q_date",
  label = "Specific Dates",
  value = NA,
  multiple = TRUE,
  clearButton = TRUE, 
  width = "100%"
)


filters_accordion = bslib::accordion(
  class = "filt-acc",
  accordion_panel(
    "Filters",
    year_picker_bit,
    # month_picker_bit,
    station_picker_bit,
    q_var_picker,
    q_wday_picker,
    q_date_picker
  ),
  accordion_panel(
    "Summary Options",
    q_period_picker,
    q_op_picker
  ),
  open = "Filters"
)

sum_stat_col = card(
  layout_column_wrap(
    width = 1/2,
    card(
      h6("Total Inspections: "),
      div(class = 'summary_number', textOutput('tot_insp', inline = T)),
      style = 'background:#88e3a0;'
    ),
    card(
      h6("High-risk Inspections: "),
      div(class = 'summary_number', textOutput('hr_insp', inline = T)),
      style = 'background:#88a3e3;'
    )
  ),
  layout_column_wrap(
    width = 1/2,
    card(
      h6("Mussel-fouled Inspections: "),
      div(class = 'summary_number', textOutput('mf_insp', inline = T)),
      style = 'background:#e66572;'
    ),
    card(
      h6("Quarantine Periods Issued: "),
      div(class = 'summary_number', textOutput('quar_insp', inline = T)),
      style = 'background:#d265e6;'
    )
  ),
  class = 'sum-stat-card'
)

the_sidebar = sidebar(
  width = '30%',
  sum_stat_col,
  filters_accordion
)

chart_col = card(
  plotlyOutput('plotly_chart')
)

dashboard_page = nav_panel(
  title = "Dashboard",
  layout_sidebar(
    layout_column_wrap(
      width = 1/2,
      card(
        leafletOutput('my_map')
      ),
      chart_col,
      style = 'margin-top:-2rem;'
    ),
    sidebar = the_sidebar
  )
)