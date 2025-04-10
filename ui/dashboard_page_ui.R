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

sum_stat_col = card(
  card(
    h6("Total Inspections: "),
    div(class = 'summary_number', textOutput('tot_insp', inline = T)),
    style = 'background:#88e3a0;font-weight:bold;'
  ),
  card(
    h6("High-risk Inspections: "),
    div(class = 'summary_number', textOutput('hr_insp', inline = T)),
    style = 'background:#88a3e3;font-weight:bold'
  ),
  card(
    h6("Mussel-fouled Inspections: "),
    div(class = 'summary_number', textOutput('mf_insp', inline = T)),
    style = 'background:#e66572;font-weight:bold'
  ),
  card(
    h6("Quarantine Periods Issued: "),
    div(class = 'summary_number', textOutput('quar_insp', inline = T)),
    style = 'background:#d265e6;font-weight:bold'
  )
)

chart_col = card(
  plotlyOutput('plotly_chart')
)

dashboard_page = nav_panel(
  title = "Dashboard",
  fluidRow(
    # class = 'picker_inputs_section',
    column(width = 4,
           year_picker_bit
    ),
    column(width = 4,
           month_picker_bit
    ),
    column(width = 4,
           uiOutput('stat_filt_ui')
    )
  ),
  layout_column_wrap(
    width = 1/3,
    card(
      leafletOutput('my_map')
    ),
    sum_stat_col,
    chart_col,
    style = 'margin-top:-2rem;'
  )
)