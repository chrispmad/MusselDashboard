library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(stringr)
library(dplyr)
library(shinyWidgets)

source('ui/dashboard_page_ui.R')
source('ui/theming.R')

ui <- page_navbar(
  # tags$head(tags$script(src="textInput_dropdown.js")),
  theme = mussel_theme,
  title = h5("BC IMDP"),
  window_title = 'ZebraQuagga Mussel Dashboard',
  dashboard_page,
  selected = 'Dashboard'
)

server <- function(input, output, session) {
  
  cur_wd = getwd()
  
  if(!str_detect(cur_wd, '/www(/)?$')){
    setwd(paste0(cur_wd, '/www/'))
  }
  
  source('server/startup_script.R')
  
  # Apply data filters (year, month, station)
  dat_date_filtered = reactive({
    dat |>
      filter(Year %in% input$year_filt) |>
      mutate(Month = lubridate::month(Month, label=T)) |>
      filter(Month %in% input$month_filt)
  })
  
  dat_station_filtered = reactive({
    req(length(input$stat_filt) > 0)
    dat_date_filtered() |>
      filter(Station %in% input$stat_filt)
  })
  
  stations_with_dat = reactive({
    req(dat_station_filtered())
    
    stat_dat_summ = dat_station_filtered() |>
      dplyr::mutate(across(c("Musselfouled","Highrisk"), \(x) tidyr::replace_na(x, FALSE))) |>
      dplyr::filter(!is.na(Year)) |>
      dplyr::group_by(Year, Station, Musselfouled, Highrisk) |>
      dplyr::summarise(number_records = sum(n))
    
    stations |>
      dplyr::left_join(stat_dat_summ)
  })
  
  stations_for_map = reactive({
    req(stations_with_dat())
    
    stations_with_dat() |> 
      dplyr::select(-lat,-lon) |> 
      dplyr::filter(map_label %in% input$stat_filt) |> 
      group_by(map_label,hours_of_operation,
               station_type) |> 
      summarise(Musselfouled = sum(Musselfouled,na.rm=T),
                Highrisk = sum(Highrisk, na.rm=T),
                number_records = sum(number_records, na.rm=T)) |> 
      ungroup()
  })
  
  # Render user interface for reactive filters.
  output$stat_filt_ui = renderUI({
    pickerInput('stat_filt',
                'Station Filter',
                multiple = T,
                choices = unique(dat_date_filtered()$Station)[order(unique(dat_date_filtered()$Station))],
                selected = unique(dat_date_filtered()$Station),
                options = list(
                  title = 'Station Filter',
                  `actions-box` = TRUE,
                  `selected-text-format` = "count > 4"
                )
    )
  })
  
  # Generate summary numbers
  output$tot_insp = renderText({
    paste0(sum(dat_station_filtered()$n)
    )
  })
  
  output$hr_insp = renderText({
    req(dat_station_filtered())
    
    paste0(dat_station_filtered() |>
             dplyr::filter(Highrisk == T) |>
             dplyr::summarise(total = sum(n)) |>
             dplyr::pull(total)
    )
  })
  
  output$mf_insp = renderText({
    req(dat_station_filtered())
    
    paste0(dat_station_filtered() |>
             dplyr::filter(Musselfouled == T) |>
             dplyr::summarise(total = sum(n)) |>
             dplyr::pull(total)
    )
  })
  
  
  output$quar_insp = renderText({
    req(dat_station_filtered())
    
    paste0(dat_station_filtered() |>
             dplyr::filter(quarantine_issued == T) |>
             dplyr::summarise(total = sum(n)) |>
             dplyr::pull(total)
    )
  })
  
  
  # Generate Leaflet map
  output$my_map = renderLeaflet({
    leaflet::leaflet(
      options = leafletOptions(zoomControl = FALSE))  |> 
      htmlwidgets::onRender("function(el, x) {
    L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }") |>
      leaflet::addProviderTiles(provider = leaflet::providers$CartoDB) |>
      leaflet::addPolygons(
        opacity = 0,
        fillOpacity = 0,
        data = bc_boundary
      ) |>
      setView(lng = bc_shape_centroid$X,
              lat = bc_shape_centroid$Y - 3,
              zoom = bc_shape_centroid$zoom)
  })
  
  # stations_labels = reactive({
  #   req(stations_with_dat())
  # })
  color_pal = reactive({
    req(!is.null(stations_for_map()))
    leaflet::colorBin(palette = 'Spectral',
                      domain = stations_for_map()$number_records)
  })
  
  leaflet_popup = reactive({
    req(!is.null(stations_for_map()))
    leafpop::popupTable(
      stations_for_map() |> 
        sf::st_drop_geometry() |> 
        group_by(map_label,hours_of_operation,
                 station_type) |> 
        summarise(Musselfouled = sum(Musselfouled,na.rm=T),
                  Highrisk = sum(Highrisk, na.rm=T),
                  number_records = sum(number_records, na.rm=T)) |> 
        ungroup()
    )
  })
  
  observe({
    req(!is.null(stations_for_map()))
    
    m = leafletProxy('my_map') |>
      clearGroup('stations') |> 
      addCircleMarkers(
        label = lapply(leaflet_popup(),
                       htmltools::HTML),
        color = 'black',
        radius = 4,
        weight = 2,
        fillColor = ~color_pal()(number_records),
        data = stations_for_map(),
        group = 'stations'
      ) |> 
      leaflet::removeControl(layerId = 'legend')
    
    if(sum(stations_for_map()$number_records) > 0){
      m = m |> 
        addLegend(pal = color_pal(),
                  values = stations_for_map()$number_records,
                  position = 'bottomright',
                  layerId = 'legend')
    }
    m
  })
  
  output$plotly_chart = renderPlotly({
    
    req(dat_station_filtered())
    
    plot_dat = dat_station_filtered() |>
      dplyr::group_by(Station) |>
      dplyr::reframe(number_inspections = sum(n)) |>
      dplyr::arrange(desc(number_inspections)) |>
      dplyr::mutate(Station = forcats::fct_lump_n(Station, n = 3, w = number_inspections)) |>
      dplyr::mutate(Station = forcats::fct_rev(Station)) |>
      dplyr::group_by(Station) |>
      dplyr::reframe(number_inspections = sum(number_inspections))
    
    # new_factor_levels = plot_dat |>
    #   dplyr::filter(station != 'Other') |>
    #   dplyr::arrange(desc(number_inspections)) |>
    #   dplyr::pull(station)
    # 
    # plot_dat_new_levels = plot_dat |>
    #   dplyr::mutate(station = factor(station, levels = c(new_factor_levels, 'Other')))
    #
    
    my_ggplot = plot_dat |>
      ggplot2::ggplot(aes(x = number_inspections, y = Station)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = 'Number of Inspections',
                    y = 'Station Name') +
      ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3, sep = ""),
                                  breaks = scales::breaks_extended(n = 5)) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(my_ggplot)
  })
  
  # ======================================
  # Data Exploration Tab
  
  x_axis_var_r = reactiveVal('Year')
  filter_var_r = reactiveVal('All')
  
  # output$x_axis_chosen = renderText({
  #   input$x_axis_var
  #   })
  
  observeEvent(input$x_axis_var, {
    # req(!is.null(input$x_axis_var))
    x_axis_var_r(input$x_axis_var)
  })
  observeEvent(input$condition_1, {
    # req(!is.null(input$data_expl_filter))
    filter_var_r(input$condition_1)
  })
  
  # output$filter_chosen = renderText(input$data_expl_filter)
  
  output$data_expl_plot = renderPlotly({
    
    # req(!is.null(input$x_axis_var))
    # req(!is.null(input$data_expl_filter))
    # x_axis_var_r = input$x_axis_var
    # filter_var_r = input$data_expl_filter
    
    if(filter_var_r() == 'All'){
      plot_dat = dat |> 
        dplyr::count(!!sym(x_axis_var_r())) |> 
        mutate(x_var = as.factor(!!sym(x_axis_var_r()))) |> 
        dplyr::as_tibble()
    } else {
      plot_dat = dat |> 
        dplyr::filter(!!sym(filter_var_r())) |> 
        dplyr::count(!!sym(x_axis_var_r())) |> 
        mutate(x_var = as.factor(!!sym(x_axis_var_r()))) |> 
        dplyr::as_tibble()
    }
    
    if(nrow(plot_dat) >= 10){
      plot_dat = plot_dat |> 
        mutate(x_var = forcats::fct_lump(x_var, n = 5, w = n)) |> 
        group_by(x_var) |> 
        summarise(n = sum(n,na.rm=T))
    }
    
    expl_plot = plot_dat |> 
      ggplot2::ggplot() + 
      geom_col(aes(x = x_var,
                   y = n,
                   fill = x_var)) + 
      labs(y = paste0('Number of Inspections (',filter_var_r(),")"),
           x = stringr::str_to_title(x_axis_var_r())) + 
      theme_minimal() + 
      theme(legend.position = 'none')
    
    # Any very long X-axis labels? If so, flip axes of ggplot.
    if(sum(stringr::str_length(plot_dat$x_var) > 20) > 0){
      expl_plot = expl_plot + 
        coord_flip()
    }
    
    plotly::ggplotly(
      expl_plot,
      tooltip = c("y")
    )
  })
  
  # output$exploration_plot = renderPlot({
  #   
  #   plot_dat = dat_station_filtered() |>
  #     dplyr::group_by(!!sym(x_axis_var)) |>
  #     dplyr::reframe(number_inspections = sum(n)) |>
  #     dplyr::arrange(desc(number_inspections)) |>
  #     dplyr::mutate(!!sym(x_axis_var) := forcats::fct_lump_n(!!sym(x_axis_var), n = 3, w = number_inspections)) |>
  #     dplyr::mutate(!!sym(x_axis_var) := forcats::fct_rev(!!sym(x_axis_var))) |>
  #     dplyr::group_by(!!sym(x_axis_var)) |>
  #     dplyr::reframe(number_inspections = sum(number_inspections))
  #   
  #   
  #   my_ggplot = plot_dat |>
  #     ggplot2::ggplot(aes(x = number_inspections, y = !!sym(x_axis_var))) +
  #     ggplot2::geom_col() +
  #     ggplot2::labs(x = 'Number of Inspections',
  #                   y = !!sym(x_axis_var)) +
  #     ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "K", scale = 1e-3, sep = ""),
  #                                 breaks = scales::breaks_extended(n = 5)) +
  #     ggplot2::theme_minimal()
  #   
  #   my_ggplot
  # })
  # observe({
  #   updateSelectInput(session,
  #                     'x_axis',
  #                     choices = names(dat))
  # })
}

shinyApp(ui, server)