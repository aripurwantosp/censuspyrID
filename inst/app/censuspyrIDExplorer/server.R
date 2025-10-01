# server for censuspyrID explorer apps

# Define server logic required to draw a histogram
function(input, output, session) {

  # main ----
  sel_mode <- reactive({input$regmode})
  sel_reg <- reactive({input$select_reg})
  sel_exphase <- reactive({ifelse(input$expand_phase == 1, TRUE, FALSE)})
  sel_year <- reactive({as.numeric(input$select_year)})
  sel_smooth <- reactive({input$smoothopt})
  sel_col <- reactive({input$col_picker})

  ## selected data ----
  # global pop data
  pop_data <- reactive({
    load_pop_data(sel_mode(), sel_smooth())
  })

  # specific year pop data
  pop_data_year <- reactive({
    pop_data_by_year(pop_data(), sel_year())
  })

  # data for trends
  pop_data_trends <- reactive({
    pop_data_by_reg(pop_data(), sel_reg())
  })

  # single pop data: specific year and reg data
  pop_data_single <- reactive({
    pop_data_by_year(pop_data_trends(), sel_year())
  })


  ## sidebar for main parameter ----

  # region (province list) conditional on regmode
  output$reg_select <- renderUI({
    reg <- censuspyrID:::reg_list(harmonized = sel_mode())
    do.call("selectInput",
            list("select_reg", "", reg,
                 selected = reg[1], multiple = TRUE,
                 selectize = FALSE, size = 6))
  })

  # expanded phase option, conditional on harmonized==FALSE and expanded province
  cond_expand <- reactive({
    if (!"select_reg" %in% names(input)) return(FALSE)
    if (is.null(sel_reg)) return(FALSE)

    !sel_mode() && is_expanded(sel_reg())
  })

  output$expand_info <- renderText({
    if(cond_expand()){
      paste0('<small>', "The province has expanded, please select phase", '</small>')
    }
  })

  output$phase_expand <- renderUI({
    if(cond_expand()){
      do.call("radioButtons",
              list("expand_phase", h5("Expand phase:"),
              censuspyrID:::censuspyrID_data_env$expand_phase, selected = 1,
              inline = TRUE))
    }
  })

  # year list conditional on selected region (province)
  output$year_select <- renderUI({
    years <- year_range(sel_reg(), harmonized = sel_mode(),
                        before_expand = sel_exphase())

    do.call(shinyWidgets::sliderTextInput,
            list("select_year", h5("Census year:"), grid = TRUE,
                 force_edges = TRUE, choices = years))
  })


  # Pyramids tab ----
  ## text for census year ----
  year_sel_out <- reactive({
    census <- censuspyrID:::ref_label$census_label |>
      dplyr::filter(year == sel_year()) |>
      dplyr::pull(label)
    paste('Census year:', census)
    })
  output$pyr_year <- renderText(year_sel_out())

  ## plot ----
  sel_useprop <- reactive({input$use_prop})
  output$pyr_plot <- renderPlot({
    pyr_single(pop_data_single(), use_prop = sel_useprop(),
               color = sel_col())
  })

  ## summary ----
  output$pyr_summary <- renderPrint({
    pop_summary(pop_data_single())
  })


  # Age-profile tab ----
  ## text for census year ----
  output$ageprof_year <- renderText(year_sel_out())

  ## plot ----
  sel_logscale <- reactive({input$logscale})
  output$ageprof_plot <- renderPlot({
    ageprof(pop_data_single(), log_scale = sel_logscale(),
            color = sel_col())
  })


  # Trends tab ----

  ## plot ----
  sel_trends_type <- reactive({as.numeric(input$trends_type)})
  sel_trends_pyr_mode <- reactive({as.numeric(input$trends_pyr_mode)})
  sel_trends_useprop <- reactive({input$trends_use_prop})
  sel_trends_area_sex <- reactive({as.numeric(input$trends_area_sex)})
  output$trends_plot <- renderPlot({

    # years, conditional on expand phase if region has expanded
    years <- year_range(sel_reg(), harmonized = sel_mode(),
                        before_expand = sel_exphase())

    # filter data based on phase of expansion (before or after)
    data <- pop_data_trends() |>
      dplyr::filter(year %in% years)

    # plot using pyramid or area
    if(sel_trends_type() == 1){
      pyr_trends(data, mode = sel_trends_pyr_mode(),
                 use_prop = sel_trends_useprop(),
                 color = sel_col())
    }else{
      area_trends(data, sex = sel_trends_area_sex(),
                  color = sel_col())
    }

  })

  ## widget for plot ----
  # disable/enable widget based on viz type
  observe({
    if (sel_trends_type() == 2) {
      shinyjs::disable("trends_pyr_mode")
      shinyjs::disable("trends_use_prop")
      shinyjs::enable("trends_area_sex")
    } else {
      shinyjs::enable("trends_pyr_mode")
      shinyjs::enable("trends_use_prop")
      shinyjs::disable("trends_area_sex")
    }
  })


  # Data ----
  # download link
  output$download_table <- downloadHandler(
    filename = function() {
      paste0(
        "age5_pop_",
        censuspyrID:::reg_name(sel_reg(), sel_mode()), "_",
        censuspyrID:::smooth_name(sel_smooth()), "_",
        sel_year(), ".csv"
      )
    },
    content = function(file) {
      tbl <- data_for_table(
        data = pop_data_single(),
        reg_code = sel_reg(),
        harmonized = sel_mode()
      )
      write.csv(tbl, file, row.names = FALSE)
    }
  )

  # DT table
  output$table <- DT::renderDT({
    df <- data_for_table(
      data = pop_data_single(),
      reg_code = sel_reg(),
      harmonized = sel_mode()
      )
    DT::datatable(df) |>
      DT::formatRound(c("Male","Female","Male+Female","sex_ratio"), digits = 3)
  })


  # Help ----
  ## Navigation ----

  ## Metadata ----

  ## Province trajectory ----
  output$sank_prov_traj <- networkD3::renderSankeyNetwork({
    # preparing nodes and links for sankey
    nodes_df <- censuspyrID:::prov_trajectory$nodes |>
      dplyr::filter(expanded == 1)
    links_df <- censuspyrID:::prov_trajectory$links |>
      dplyr::filter(expanded == 1)
    links_df$source <- match(links_df$source, nodes_df$name) - 1
    links_df$target <- match(links_df$target, nodes_df$name) - 1

    # make sankey
    networkD3::sankeyNetwork(
      Links = links_df,
      Nodes = nodes_df,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      NodeGroup = "year",
      fontSize = 9,
      nodeWidth = 25,
      sinksRight = TRUE
    )
  })

  ## About ----


}
