# UI for censuspyrID explorer apps

# Define UI for application that draws a histogram
fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("yeti"),
  titlePanel(
    title = HTML(
     "<h3><b>censuspyrID Explorer</b></h3>
     <h5>Exploring Harmonized and Non-Harmonized Population Pyramids
     from Indonesia’s Censuses (1971–2020)</h5>"
    )
  ),


  # Sidebar for main parameter ----
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # mode & province
      h5("Province:"),
      shinyWidgets::materialSwitch(
        "regmode",
        "Harmonized",
        value = TRUE,
        status = "primary"
      ),

      # region (province) list
      uiOutput("reg_select"),

      # expanded information
      htmlOutput("expand_info"),

      # expand phase
      uiOutput("phase_expand"),

      # year
      uiOutput("year_select"),

      # spacing
      hr(),

      # smoothing option
      shinyWidgets::radioGroupButtons(
        "smoothopt",
        h5("Smoothing option:"),
        censuspyrID:::censuspyrID_data_env$smoothing,
        selected = 1,
        justified = FALSE,
        direction = "horizontal",
        size = "sm"
      ),

      # spacing
      hr(),

      # color palettes using canva palettes from ggthemes package
      shinyWidgets::pickerInput(
        "col_picker",
        h5("Color:"),
        choices = censuspyrID:::censuspyrID_data_env$col_name,
        choicesOpt = list(
          content = censuspyrID:::censuspyrID_data_env$col_name_opt
        ))

    ),


    mainPanel(
      tabsetPanel(


        # Pyramids tab ----
        tabPanel(
          "Pyramids",
          value = 1,

          # tags$head(
          #   tags$style(HTML("
          #     .equal-box {
          #     border: 1px solid #333;
          #     padding: 10px;
          #     height: 500px;   /* atur tinggi sesuai kebutuhan */
          #     overflow-y: auto;
          #     }
          #                   "))
          # ),
          ## text for census year ----
          fluidRow(column(6, offset = 5, textOutput('pyr_year'))),

          fluidRow(
            ## pyramid plot ----
            column(
              width = 8,
              # div(class = "equal-box",
              #     plotOutput("pyr_plot"),
              #
              # ),
              plotOutput("pyr_plot", height = "500px"),
              # widget for plot option
              checkboxInput("use_prop", "Use proportion")
            ),

            ## summary ----
            column(
              width = 4,
              div(
                style = "border: 1px solid #ddd;
                    border-radius: 5px;
                    padding: 10px;
                    background-color: #f5f5f5;
                    height:500px;
                    overflow-y:auto;",
                verbatimTextOutput("pyr_summary")
              )
            )
          )
        ),


        # Age profile ----
        tabPanel(
          "Age-profile",
          value = 2,

          ## text for census year ----
          fluidRow(column(6, offset = 5, textOutput('ageprof_year'))),

          fluidRow(
            ## age-profile plot ----
            plotOutput("ageprof_plot", height = "500px"),
            # widget for plot option
            checkboxInput("logscale", "Log scale")
          )
        ),

        # Trends ----
        tabPanel(
          "Trends",
          value = 3,

          fluidRow(
            ## trends plot ----
            plotOutput("trends_plot", height = "500px"),

            # widget for plot option
            column(
              width = 2,
              radioButtons(
                "trends_type",
                "Type:",
                choices = censuspyrID:::censuspyrID_data_env$trends_type,
                selected = 1,
                inline = TRUE)
              ),
            column(
              width = 2,
              radioButtons(
                "trends_pyr_mode",
                "Pyramid mode:",
                choices = censuspyrID:::censuspyrID_data_env$trends_pyr_mode,
                selected = 1,
                inline = TRUE)
              ),
            column(width = 2, checkboxInput("trends_use_prop", "Use proportion")),
            column(
              width = 4,
              radioButtons(
                "trends_area_sex",
                "Sex:",
                choices = censuspyrID:::censuspyrID_data_env$trends_area_sex,
                selected = 1,
                inline = TRUE
              )
            )
          )

        ),

        # Data tab ----
        tabPanel(
          "Data",
          value = 4,
          br(),
          downloadLink("download_table", "Download"),
          hr(),
          DT::dataTableOutput("table")
        ),

        # Help ----
        tabPanel(
          "Help",
          value = 5,
          tabsetPanel(
            ## Navigation ----
            tabPanel(
              "Navigation",
              div(
                style = "max-height:600px; overflow-y:auto; padding:10px; border:1px solid #ddd;",
                includeMarkdown(system.file("app/censuspyrIDExplorer/help/navigation.md", package = "censuspyrID"))
              )
            ),

            ## Metadata ----
            tabPanel(
              "Metadata",
              div(
                style = "max-height:600px; overflow-y:auto; padding:10px; border:1px solid #ddd;",
                includeMarkdown(system.file("app/censuspyrIDExplorer/help/metadata.md", package = "censuspyrID"))
              )
            ),

            ## Province trajectory ----
            tabPanel(
              "Province trajectory",
              networkD3::sankeyNetworkOutput("sank_prov_traj", height = "575px")
            ),

            ## About ----
            tabPanel(
              "About",
              div(
                style = "max-height:600px; overflow-y:auto; padding:10px; border:1px solid #ddd;",
                includeMarkdown(system.file("app/censuspyrIDExplorer/help/about.md", package = "censuspyrID"))
              )
            ),
            type = "pills"
          )
        )

      )
    )

  )

)
