shiny::tagList(
  shiny::fluidPage(
    style = "overflow-y:scroll; max-height: 70vh;",
    shiny::h4(shiny::strong("Regression coefficients")),
    shiny::br(),
    shiny::tableOutput("tab_betas"),
    shiny::br(),
    shiny::h4(shiny::strong("S.D. of the random effects")),
    shiny::br(),
    shiny::tableOutput("tab_SDs"),
    shiny::br(),
    shiny::h4(shiny::strong("Checks on residuals")),
    shiny::br(),
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(width = 6, shiny::plotOutput("hist_resid"),
                      shiny::br(),
                      shiny::downloadButton("download_hist_resid", label = "Save ggplot as .RData"),
                      shiny::downloadButton("save_pdf_hist_resid", label = "Save as .pdf")),
        shiny::column(width = 6, shiny::h5(shiny::strong("Summary statistics")),
                      shiny::tableOutput("tab_resid"))
      )
    ),
    shiny::conditionalPanel(
      "output.cond_map_shp_matched==true",{
        shiny::wellPanel(
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 79%;",
            shiny::h4("  ")),
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 20%;",
            shinyWidgets::dropdown(
              inputId = "button_time_resid",
              shiny::tags$h4(shiny::strong("Plot options")
              ),
              shiny::uiOutput("choose_time_map_resid"),
              status = "primary",
              icon = shiny::icon("gears"),
              width = "100%", block = T,
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
            )), shiny::br(),shiny::br(),
          leaflet::leafletOutput("map_resid"),
          shiny::br(),
          shiny::downloadButton("download_map_resid", label = "Save tmap as .RData"),
          shiny::downloadButton("save_pdf_map_resid", label = "Save as .pdf")
        )
      }),
    shiny::br(),
    shiny::h4(shiny::strong("LOO Information Criterion")),
    shiny::actionButton(inputId = "compute_LOOIC", label = "Click compute LOOIC"),
    shiny::br(),shiny::br(),
    shiny::tableOutput("tab_LOOIC")
  )
)
