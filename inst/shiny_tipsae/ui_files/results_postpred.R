shiny::tagList(
  shiny::fluidPage(
    style = "overflow-y:scroll; max-height: 75vh;",
    shiny::wellPanel(
      shinyWidgets::dropdown(
        shiny::tags$h4(shiny::strong("Plot options")),
        shiny::radioButtons("kind_ppc",
                            "Data feature to show:",
                            choices = c("Density",
                                        "Mean",
                                        "S.D.")),
        status = "primary",
        icon = shiny::icon("gear"), width = "300px",
        tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
      ),shiny::br(),shiny::br(),
      shiny::plotOutput("ppc_post"),
      shiny::br(),
      shiny::downloadButton("download_ppc", label = "Save ggplot as .RData"),
      shiny::downloadButton("save_pdf_ppc", label = "Save as .pdf")
    ),shiny::br(),
    shiny::h4(shiny::strong("Area specific Bayesian p-values")),
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(width = 6, shiny::plotOutput("hist_pval"),
                      shiny::br(),
                      shiny::downloadButton("download_hist_pval", label = "Save ggplot as .RData"),
                      shiny::downloadButton("save_pdf_hist_pval", label = "Save as .pdf")),
        shiny::column(width = 6, shiny::h5(shiny::strong("Summary statistics")),
                      shiny::tableOutput("tab_pval"))
      )
    )
  )
)
