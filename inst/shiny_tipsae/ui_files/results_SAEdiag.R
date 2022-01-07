shiny::tagList(
  shiny::fluidPage(
    style = "overflow-y:scroll; max-height: 70vh;",
    shiny::h4(shiny::strong("Shrinkage of the estimates")),
    shiny::br(),
    shiny::textOutput("sbr"),
    shiny::br(),
    shiny::wellPanel(
      shiny::fluidRow(shiny::column(
        width = 6,
        shiny::plotOutput("plot_shrinkage"),
        shiny::br(),
        shiny::downloadButton("download_shrinkage", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_shrinkage", label = "Save as .pdf")
      ),
      shiny::column(
        width = 6,
        shiny::plotOutput("plot_density"),
        shiny::br(),
        shiny::downloadButton("download_density", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_density", label = "Save as .pdf")
      )
      )
    ), shiny::br(),
    shiny::conditionalPanel(
      condition = "output.presence_sample_size==true",
      shiny::h4(shiny::strong("Design consistency")),
      shiny::wellPanel(
        shiny::plotOutput("design_cons"),
        shiny::br(),
        shiny::downloadButton("download_design_cons", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_design_cons", label = "Save as .pdf")
      ),
      shiny::br()
    ),
    shiny::h4(shiny::strong("Standard Deviation Estimates")),
    shiny::wellPanel(
      shiny::plotOutput("boxplot_sdr"),
      shiny::br(),
      shiny::downloadButton("download_box_sdr", label = "Save ggplot as .RData"),
      shiny::downloadButton("save_pdf_box_sdr", label = "Save as .pdf")
    ),
    shiny::br(),
    shiny::h4(shiny::strong("Standard Deviation Reduction")),
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::plotOutput("hist_sdr"),
          shiny::br(),
          shiny::downloadButton("download_hist_sdr", label = "Save ggplot as .RData"),
          shiny::downloadButton("save_pdf_hist_sdr", label = "Save as .pdf")
        ),
        shiny::column(
          width = 6,
          shiny::h5(shiny::strong("Summary statistics")),
          shiny::tableOutput("tab_sdr")
        )
      )
    )
  )
)
