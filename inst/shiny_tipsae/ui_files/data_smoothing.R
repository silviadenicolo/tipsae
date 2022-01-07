shiny::tagList(
  shiny::h4("The smoothing procedure for the dispersion parameters was required.
                  Note that only a simplified smoothing procedure is provided in the Shiny app.
                  See the function smoothing() of the tipsae package to implement more sophisticated methodologies."),
  shiny::hr(style = "border-top: 1px solid #000000;"),
  shiny::br(),
  shiny::wellPanel(
    shiny::h4(shiny::strong("Smoothing assuming that the response is a proportion")),
    shiny::fluidRow(
      shiny::column(width = 6, shiny::radioButtons(
        "smooth_type",
        "Which smoothing procedure do you want to use?",
        c("OLS" = "ols", "GLS" = "gls"),
        inline = T)),
      shiny::column(width = 6, shiny::radioButtons(
        "var_type_smooth",
        "Which dispersion parameter you want to use in the model?",
        c("Variance" = "var", "Effective Sample Size" = "neff"),
        inline = T))
    )
  ),
  shiny::wellPanel(
    shiny::h4(shiny::strong("Diagnostic of the smoothing process")),
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::plotOutput("plot_smoothing")),
      shiny::column(width = 6,
                    shiny::plotOutput("bp_smoothing"))
    )
  )
)
