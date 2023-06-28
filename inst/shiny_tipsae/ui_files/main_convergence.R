shiny::tagList(
  shiny::conditionalPanel(
    condition = "output.cond_model_fitted == false", {
      shiny::h3(shiny::strong("The model has not been fitted yet"))
    }),
  shiny::fluidRow(
    shiny::column(width = 1),
    shiny::column(
      width = 10,
      shiny::conditionalPanel(
        condition = "output.cond_model_fitted == true",
        shinybusy::add_busy_spinner(position = 'bottom-left', height = "100px", width = "100px"),
        shiny::h3(shiny::strong("Check of the MCMC algorithm convergence")),
        shiny::wellPanel(
          shiny::h4(shiny::strong("R-hat and effective sample size")),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::plotOutput("plot_rhat")
            ),
            shiny::column(
              width = 5,
              offset = 2,
              shiny::plotOutput("plot_neff")
            )
          )
        ),
        shiny::br(),
        shiny::wellPanel(
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 79%;",
            shiny::h4(shiny::strong("Single parameter's focus"))
          ),
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 20%;",
            shinyWidgets::dropdown(
              shiny::tags$h4(shiny::strong("Plot options")),
              shiny::uiOutput("choose_var_diag"),
              shiny::radioButtons(
                "kind_plot",
                "Diagnostic plot:",
                c("Trace-plot", "ACF", "Density", "Rank plots"),
                inline = F
              ),
              status = "primary",
              icon = shiny::icon("gears"), width = "100%", block = T,
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
            )
          ),
          shiny::plotOutput("plot_single")
        )
      )
    ),
    shiny::column(width = 1)
  )
)
