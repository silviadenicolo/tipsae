shiny::tagList(
  shiny::conditionalPanel(
    condition = "output.cond_model_fitted == false", {
      shiny::h3(shiny::strong("The model has not been fitted yet"))
    }),
  shiny::conditionalPanel(
    condition = "output.cond_model_fitted == true",
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::radioButtons(
          inputId = "level_cred",
          label = "Choose the level of the credibility intervals:",
          choices = c("0.8", "0.9", "0.95", "0.99"),
          selected = "0.95",
          inline = T
        )
      ),
      shiny::column(
        width = 4,
        shiny::numericInput(inputId = "digits", label = "Number of digits to display",
                            min = 1, max = 6, step = 1,value = 4)
      ),
      shiny::column(
        width = 3,
        offset = 1,
        shiny::actionButton(inputId = "compute_results", label = "Click to update the results")
      )
    ),
    shiny::hr(style = "border-top: 1px solid #000000;"),
    shiny::br()
  ),
  shiny::conditionalPanel(
    condition = "output.cond_res_show == true",
    shinybusy::add_busy_spinner(position = "bottom-left", height = "100px", width = "100px"),
    shiny::navlistPanel(
      well = TRUE,
      fluid = FALSE,
      widths = c(2, 10),
      shiny::tabPanel(

        #### Model Summaries --------
        title = "Model summaries",
        source_ui("results_summary.R")
      ),
      shiny::tabPanel(
        #### Posterior predictive --------
        title = "Posterior Predictive Checks",
        source_ui("results_postpred.R")
      ),
      shiny::tabPanel(
        #### SAE DIAG --------
        title = "SAE Diagnostics",
        source_ui("results_SAEdiag.R")
      ),
      shiny::tabPanel(
        #### random Effects --------
        title = "Random effects",
        source_ui("results_raneff.R")
      ),
      shiny::tabPanel(
        #### Estimates --------
        title = "Model Estimates",
        source_ui("results_estimates.R")
      )
    )
  )


)
