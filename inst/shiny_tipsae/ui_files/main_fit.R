shiny::tagList(
  # Message no data
  shiny::conditionalPanel(
    condition = "output.activate_spatial==false || output.cond_smoothing_ok==false", {
      shiny::h3(shiny::strong("Data loading required to estimate the model."))
    }),
  shiny::fluidRow(
    shiny::column(width = 1),
    shiny::column(
      width = 10,
      # Message no models estimable with the loaded data
      shiny::conditionalPanel(
        "output.activate_spatial==true && output.cond_smoothing_ok==true && output.cond_est1==false",{
          shiny::fluidPage(
            shiny::textOutput("warning_fit1")
          )
        }),
      # Main panel
      shiny::conditionalPanel(
        "output.activate_spatial==true && output.cond_smoothing_ok==true && output.cond_est1==true",
        ## Model specification
        shiny::h3(shiny::strong("Model specification")),## column model specification
        shinyjs::useShinyjs(),
        shinybusy::add_busy_spinner(position = 'bottom-left', height = "100px", width = "100px"),
        shiny::wellPanel(
          shiny::h3(shiny::strong("1) Likelihood")),
          shiny::uiOutput("choice_lik"),
          shiny::h3(shiny::strong("2) Random Effects")),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::radioButtons(
                "prior_reff",
                "Select the prior setting for the ustructured random
            effects (ignored in spatio-temporal models):",
                choices = c("Gaussian" = "normal",
                            "Robust (Student's t)" = "t",
                            "Shrinkage (Variance Gamma)" = "VG")
              )
            ),
            shiny::column(
              width = 6,
              shiny::conditionalPanel(
                "output.cond_est2 == true",{
                  shiny::uiOutput("choice_str_reff")
                })
            )
          )
        ),
        shiny::hr(style = "border-top: 1px solid #000000;"),
        shiny::h3(shiny::strong("Settings about the MCMC algorithm")),
        # Settings HMC
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::br(),
            shiny::numericInput(inputId = "iter", label = "MC iterations (half as warm-up)",
                                min = 100, max = 10000, step = 1,value = 4000),
            shiny::br(),shiny::br(),
            shiny::checkboxInput(inputId = "multiple_chains",
                                 label = "Multiple chains approach", value = TRUE),
            shiny::br(),
          ),
          shiny::column(
            width = 6,
            shiny::wellPanel(
              id = "parallel_panel",
              shiny::numericInput(inputId = "chains", label = "Number of chains",
                                  min = 2, max = 6, step = 1,value = 4),
              shiny::fluidRow(
                shiny::column(
                  width = 5,
                  style = "margin-top: 12px;",
                  shiny::checkboxInput(inputId = "parallel", label = "Parallel computation", value = TRUE)
                ),
                shiny::column(
                  width = 5,
                  offset = 2,
                  shiny::numericInput(inputId = "cores", label = "Number of cores", min = 2,
                                      max = parallel::detectCores(), step = 1,value = 4)
                )
              )
            )
          )
        ),
        shiny::br(),
        shiny::wellPanel(
          shiny::h4(shiny::strong("Additional options for the HMC algorithm")),
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shiny::sliderInput(inputId = "max_tree", label = "Maximum treedepth",
                                 min = 8, max = 18, step = 1, value = 10
              )
            ),
            shiny::column(
              width = 5,
              offset = 2,
              shiny::sliderInput(inputId = "adapt_delta", label = "adapt_delta",
                                 min = .6, max = 0.99, step = 0.01, value = 0.8)
            )
          )
        ),
        # Fit model and results
        shiny::br(),shiny::br(),
        shiny::actionButton(inputId = "fit_model", label = "Fit Model"),
        shiny::br(),shiny::br(),
        shiny::conditionalPanel(
          condition = "output.cond_model_fitting == true",
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::tags$head(shiny::tags$style("#show_prog{overflow-y:scroll; max-height: 300px; overflow-x:scroll;}")),
                shiny::verbatimTextOutput("show_prog")
              ),
              shiny::column(
                width = 6,
                shiny::conditionalPanel(
                  condition = "output.cond_model_fitted == true",
                  shiny::h3("Model fitted. Checks on the Monte Carlo algorithm."),
                  shiny::h4("Divergent transitions:"),
                  shiny::verbatimTextOutput("show_div"),
                  shiny::h4("Tree depth:"),
                  shiny::verbatimTextOutput("show_tree")
                )
              )
            )
          )
        )
      )
    ),
    shiny::column(width = 1),
  )
)
