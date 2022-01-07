shiny::tagList(
  shiny::fluidPage(
    style = "overflow-y:scroll; max-height: 70vh;",
    shiny::h4(shiny::strong("Model-based Estimates")),
    shiny::br(),
    shiny::wellPanel(
      shiny::h5(shiny::strong("In-sample areas")),
      shiny::dataTableOutput("tab_is")
    ),
    shiny::wellPanel(
      id = "panel_tab_oos",
      shiny::h5(shiny::strong("Out-of-sample areas")),
      shiny::dataTableOutput("tab_oos")
    ),
    shiny::downloadButton('download_estimates',"Download the Model-Based Estimates"),
    shiny::conditionalPanel(
      condition = "output.cond_map_shp_matched==true",
      shiny::br(),
      shiny::h4(shiny::strong("Map")),
      shiny::wellPanel(
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 75px;",
          shinyWidgets::dropdown(
            shiny::tags$h4(shiny::strong("Plot options")),
            shiny::radioButtons("select_map_quantity_estimates",
                                "Variable to show in the map:",
                                choices = c("HB Estimate" = "HB est.", "Posterior S.D." = "sd"),
                                width = '200px'),
            shiny::radioButtons("oos_in_map",
                                "Show the out-of-sample areas?",
                                choices = c("Yes", "No"),
                                width = '200px'),
            shiny::conditionalPanel(condition = "output.time_present==true", {
              shiny::uiOutput("time_map_estimates")
            }),
            status = "primary",
            icon = shiny::icon("gear"), width = "300px",
            tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
          )),
        shiny::div(
          shiny::br(),
          shiny::plotOutput("plot_map_estimates"),
          shiny::br(),
          shiny::downloadButton("download_map_estimates", label = "Save ggplot as .RData"),
          shiny::downloadButton("save_pdf_map_estimates", label = "Save as .pdf")
        )
      )
    ),
    shiny::br(),
    shiny::h4(shiny::strong("Caterpillar plot")),
    shiny::wellPanel(
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 75px;",
        shinyWidgets::dropdown(
          shiny::tags$h4(shiny::strong("Plot options")),
          shiny::radioButtons("oos_in_cat",
                              "Show the out-of-sample areas?",
                              choices = c("Yes", "No"),
                              width = '200px'),
          shiny::conditionalPanel(condition = "output.time_present==true", {
            shiny::uiOutput("time_cat_estimates")
          }),
          status = "primary",
          icon = shiny::icon("gear"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
        )),
      shiny::div(
        shiny::br(),
        shiny::plotOutput("plot_cat_estimates"),
        shiny::br(),
        shiny::downloadButton("download_cat_estimates", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_cat_estimates", label = "Save as .pdf")
      )
    ),
    shiny::conditionalPanel(
      condition = "output.cond_temp==true",## trajectories
      shiny::wellPanel(
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 75px;",
          shinyWidgets::dropdown(
            shiny::tags$h4("Plot options"),
            shiny::radioButtons("oos_in_traj",
                                "Show the out-of-sample areas?",
                                choices = c("Yes", "No"),
                                width = '200px'),
            shiny::uiOutput("choose_domain_traj_estimate"),
            status = "primary",
            icon = shiny::icon("gear"), width = "300px",
            tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
          )),
        shiny::div(
          style = "display: inline-block;vertical-align:top; width: 500px;",
          shiny::h4(shiny::strong("Time trajectories"))),
        shiny::br(),shiny::br(),
        shiny::plotOutput("plot_traj_estimate"),
        shiny::br(),
        shiny::downloadButton("download_time_traj_estimate", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_time_traj_estimate", label = "Save as .pdf")
      )
    )

  )
)
