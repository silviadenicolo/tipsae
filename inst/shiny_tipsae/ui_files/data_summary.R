shiny::tagList(
  shiny::tags$ul(
    shiny::tags$li(shiny::textOutput("is_size")),
    shiny::tags$li(shiny::textOutput("oos_size")),
    shiny::tags$li(shiny::textOutput("zerones"))
  ),
  shiny::hr(style = "border-top: 1px solid #000000;"),

  ### Distribution response ------
  shiny::wellPanel(
    shiny::div(
      style = "display: inline-block;vertical-align:top; width: 75px;",
      shinyWidgets::dropdown(
        shiny::tags$h4(shiny::strong("Plot options")),
        shiny::radioButtons(
          "plot_expl_dist_kind",
          "Plot type to show:",
          c("Box-Plot" = "BP", "Kernel Density" = "Den"),
          inline = F
        ),
        shiny::conditionalPanel(
          condition = "output.time_present==true", {
            shiny::uiOutput("choose_time_dist")
          }),
        status = "primary",
        icon = shiny::icon("gear"), width = "400px",
        tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
      )),
    shiny::div(
      style = "display: inline-block; vertical-align:top; width: 500px;",
      shiny::h4(shiny::strong("Distribution of the response"))),
    shiny::br(),shiny::br(),
    shiny::plotOutput("plot_expl_dist"),
    shiny::br(),
    shiny::downloadButton("download_density_resp", label = "Save ggplot as .RData"),
    shiny::downloadButton("save_pdf_density_resp", label = "Save as .pdf")
  ),

  ### Logit vs covariate ------
  shiny::wellPanel(
    shiny::div(
      style = "display: inline-block;vertical-align:top; width: 75px;",
      shinyWidgets::dropdown(
        shiny::tags$h4(shiny::strong("Plot options")),
        shiny::uiOutput("cov_plot"),
        shiny::conditionalPanel(condition = "output.time_present==true", {
          shiny::uiOutput("choose_time_cov")
        }),
        shiny::radioButtons(
          "plot_expl_yx_loess",
          "LOESS line:",
          c("Yes", "No"),
          inline = F
        ),
        status = "primary",
        icon = shiny::icon("gear"), width = "300px",
        tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
      )),
    shiny::div(
      style = "display: inline-block;vertical-align:top; width: 500px;",
      shiny::h4(shiny::strong("Logit of the response vs covariates"))),
    shiny::br(),shiny::br(),
    shiny::plotOutput("plot_expl_xy"),
    shiny::br(),
    shiny::downloadButton("download_logit", label = "Save ggplot as .RData"),
    shiny::downloadButton("save_pdf_logit", label = "Save as .pdf")
  ),

  ### Plot variances ------
  shiny::conditionalPanel(
    condition = "output.cond_smoothing_ok==true",
    shiny::wellPanel(
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 75px;",
        shinyWidgets::dropdown(
          shiny::tags$h4(shiny::strong("Plot options")),
          shiny::conditionalPanel(
            condition = "output.presence_sample_size==true", {
              shiny::radioButtons(
                "plot_var_kind",
                "",
                c("Dispersion vs sample size" = "n",
                  "Dispersion vs response" = "y"),
                inline = F
              )
            }),
          shiny::conditionalPanel(
            condition = "output.time_present==true", {
              shiny::uiOutput("choose_time_var")
            }),
          shiny::radioButtons(
            "plot_var_loess",
            "LOESS line:",
            c("Yes", "No"),
            inline = F
          ),
          status = "primary",
          icon = shiny::icon("gear"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
        )),
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 500px;",
        shiny::h4(shiny::strong("Dispersion parameter"))),
      shiny::br(),shiny::br(),
      shiny::plotOutput("plot_var_xy"),
      shiny::br(),
      shiny::downloadButton("download_dispersion", label = "Save ggplot as .RData"),
      shiny::downloadButton("save_pdf_dispersion", label = "Save as .pdf")
    )
  ),

  ### Plot map ------
  shiny::conditionalPanel(
    condition = "output.cond_map_shp_matched==true",
    shiny::wellPanel(
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 75px;",
        shinyWidgets::dropdown(
          shiny::tags$h4(shiny::strong("Plot options")),
          shiny::uiOutput("choice_map_quantity_expl"),
          shiny::conditionalPanel(condition = "output.time_present==true", {
            shiny::uiOutput("choose_time_map")
          }),
          status = "primary",
          icon = shiny::icon("gear"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
        )),
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 500px;",
        shiny::h4(shiny::strong("Map"))),
      shiny::br(),shiny::br(),
      shiny::plotOutput("plot_map_expl"),
      shiny::br(),
      shiny::downloadButton("download_map_summary", label = "Save ggplot as .RData"),
      shiny::downloadButton("save_pdf_map_summary", label = "Save as .pdf")
    )
  ),

  ### time trajectories ------
  shiny::conditionalPanel(
    condition = "output.time_present==true",
    shiny::wellPanel(
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 75px;",
        shinyWidgets::dropdown(
          shiny::tags$h4("Plot options"),
          shiny::uiOutput("choice_traj_quantity_expl"),
          shiny::uiOutput("choose_domain_traj"),
          status = "primary",
          icon = shiny::icon("gear"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
        )),
      shiny::div(
        style = "display: inline-block;vertical-align:top; width: 500px;",
        shiny::h4(shiny::strong("Time trajectories"))),
      shiny::br(),shiny::br(),
      shiny::plotOutput("plot_traj_expl"),
      shiny::br(),
      shiny::downloadButton("download_time_traj", label = "Save ggplot as .RData"),
      shiny::downloadButton("save_pdf_time_traj", label = "Save as .pdf")
    )
  )
)
