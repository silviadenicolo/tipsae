shiny::tagList(
  shiny::fluidPage(
    style = "overflow-y:scroll; max-height: 70vh;",
    shiny::conditionalPanel(
      condition = "output.cond_unstr==true",## unstructured
      shiny::h4(shiny::strong("Unstructured random effects")),
      shiny::wellPanel(
        shiny::h4("Density posterior means"),
        shiny::plotOutput("dens_unstr"),
        shiny::br(),
        shiny::downloadButton("download_dens_unstr", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_dens_unstr", label = "Save as .pdf")),
      shiny::wellPanel(
        shiny::h4("Caterpillar plot"),
        shiny::plotOutput("cat_unstr"),
        shiny::br(),
        shiny::downloadButton("download_cat_unstr", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_cat_unstr", label = "Save as .pdf")),
      shiny::conditionalPanel(
        "output.cond_map_shp_matched==true",{
          shiny::wellPanel(
            shiny::h4("Random effects map"),
            shiny::plotOutput("map_unstr"),
            shiny::br(),
            shiny::downloadButton("download_map_unstr", label = "Save ggplot as .RData"),
            shiny::downloadButton("save_pdf_map_unstr", label = "Save as .pdf")
          )
        })
    ),
    shiny::conditionalPanel(
      condition = "output.cond_spat==true",## spatial
      shiny::h4(shiny::strong("Spatial random effects")),
      shiny::wellPanel(
        shiny::h4("Density posterior means"),
        shiny::plotOutput("dens_spat"),
        shiny::br(),
        shiny::downloadButton("download_dens_spat", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_dens_spat", label = "Save as .pdf")),
      shiny::wellPanel(
        shiny::h4("Caterpillar plot"),
        shiny::plotOutput("cat_spat"),
        shiny::br(),
        shiny::downloadButton("download_cat_spat", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_cat_spat", label = "Save as .pdf")),
      shiny::conditionalPanel(
        "output.cond_map_shp_matched==true",{
          shiny::wellPanel(
            shiny::h4("Random effects map"),
            shiny::plotOutput("map_spat"),
            shiny::br(),
            shiny::downloadButton("download_map_spat", label = "Save ggplot as .RData"),
            shiny::downloadButton("save_pdf_map_spat", label = "Save as .pdf")
          )
        })
    ),
    shiny::conditionalPanel(
      condition = "output.cond_temp==true",## temporal
      shiny::h4(shiny::strong("Temporal random effects")),
      shiny::wellPanel(
        shiny::h4("Density posterior means"),
        shiny::plotOutput("dens_temp"),
        shiny::br(),
        shiny::downloadButton("download_dens_temp", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_dens_temp", label = "Save as .pdf")
      ),
      shiny::wellPanel(
        shiny::h4("Caterpillar plot"),
        shinyWidgets::dropdown(
          inputId = "button_time_cat",
          shiny::tags$h4(shiny::strong("Plot options")),
          shiny::uiOutput("choose_time_cat"),
          status = "primary",
          icon = shiny::icon("cog"), width = "300px",
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
        ),shiny::br(),shiny::br(),
        shiny::plotOutput("cat_temp"),
        shiny::br(),
        shiny::downloadButton("download_cat_temp", label = "Save ggplot as .RData"),
        shiny::downloadButton("save_pdf_cat_temp", label = "Save as .pdf")
      ),
      shiny::conditionalPanel(
        "output.cond_map_shp_matched==true",{
          shiny::wellPanel(
            shiny::h4("Random effects map"),
            shinyWidgets::dropdown(
              inputId = "button_time_map2",
              shiny::tags$h4(shiny::strong("Plot options")),
              shiny::uiOutput("choose_time_map2"),
              status = "primary",
              icon = shiny::icon("cog"), width = "300px",
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
            ),shiny::br(),shiny::br(),
            shiny::plotOutput("map_temp"),
            shiny::br(),
            shiny::downloadButton("download_map_temp", label = "Save ggplot as .RData"),
            shiny::downloadButton("save_pdf_map_temp", label = "Save as .pdf")
          )
        }),
        shiny::wellPanel(
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 75px;",
            shinyWidgets::dropdown(
              shiny::tags$h4("Plot options"),
              shiny::uiOutput("choose_domain_traj_raneff"),
              status = "primary",
              icon = shiny::icon("cog"), width = "300px",
              tooltip = shinyWidgets::tooltipOptions(title = "Click to see plot options")
            )),
          shiny::div(
            style = "display: inline-block;vertical-align:top; width: 500px;",
            shiny::h4(shiny::strong("Time trajectories"))),
          shiny::br(),shiny::br(),
          shiny::plotOutput("plot_traj_raneff"),
          shiny::br(),
          shiny::downloadButton("download_time_traj_raneff", label = "Save ggplot as .RData"),
          shiny::downloadButton("save_pdf_time_traj_raneff", label = "Save as .pdf")
      )
    )
  )
)

