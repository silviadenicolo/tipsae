shiny::tagList(
  shiny::navlistPanel(
    well = TRUE,
    fluid = TRUE,
    widths = c(2, 10),
    #### LOAD DATA ----------------------------------
    shiny::tabPanel(
      title = "Loading data",
      shiny::fluidPage(
            style = "overflow-y:scroll; max-height: 85vh;",
            shiny::h3(shiny::strong("File input")),
            source_ui("data_load.R")
      )
    ),
    #### SMOOTHING PROCEDURE ----------------------------------
    shiny::tabPanel(
      title = "Smoothing",
      # No loaded data
      shiny::conditionalPanel(
        condition = "output.activate_spatial==false",
        shiny::h3(shiny::strong("Loading data is required to smooth variances"))
      ),
      ## No smoothing procedure required
      shiny::conditionalPanel(
        condition = "output.cond_no_smooth==true",
        shiny::h3(shiny::strong("The smoothing procedure for the dispersion parameters is not required."))
      ),
      ## Smoothing procedure
      shiny::conditionalPanel(
        condition = "output.cond_no_smooth==false",
        shiny::h3(shiny::strong("Smoothing procedure for dispersion parameters")),
        source_ui("data_smoothing.R")
      )
    ),
    #### LOADING SPATIAL STRUCTURE ----------------------------------
    shiny::tabPanel(
      title = "Load shapefile",
      shiny::fluidPage(
        style = "overflow-y:scroll; max-height: 85vh; min-height: 85vh; height: 85vh;",
        ## Section not active until data is loaded
        shiny::conditionalPanel(
          condition = "output.activate_spatial==false",
          shiny::h3(shiny::strong("Loading data is required before loading a shapefile."))
        ),
        ## Data loaded: request for a spatial structure
        shiny::conditionalPanel(
          condition = "output.activate_spatial==true",
          shiny::h3(shiny::strong("Do you want to load a shapefile?")),
          source_ui("data_shapefile.R")
        )
      )
    ),
    #### PART WITH SOME DESCRIPTIVE OUTCOMES ----------------------------------
    shiny::tabPanel(
      title = "Data Summary",
      shiny::fluidPage(
        style = "overflow-y:scroll; max-height: 85vh",
        shiny::conditionalPanel(
          condition = "output.activate_spatial==false",
          shiny::h3(shiny::strong("Loading data is required before displaying exploratory tools"))
        ),
        ## Data loaded: request for a spatial structure
        shiny::conditionalPanel(
          condition = "output.activate_spatial==true",
          shinybusy::add_busy_spinner(position = 'bottom-left', height = "100px", width = "100px"),
          shiny::h3(shiny::strong("Information about the data")),
          source_ui("data_summary.R")
        )
      )
    )
  )
)

