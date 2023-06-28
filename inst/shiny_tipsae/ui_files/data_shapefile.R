shiny::tagList(
  shiny::radioButtons(
    "spatial_load",
    "Requested to produce maps and/or
          to include a spatially structured random effect in the model.",
    c("No", "Yes"),
    inline = T
  ),
  ## It NO: stop here for the spatial part
  ## If YES: request for the kind of file to load (shp/rds)
  shiny::conditionalPanel(
    condition = "output.cond_need_spatial==true" ,
    shiny::br(),shiny::br(),
    shiny::h3(shiny::strong("Shapefile input")),
    shiny::br(),
    shiny::wellPanel(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::radioButtons(
            "spatial_load_kind",
            "Which kind of object you want to load?",
            c("ShapeFile", "RDS"),
            inline = T
          )
        ),
        shiny::column(
          width = 7, offset = 1,
          ## If YES and SHP: input block of requested files
          shiny::conditionalPanel(
            condition = "output.cond_spatial_kind==true" , {
              shiny::fileInput(
                "shpFile",
                buttonLabel = "Browse",
                accept = c('.shp', '.dbf', '.sbn', '.sbx', '.shx', ".prj"),
                label = "Input Shape file: all the files in the shp folder must be selected",
                multiple = TRUE
              )
            }),
          ## If YES and RDS: input the requested file (check for the contained object)
          shiny::conditionalPanel(
            condition = "output.cond_spatial_kind==false" , {
              shiny::fileInput(buttonLabel = "Browse",
                "shpFileRDS",
                accept = c('.rds'),
                label = "Input RDS: an object containing a 'SpatialPolygonsDataFrame' or 'sf' must be loaded"
              )
            })
        )
      )
    ),
    ## Map correctly loaded: information to match data and map
    shiny::conditionalPanel(
      condition = "output.cond_map_loaded==true" ,
      shiny::br(),shiny::br(),
      shiny::h3(shiny::strong("Loaded map and matching variable")),
      shiny::br(),
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::column(width = 5, shiny::plotOutput("map")),
          shiny::column(width = 5,offset = 2,
                        style = "margin-top: 8px;",
                        shiny::uiOutput("match_field"),
                        shiny::conditionalPanel(
                          condition = "output.cond_map_shp_matched==true", {
                          shiny::htmlOutput("details_matching")
                        })
          )
        )
      ),
      ## Print the domains names and the field selected to match
      shiny::fluidRow(
        shiny::column(width = 6,
                      shiny::h4(shiny::strong("Names of the domains in the input data")),
                      shiny::wellPanel(shiny::textOutput("id_domains_data"))
        ),
        shiny::column(width = 6,
                      shiny::h4(shiny::strong("Names of the domains in the spatial dataset")),
                      shiny::wellPanel(shiny::textOutput("id_domains_shp"))
        )
      )
    )
  )
)
