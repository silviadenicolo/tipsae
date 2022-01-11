### Create Reactive objects -----

## Creation reactive object with the SpatialPlygonDataFrame
map_shp <- shiny::reactive({
  # Active if the user need a .shp file
  if (!is.null(input$shpFile)) {
    shpDF <- input$shpFile
    prevWD <- getwd()
    uploadDirectory <- dirname(shpDF$datapath[1])
    setwd(uploadDirectory)
    for (i in 1:nrow(shpDF)) {
      file.rename(shpDF$datapath[i], shpDF$name[i])
    }
    shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
    shpPath <- paste(uploadDirectory, shpName, sep = "/" )
    setwd(prevWD)
    check_shp_file <- length(shpName) == 0
    shinyFeedback::feedbackDanger("shpFile", check_shp_file,
                                  "The object contained in the '.shp'
                                  input must contain a valid shapefile")
    if (check_shp_file) {
      # NULL if the path input does not refer to a shapefile
      return(NULL)
    } else {
      shpFile <- rgdal::readOGR(shpPath)
      return(shpFile)
    }
  } else {
    # Active if the user need a .rds file
    if (!is.null(input$shpFileRDS)) {
      shpFile <- readRDS(input$shpFileRDS$datapath)
      # check shpFile
      check_shp_file <- class(shpFile) != "SpatialPolygonsDataFrame"
      shinyFeedback::feedbackDanger("shpFileRDS", check_shp_file,
                                    "The object contained in the '.rds' input must be of class
                                      'SpatialPolygonsDataFrame' (see 'sp' package)")
      if (check_shp_file) {
        # NULL if the loaded RDS do not contain a SpatialPolygonsDataFrame
        return(NULL)
      } else {
        return(shpFile)
      }
    } else {# NULL until the a path is specified
      return(NULL)
    }
  }
})


map_shp_matching <- shiny::reactive({
  if (!is.null(input$choice_match)) {
    # Proportion of observed domains that are in the map (MUST BE 1)
    prop_data2sp <- mean(unique(organized_data()$id_domains) %in% map_shp()[[input$choice_match]])
    # Proportion of areas in the map with an observation (COULD BE LESS THAN 1)
    prop_sp2data <- mean(map_shp()[[input$choice_match]] %in% unique(organized_data()$id_domains))
    # check 1: some domains in data are not included in the spatial df: Error
    check1 <- prop_data2sp < 1
    shinyFeedback::feedbackDanger("choice_match", check1,
                                  "At least one data row cannot be matched with areas of the loaded map.")
    ## if no time
    if (is.null(organized_data()$time)) {
      # matching
      spatial_df <- map_shp()
      indices_match <- match(spatial_df[[input$choice_match]], organized_data()$id_domains)
      map_data <- organized_data()$data[indices_match,c(input$choice_resp, input$choice_cov)]
      if (!is.data.frame(map_data)) {
        map_data <- as.data.frame(map_data)
        colnames(map_data) <- input$choice_resp
      }
      # fortify
      spatial_df@data[colnames(map_data)] <- map_data

      spatial_df_tidy <- broom::tidy(spatial_df, region = input$choice_match)
      spatial_df_tidy <- merge(spatial_df_tidy,
                               spatial_df@data,
                               by.x = "id",
                               by.y = input$choice_match)

      if (check1) {
        return(NULL)
      } else {
        return(list(prop_sp2data = prop_sp2data,
                    spatial_df_tidy = spatial_df_tidy,
                    sorted_spatial_df = spatial_df[match(organized_data()$id_domains,
                                                         spatial_df[[input$choice_match]]),]))
      }
    }else{ # With time
      spatial_df <- map_shp()
      # fortify
      spatial_df_tidy <- broom::tidy(spatial_df, region = input$choice_match)
      if (check1) {
        return(NULL)
      } else {
        return(list(prop_sp2data = prop_sp2data,
                    spatial_df_tidy = spatial_df_tidy,
                    sorted_spatial_df = spatial_df[match(unique(organized_data()$id_domains),
                                                         spatial_df[[input$choice_match]]),]))
      }
    }

  }
})



### renderUI for Inputs -----

## Input: field to match with the domains names
output$match_field <- shiny::renderUI({
  shiny::selectInput("choice_match",
                     "Matching field",
                     choices = names(map_shp()),
                     width = '250px')
})

## Output: logical statements ------

## Condition: the section is not active until data is loaded
output$activate_spatial <- shiny::reactive({
  if (input$update_data1 == 0) {
    FALSE
  }else{
    !is.null(organized_data())
  }
})
shiny::outputOptions(output, 'activate_spatial', suspendWhenHidden = FALSE)

## Condition: TRUE if the user want to load spatial data
output$cond_need_spatial <- shiny::reactive({
  input$spatial_load == "Yes"
})
shiny::outputOptions(output, 'cond_need_spatial', suspendWhenHidden = FALSE)

## Condition: TRUE if the user want to use a shapefile
output$cond_spatial_kind <- shiny::reactive({
  input$spatial_load_kind == "ShapeFile"
})
shiny::outputOptions(output, 'cond_spatial_kind', suspendWhenHidden = FALSE)

## Condition: TRUE if a map is correctly specified
output$cond_map_loaded <- shiny::reactive({
  !is.null(map_shp())
})
shiny::outputOptions(output, 'cond_map_loaded', suspendWhenHidden = FALSE)

## Condition: TRUE if matching is valid
output$cond_map_shp_matched <- shiny::reactive({
  !is.null(map_shp_matching())
})
shiny::outputOptions(output, 'cond_map_shp_matched', suspendWhenHidden = FALSE)

## Output: visual/data outcomes -------

## Output: map with the loaded grid
output$map <- shiny::renderPlot({
  if (is.null(map_shp())) return()
  sp::plot(map_shp(), main = "Loaded map")
})

## Output: names of the domains
output$id_domains_data <- shiny::renderText({
  paste(unique(organized_data()$id_domains), collapse = ", ")
})

## Output: names of the areas according to the selected field
output$id_domains_shp <- shiny::renderText({
  if (!is.null(input$choice_match)) {
    paste(map_shp()[[input$choice_match]], collapse = ", ")
  }
})

## Output: some details about the matching step
output$details_matching <- shiny::renderText({
  if (!is.null(map_shp_matching())) {
    if (map_shp_matching()$prop_sp2data == 1) {
      "<font color=\"#5cb85c\"><b>All the domains and the areas in the map matched</b></font>"
    } else {
      "<font color=\"#d9534f\"><b>Observed domains matched, but some areas in the map
        do not have an observed value</b></font>"
    }
  }
})
