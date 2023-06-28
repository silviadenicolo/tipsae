### Requested inputs ------

output$time_map_estimates <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_map_estimates",
                            "Time to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time)[1],
                            multiple = FALSE)
})

shiny::observe({
  if (!is.null(estimates()$out_of_sample)) {
    shinyjs::show(id = "oos_in_map")
  }else{
    shinyjs::hide(id = "oos_in_map")
  }
})

### Creating plot ------

plot_map_estimates <- shiny::reactive({
  if (is.null(input$select_map_quantity_estimates)) {
    name_var <- "HB est."
  }else{
    name_var <- input$select_map_quantity_estimates
  }

  #input?
  color_palette = c("snow2","#A4112E")
  if(!is.null(organized_data()$time)){
    if(is.null(input$select_time_map_estimates)){
      selected_times <- unique(organized_data()$time)[1]
    }else{
      selected_times <- input$select_time_map_estimates
    }


    data_sub <- estimates()$in_sample[estimates()$in_sample$Times == selected_times,
                                      c("Domains", name_var)]
  }else{
    data_sub <- estimates()$in_sample[, c("Domains", name_var)]
  }
    if (input$oos_in_map == "Yes") {
      if (!is.null(organized_data()$time)) {
        data_oos <- estimates()$out_of_sample[estimates()$out_of_sample$Times == selected_times,
                                          c("Domains", name_var)]
      }else{
        data_oos <- estimates()$out_of_sample[,c("Domains", name_var)]
      }

      data_sub <- rbind(data_sub, data_oos)
    }


    spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                      data_sub, by = setNames("Domains", input$choice_match))


    map <-  tmap::tm_shape(spatial_df_plot) +
      tmap::tm_polygons(name_var,
                        palette = color_palette)


  map
})

### Output: plot and save -----

output$plot_map_estimates <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_estimates()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})

output$download_map_estimates <- shiny::downloadHandler(
  filename = 'tipsae_map_estimates.RData',
  content = function(file) {
    tipsae_map_estimates <- plot_map_estimates()
    save(tipsae_map_estimates, file = file)
  }
)

output$save_pdf_map_estimates <- shiny::downloadHandler(
  filename = "tipsae_map_estimates.pdf",
    content = function(file) {
    tipsae_map_estimates <- plot_map_estimates()
    tmap::tmap_save(tm = tipsae_map_estimates,
                    filename = file)
  }
)

