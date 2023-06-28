### Requested inputs ------

output$choose_time_map <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_map",
                            "Times to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time)[1],
                            multiple = FALSE)
})
# Input: choose the names of the domains
output$choice_map_quantity_expl <- shiny::renderUI({
  shiny::radioButtons("map_quantity_expl",
                      "Variable to plot in the map:",
                      choices = c(input$choice_resp, input$choice_cov),
                      width = '200px')
})


### Creating plot ------

plot_map_summary <- shiny::reactive({
  if(is.null(input$map_quantity_expl)){
    name_var<-input$choice_resp
  }else{
    name_var<-input$map_quantity_expl
  }

  #input?
  color_palette = c("snow2","#A4112E")
  if(!is.null(organized_data()$time)){
    if(is.null(input$select_time_map)){
      selected_times <- unique(organized_data()$time)[1]
    }else{
      selected_times <- input$select_time_map
    }


    data_sub <- organized_data()$data[organized_data()$data[,input$time_col] == selected_times,
                                      c(input$domain_col, name_var)]

    spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                        data_sub, by = setNames(input$domain_col, input$choice_match))


    map <-  tmap::tm_shape(spatial_df_plot) +
      tmap::tm_polygons(name_var,
                        palette = color_palette)

  }else{

    map <-  tmap::tm_shape(map_shp_matching()$spatial_df_tidy) +
      tmap::tm_polygons(name_var,
                        palette = color_palette)

  }


  map
})

### Output: plot and save -----

output$plot_map_expl <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_summary()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})


output$download_map_summary <- shiny::downloadHandler(
  filename = 'tipsae_map_summary.RData',
  content = function(file) {
    tipsae_map_summary <- plot_map_summary()
    save(tipsae_map_summary, file = file)
  }
)

output$save_pdf_map_summary = shiny::downloadHandler(
  filename = "tipsae_map_summary.pdf",
    content = function(file) {
    tipsae_map_summary <- plot_map_summary()
    tmap::tmap_save(tm = tipsae_map_summary,
                    filename = file)
  }
)




