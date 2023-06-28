### Requested inputs ------

output$choose_time_map2 <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_map2",
                            "Time to include",
                            choices = unique(res_model()$summary$raneff$temporal$Times),
                            selected = unique(res_model()$summary$raneff$temporal$Times)[1],
                            multiple = FALSE)
})

### Creating plot ------

plot_map_temp <- shiny::reactive({
  if(is.null(input$select_time_map2)){
    time_selected <- unique(res_model()$summary$raneff$temporal$Times)[1]
  }else{
    time_selected <- input$select_time_map2
  }

  data_reff_t <- data.frame(Domains =res_model()$summary$raneff$temporal[, "Domains"],
                            means = res_model()$summary$raneff$temporal[, "mean"])
  data_reff_t <- data_reff_t[res_model()$summary$raneff$temporal$Times==time_selected, ]

  color_palette = c("snow2","#A4112E")


  spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                      data_reff_t, by = setNames("Domains", input$choice_match))


  map <-  tmap::tm_shape(spatial_df_plot) +
    tmap::tm_polygons("means",
                      palette = color_palette)


  })

### Output: plot and save -----

output$map_temp <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_temp()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})


output$download_map_temp <- shiny::downloadHandler(
  filename = 'tipsae_map_temp.RData',
  content = function(file) {
    tipsae_map_temp <- plot_map_temp()
    save(tipsae_map_temp, file = file)
  }
)

output$save_pdf_map_temp = shiny::downloadHandler(
  filename = "tipsae_map_temp.pdf",
  content = function(file) {
    tmap::tmap_save(tm = plot_map_temp(),
                    filename = file)
  }
)




