### Requested inputs ------

# Select the time
output$choose_time_map_resid <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_map_resid",
                            "Times to include",
                            choices = unique(res_model()$data_ris$Time),
                            selected = unique(res_model()$data_ris$Time)[1],
                            multiple = FALSE)
})

### Creating plot ------

plot_map_resid <- shiny::reactive({
  if(!is.null(map_shp_matching())){
    color_palette = c("snow2","#A4112E")

    if(!is.null(res_model()$data_ris$Time)){
      if(is.null(input$select_time_map_resid)){
        selected_times <- unique(res_model()$data_ris$Time)[1]
      }else{
        selected_times <- input$select_time_map_resid
      }
      # prendo i dati solo dell'anno
      data_sub <- res_model()$data_ris[res_model()$data_ris$Time == selected_times,
                                       c("Domain", "Residuals")]

      spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                          data_sub, by = setNames("Domain", input$choice_match))


      map <-  tmap::tm_shape(spatial_df_plot) +
        tmap::tm_polygons("Residuals",
                          palette = color_palette)


    }else{
      data_sub <- res_model()$data_ris[,
                                       c("Domain", "Residuals")]


      spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                          data_sub, by = setNames("Domain", input$choice_match))


      map <-  tmap::tm_shape(spatial_df_plot) +
        tmap::tm_polygons("Residuals",
                          palette = color_palette)


    }
    map


  }
})

### Output: plot and save -----

output$map_resid <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_resid()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})

output$download_map_resid <- shiny::downloadHandler(
  filename = 'tipsae_map_resid.RData',
  content = function(file) {
    tipsae_logit <- plot_map_resid()
    save(tipsae_map_resid, file = file)
  }
)

output$save_pdf_map_resid = shiny::downloadHandler(
  filename = "tipsae_map_resid.pdf",
  content = function(file) {
    tmap::tmap_save(tm = plot_map_resid(),
                    filename = file)
  }
)




