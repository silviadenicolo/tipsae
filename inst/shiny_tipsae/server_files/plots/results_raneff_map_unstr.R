### Requested inputs ------

# none

### Creating plot ------

plot_map_unstr <- shiny::reactive({
  data_reff_u <- data.frame(Domains = res_model()$summary$raneff$unstructured[, "Domains"],
                            means = res_model()$summary$raneff$unstructured[, "mean"])

  color_palette = c("snow2","#A4112E")


  spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                      data_reff_u, by = setNames("Domains", input$choice_match))


  map <-  tmap::tm_shape(spatial_df_plot) +
    tmap::tm_polygons("means",
                      palette = color_palette)



  })

### Output: plot and save -----

output$map_unstr <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_unstr()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})


output$download_map_unstr <- shiny::downloadHandler(
  filename = 'tipsae_map_unstr.RData',
  content = function(file) {
    tipsae_map_unstr <- plot_map_unstr()
    save(tipsae_map_unstr, file = file)
  }
)

output$save_pdf_map_unstr = shiny::downloadHandler(
  filename = "tipsae_map_unstr.pdf",
  content = function(file) {
    tmap::tmap_save(tm = plot_map_unstr(),
                    filename = file)
  }
)




