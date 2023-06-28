### Requested inputs ------

# none

### Creating plot ------

plot_map_spat <- shiny::reactive({
  data_reff_s <- data.frame(Domains = res_model()$summary$raneff$spatial[, "Domains"],
                            means = res_model()$summary$raneff$spatial[, "mean"])

  color_palette = c("snow2","#A4112E")

  spatial_df_plot <- dplyr::left_join(map_shp_matching()$spatial_df_tidy,
                                      data_reff_s, by = setNames("Domains", input$choice_match))


  map <-  tmap::tm_shape(spatial_df_plot) +
    tmap::tm_polygons("means",
                      palette = color_palette)


  })

### Output: plot and save -----

output$map_spat <- leaflet::renderLeaflet({
  tmap::tmap_leaflet(plot_map_spat()+
                       tmap::tm_view(view.legend.position = c("left", "bottom")), in.shiny = T)
})


output$download_map_spat <- shiny::downloadHandler(
  filename = 'tipsae_map_spat.RData',
  content = function(file) {
    tipsae_map_spat <- plot_map_spat()
    save(tipsae_map_spat, file = file)
  }
)

output$save_pdf_map_spat = shiny::downloadHandler(
  filename = "tipsae_map_spat.pdf",
  content = function(file) {
    tmap::tmap_save(tm = plot_map_spat(),
                    filename = file)
  }
)




