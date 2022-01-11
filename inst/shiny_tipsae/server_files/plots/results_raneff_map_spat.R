### Requested inputs ------

# none

### Creating plot ------

plot_map_spat <- shiny::reactive({
  data_reff_s <- data.frame(Domain = res_model()$summary$raneff$spatial[, "Domains"],
                            means = res_model()$summary$raneff$spatial[, "mean"])
  print(data_reff_s)

  color_palette = c("snow2","#A4112E")
  spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                           data_reff_s,
                           by.x = "id",
                           by.y = "Domain")
  ggplot2::ggplot(spatial_df_plot,
                  ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                fill  = ~ means)) +
    ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(
      low = color_palette[1],
      high = color_palette[2],
      limits = range(spatial_df_plot["means"][, 1]),
      name = "Ran. Effects",
      guide = "colourbar"
    ) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
    ggplot2::coord_sf()


  })

### Output: plot and save -----

output$map_spat <- shiny::renderPlot({
  plot_map_spat()
}, bg = "transparent")


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
    ggplot2::ggsave(file, plot = plot_map_spat(), device = "pdf")
  }
)




