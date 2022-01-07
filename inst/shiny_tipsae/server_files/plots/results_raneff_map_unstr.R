### Requested inputs ------

# none

### Creating plot ------

plot_map_unstr <- shiny::reactive({
  data_reff_u <- data.frame(Domain = res_model()$summary$raneff$unstructured[, "Domains"],
                            means = res_model()$summary$raneff$unstructured[, "mean"])

  color_palette = c("snow2","#A4112E")
  spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                           data_reff_u,
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

output$map_unstr <- shiny::renderPlot({
  plot_map_unstr()
}, bg = "transparent")


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
    ggplot2::ggsave(file, plot = plot_map_unstr(), device = "pdf")
  }
)




