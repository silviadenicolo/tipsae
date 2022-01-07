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

  data_reff_t <- data.frame(Domain =res_model()$summary$raneff$temporal[, "Domains"],
                            means = res_model()$summary$raneff$temporal[, "mean"])
  data_reff_t <- data_reff_t[res_model()$summary$raneff$temporal$Times==time_selected, ]

  color_palette = c("snow2","#A4112E")
  spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                           data_reff_t,
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

output$map_temp <- shiny::renderPlot({
  plot_map_temp()
}, bg = "transparent")


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
    ggplot2::ggsave(file, plot = plot_map_temp(), device = "pdf")
  }
)




