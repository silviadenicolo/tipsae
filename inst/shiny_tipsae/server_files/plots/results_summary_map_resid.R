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
      map_shp_matching()$spatial_df_tidy
      spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                               data_sub,
                               by.x = "id",
                               by.y = "Domain")
      map <- ggplot2::ggplot(spatial_df_plot,
                             ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                           fill  = ~ Residuals)) +
        ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::scale_fill_gradient(
          low = color_palette[1],
          high = color_palette[2],
          limits = range(spatial_df_plot["Residuals"][, 1]),
          name = "Residuals",
          guide = "colourbar"
        ) +
        ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank()) +
        ggplot2::coord_sf()
    }else{
      data_sub <- res_model()$data_ris[,
                                       c("Domain", "Residuals")]
      spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                               data_sub,
                               by.x = "id",
                               by.y = "Domain")
      map <- ggplot2::ggplot(spatial_df_plot,
                             ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                           fill  = ~ Residuals)) +
        ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_bw(base_size = 15) +
        ggplot2::scale_fill_gradient(
          low = color_palette[1],
          high = color_palette[2],
          limits = range(spatial_df_plot["Residuals"][, 1]),
          name = "Residuals",
          guide = "colourbar"
        ) +
        ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                       axis.text = ggplot2::element_blank()) +
        ggplot2::coord_sf()
    }
    map


  }
})

### Output: plot and save -----

output$map_resid <- shiny::renderPlot({
  plot_map_resid()
}, bg = "transparent")


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
    ggplot2::ggsave(file, plot = plot_map_resid(), device = "pdf")
  }
)




