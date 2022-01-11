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

    spatial_df_plot <- merge(map_shp_matching()$spatial_df_tidy,
                             data_sub,
                             by.x = "id",
                             by.y = input$domain_col)

    map <- ggplot2::ggplot(spatial_df_plot,
                           ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                         fill  = spatial_df_plot[name_var][, 1])) +
      ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_gradient(
        low = color_palette[1],
        high = color_palette[2],
        limits = range(spatial_df_plot[name_var][, 1]),
        guide = "colourbar"
      ) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank()) +
      ggplot2::coord_sf()

  }else{
    map <- ggplot2::ggplot(map_shp_matching()$spatial_df_tidy,
                           ggplot2::aes_(x = ~ long, y = ~ lat, group = ~ group,
                                         fill  = map_shp_matching()$spatial_df_tidy[name_var][, 1])) +
      ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme_bw() +
      ggplot2::scale_fill_gradient(
        low = color_palette[1],
        high = color_palette[2],
        limits = range(map_shp_matching()$spatial_df_tidy[name_var][, 1]),
        name = name_var,
        guide = "colourbar"
      ) +
      ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank()) +
      ggplot2::coord_sf()

  }


  map
})

### Output: plot and save -----

output$plot_map_expl <- shiny::renderPlot({
  plot_map_summary()
}, bg = "transparent")


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
    ggplot2::ggsave(file, plot = tipsae_map_summary, device = "pdf")
  }
)




