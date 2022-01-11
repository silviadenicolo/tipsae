### Requested inputs ------

output$choose_time_dist <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_dist",
                            "Times to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})

### Creating plot ------

plot_density_resp <- shiny::reactive({
  data_plot <- data.frame("y_obs" = organized_data()$all$y_is)
  if(!is.null(organized_data()$time)){
    if(is.null(input$select_time_dist)){
      selected_times <- unique(organized_data()$time)
    }else{
      selected_times <- input$select_time_dist
    }
    data_plot$Time <- organized_data()$time[!organized_data()$all$is_oos]
    data_plot <- data_plot[data_plot$Time %in% selected_times,]
    data_plot$Time <- factor(data_plot$Time)
    plot1 <- ggplot2::ggplot(data_plot,
                             ggplot2::aes_(x = ~ y_obs)) +
      ggplot2::xlab(paste0(input$choice_resp)) +
      ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
    if (input$plot_expl_dist_kind == "BP") {
      plot1 + ggplot2::geom_boxplot(ggplot2::aes_(y = ~ Time)) + ggplot2::ylab(paste0(input$time_col))
    } else {
      plot1 + ggplot2::geom_density(position = "identity", ggplot2::aes_(colour = ~ Time)) +
        ggplot2::ylab("Density")+ggplot2::guides(colour=ggplot2::guide_legend(title=paste0(input$time_col)))
    }
  } else {
    plot1 <- ggplot2::ggplot(data_plot,
                             ggplot2::aes_(x = ~ y_obs)) +
      ggplot2::xlab(paste0(input$choice_resp)) +
      ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
    if (input$plot_expl_dist_kind == "BP") {
      plot1 + ggplot2::geom_boxplot() + ggplot2::ylab("")
    } else {
      plot1 + ggplot2::geom_density(position = "identity") + ggplot2::ylab("Density")
    }
  }
})

### Output: plot and save -----

output$plot_expl_dist <- shiny::renderPlot({
  plot_density_resp()
}, bg = "transparent")


output$download_density_resp <- shiny::downloadHandler(
  filename = 'tipsae_density_resp.RData',
  content = function(file) {
    tipsae_density_resp <- plot_density_resp()
    save(tipsae_density_resp, file = file)
  }
)

output$save_pdf_density_resp = shiny::downloadHandler(
  filename = "tipsae_density_resp.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_density_resp(), device = "pdf")
  }
)




