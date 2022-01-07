### Requested inputs ------

output$choose_time_var <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_var",
                            "Times to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})

### Creating plot ------

plot_dispersion <- shiny::reactive({
  data_plot <- data.frame("Response" = organized_data()$all$y_is,
                          "Dispersion" = organized_data()$all$dispersion)
  Disp_string <- ifelse(input$type_disp == "neff", "Effective Sample Size", "Variance")
  if(!is.null(smoothing_output())){
    data_plot$Dispersion <- smoothing_output()$disp
    Disp_string <- ifelse(input$var_type_smooth == "neff", "Effective Sample Size", "Variance")
  }

  if(!is.null(organized_data()$time)){
    if(is.null(input$select_time_var)){
      selected_times <- unique(organized_data()$time)
    }else{
      selected_times <- input$select_time_var
    }
    data_plot$Time <- organized_data()$time[!organized_data()$all$is_oos]
    if(!is.null(organized_data()$all$domain_size_n)){
      data_plot$size <- organized_data()$all$domain_size_n[!organized_data()$all$is_oos]
      data_plot <- data_plot[data_plot$Time %in% selected_times,]
      data_plot$Time <- factor(data_plot$Time)
      if(input$plot_var_kind == "y"){
        plot3 <- ggplot2::ggplot(data_plot,
                                 ggplot2::aes_(x = ~ Response, y = ~ Dispersion)) +
          ggplot2::ylab(Disp_string) + ggplot2::xlab(paste0(input$choice_resp)) +
          ggplot2::theme_bw(base_size = 15)+ ggplot2::geom_point(ggplot2::aes_(colour = ~ Time))+
          ggplot2::guides(colour=ggplot2::guide_legend(title=paste0(input$time_col))) + ggplot2::theme(aspect.ratio = 2/3)
      } else{
        plot3 <- ggplot2::ggplot(data_plot,
                                 ggplot2::aes_(x = ~ size, y = ~ Dispersion)) +
          ggplot2::ylab(Disp_string) + ggplot2::xlab("Area Sample Size") +
          ggplot2::theme_bw(base_size = 15)+ ggplot2::geom_point(ggplot2::aes_(colour = ~ Time))+
          ggplot2::guides(colour=ggplot2::guide_legend(title=paste0(input$time_col))) + ggplot2::theme(aspect.ratio = 2/3)
      }
    } else {
      data_plot <- data_plot[data_plot$Time %in% selected_times,]
      data_plot$Time <- factor(data_plot$Time)
      plot3 <- ggplot2::ggplot(data_plot,
                               ggplot2::aes_(x = ~ Response, y = ~ Dispersion)) +
        ggplot2::ylab(Disp_string) + ggplot2::xlab(paste0(input$choice_resp)) +
        ggplot2::theme_bw(base_size = 15)+ ggplot2::geom_point(ggplot2::aes_(colour = ~ Time))+
        ggplot2::guides(colour=ggplot2::guide_legend(title=paste0(input$time_col))) + ggplot2::theme(aspect.ratio = 2/3)
    }
  }else{
    if(!is.null(organized_data()$all$domain_size_n)){
      data_plot$size <- organized_data()$all$domain_size_n
      if(input$plot_var_kind == "y"){
        plot3 <- ggplot2::ggplot(data_plot,
                                 ggplot2::aes_(x = ~ Response, y = ~ Dispersion)) +
          ggplot2::ylab(Disp_string) + ggplot2::xlab(paste0(input$choice_resp)) +
          ggplot2::geom_point() +
          ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
      } else{
        plot3 <- ggplot2::ggplot(data_plot,
                                 ggplot2::aes_(x = ~ size, y = ~ Dispersion)) +
          ggplot2::ylab(Disp_string) + ggplot2::xlab("Area Sample Size") +
          ggplot2::geom_point() +
          ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
      }
    } else {
      plot3 <- ggplot2::ggplot(data_plot,
                               ggplot2::aes_(x = ~ Response, y = ~ Dispersion)) +
        ggplot2::ylab(Disp_string) + ggplot2::xlab(paste0(input$choice_resp)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
    }

  }
  if (input$plot_var_loess == "Yes") {
    plot3 + ggplot2::geom_smooth()
  } else {
    plot3
  }

})

### Output: plot and save -----

output$plot_var_xy <- shiny::renderPlot({
  plot_dispersion()
}, bg = "transparent")


output$download_dispersion <- shiny::downloadHandler(
  filename = 'tipsae_dispersion.RData',
  content = function(file) {
    tipsae_dispersion <- plot_dispersion()
    save(tipsae_dispersion, file = file)
  }
)

output$save_pdf_dispersion = shiny::downloadHandler(
  filename = "tipsae_dispersion.pdf",
    content = function(file) {
    tipsae_dispersion <- plot_dispersion()
    ggplot2::ggsave(file, plot = tipsae_dispersion, device = "pdf")
  }
)




