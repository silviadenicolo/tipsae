### Requested inputs ------

output$choose_time_cov <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_cov",
                            "Times to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})

output$cov_plot <- shiny::renderUI({
  shiny::selectInput("cov_plot_expl",
                     "Covariate to show",
                     choices = colnames(organized_data()$all$X),
                     selected = colnames(organized_data()$all$X)[1],
                     width = '200px',
                     multiple = FALSE)
})

### Creating plot ------

plot_logit_cov <- shiny::reactive({
  if(is.null(input$cov_plot_expl)){
    cov_name <- colnames(organized_data()$all$X)[1]
  }else{
    cov_name <- input$cov_plot_expl
  }
  if(is.null(input$select_time_cov)){
    selected_times <- unique(organized_data()$time)
  }else{
    selected_times <- input$select_time_cov
  }

  data_plot <- data.frame("Logit" = log(organized_data()$all$y_is/(1 - organized_data()$all$y_is)),
                          "x" = organized_data()$data[!organized_data()$all$is_oos ,cov_name])

  if (!is.null(organized_data()$time)) {
    data_plot$Time <- organized_data()$time[!organized_data()$all$is_oos]
    data_plot <- data_plot[data_plot$Time %in% selected_times,]
    data_plot$Time <- factor(data_plot$Time)
    plot2 <- ggplot2::ggplot(data_plot,
                             ggplot2::aes(x = x, y = Logit)) +
      ggplot2::xlab(paste0(cov_name)) + ggplot2::ylab(paste0("Logit of ", input$choice_resp)) +
      ggplot2::geom_point(ggplot2::aes(colour = Time)) +
      ggplot2::theme_bw(base_size = 15) + ggplot2::guides(colour=ggplot2::guide_legend(title=paste0(input$time_col)))+ ggplot2::theme(aspect.ratio = 2/3)
    if (input$plot_expl_yx_loess == "Yes") {
      plot2 + ggplot2::geom_smooth()
    } else {
      plot2
    }
  } else{
    plot2 <- ggplot2::ggplot(data_plot,
                             ggplot2::aes(x = x, y = Logit)) +
      ggplot2::xlab(paste0(cov_name)) + ggplot2::ylab(paste0("Logit of ", input$choice_resp)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
    if (input$plot_expl_yx_loess == "Yes") {
      plot2 + ggplot2::geom_smooth()
    } else {
      plot2
    }

  }
})

### Output: plot and save -----

output$plot_expl_xy <- shiny::renderPlot({
  plot_logit_cov()
}, bg = "transparent")


output$download_logit <- shiny::downloadHandler(
  filename = 'tipsae_logit.RData',
  content = function(file) {
    tipsae_logit <- plot_logit_cov()
    save(tipsae_logit, file = file)
  }
)

output$save_pdf_logit = shiny::downloadHandler(
  filename = "tipsae_logit.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_logit_cov(), device = "pdf")
  }
)




