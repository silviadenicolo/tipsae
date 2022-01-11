### Requested inputs ------


# Domains choice
output$choose_domain_traj_raneff <- shiny::renderUI({
  shinyWidgets::pickerInput("select_domain_traj_raneff",
                            "Domains to include:",
                            choices = levels(organized_data()$all$domains_names),
                            selected = levels(organized_data()$all$domains_names),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})

### Creating plot ------

plot_time_traj_raneff <- shiny::reactive({
  if (is.null(input$select_domain_traj_raneff)) {
    selected_domains <- levels(organized_data()$all$domains_names)
  }else{
    selected_domains <- input$select_domain_traj_raneff
  }



  data_plot <- data.frame(Domain = res_model()$summary$raneff$temporal[, "Domains"],
                          time = res_model()$summary$raneff$temporal[, "Times"],
                          means = res_model()$summary$raneff$temporal[, "mean"])

  data_plot <- data_plot[data_plot$Domain %in% selected_domains,]
  plot2 <- ggplot2::ggplot(data_plot,
                           ggplot2::aes_(x = ~ time, y = ~ means, group = ~ Domain)) +
    ggplot2::ylab("Temporal Random Effect") + ggplot2::xlab(paste0(input$time_col)) +
    ggplot2::scale_x_continuous(breaks = unique(data_plot$time)) +
    ggplot2::geom_line(ggplot2::aes_(colour = ~ Domain)) +
    ggplot2::geom_point(ggplot2::aes_(colour = ~ Domain)) +
    ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3) +
    ggplot2::guides(colour=ggplot2::guide_legend(title = paste0("Domain")))

  plot2
})

### Output: plot and save -----

output$plot_traj_raneff <- shiny::renderPlot({
  plot_time_traj_raneff()
}, bg = "transparent")


output$download_time_traj_raneff <- shiny::downloadHandler(
  filename = 'tipsae_time_traj.RData',
  content = function(file) {
    tipsae_time_traj_raneff <- plot_time_traj_raneff()
    save(tipsae_time_traj_raneff, file = file)
  }
)

output$save_pdf_time_traj_raneff = shiny::downloadHandler(
  filename = "tipsae_time_traj.pdf",
    content = function(file) {
      tipsae_time_traj_raneff <- plot_time_traj_raneff()
    ggplot2::ggsave(file, plot = tipsae_time_traj_raneff, device = "pdf")
  }
)




