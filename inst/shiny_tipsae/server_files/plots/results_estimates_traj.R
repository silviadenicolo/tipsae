### Requested inputs ------


# Domains choice
output$choose_domain_traj_estimate <- shiny::renderUI({
  shinyWidgets::pickerInput("select_domain_traj_estimate",
                            "Domains to include:",
                            choices = levels(organized_data()$all$domains_names),
                            selected = levels(organized_data()$all$domains_names),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})


### Observer ------


shiny::observe({
  if (!is.null(estimates()$out_of_sample)) {
    shinyjs::show(id = "oos_in_traj")
  }else{
    shinyjs::hide(id = "oos_in_traj")
  }
})

### Creating plot ------

plot_time_traj_estimate <- shiny::reactive({
  if (is.null(input$select_domain_traj_estimate)) {
    selected_domains <- levels(organized_data()$all$domains_names)
  }else{
    selected_domains <- input$select_domain_traj_estimate
  }
  data_sub <- estimates()$in_sample
  if (input$oos_in_traj == "Yes") {
    data_sub <- rbind(data_sub, estimates()$out_of_sample)
  }

  data_plot <- data.frame(Domain = data_sub$Domains,
                          time = data_sub$Times,
                          means = data_sub[, "HB est."])

  data_plot <- data_plot[data_plot$Domain %in% selected_domains,]
  plot2 <- ggplot2::ggplot(data_plot,
                           ggplot2::aes_(x = ~ time, y = ~ means, group = ~ Domain)) +
    ggplot2::ylab("H.B. Estimates") + ggplot2::xlab(paste0(input$time_col)) +
    ggplot2::scale_x_continuous(breaks = unique(data_plot$time)) +
    ggplot2::geom_line(ggplot2::aes_(colour = ~ Domain)) +
    ggplot2::geom_point(ggplot2::aes_(colour = ~ Domain)) +
    ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3) +
    ggplot2::guides(colour=ggplot2::guide_legend(title = paste0("Domain")))

  plot2
})

### Output: plot and save -----

output$plot_traj_estimate <- shiny::renderPlot({
  plot_time_traj_estimate()
}, bg = "transparent")


output$download_time_traj_estimate <- shiny::downloadHandler(
  filename = 'tipsae_time_traj.RData',
  content = function(file) {
    tipsae_time_traj_estimate <- plot_time_traj_estimate()
    save(tipsae_time_traj_estimate, file = file)
  }
)

output$save_pdf_time_traj_estimate = shiny::downloadHandler(
  filename = "tipsae_time_traj.pdf",
  content = function(file) {
    tipsae_time_traj_estimate <- plot_time_traj_estimate()
    ggplot2::ggsave(file, plot = tipsae_time_traj_estimate, device = "pdf")
  }
)




