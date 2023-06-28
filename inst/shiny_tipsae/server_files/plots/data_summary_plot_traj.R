### Requested inputs ------

# Varible choice
output$choice_traj_quantity_expl <- shiny::renderUI({
  shiny::radioButtons("traj_quantity_expl",
                      "Variable to plot in the trajectories:",
                      choices = c(input$choice_resp, input$choice_cov),
                      width = '200px')
})

# Domains choice
output$choose_domain_traj <- shiny::renderUI({
  shinyWidgets::pickerInput("select_domain_traj",
                            "Domains to include:",
                            choices = levels(organized_data()$all$domains_names),
                            selected = levels(organized_data()$all$domains_names),
                            options = list(`actions-box` = TRUE),
                            multiple = TRUE)
})

### Creating plot ------

plot_time_traj <- shiny::reactive({
  if (is.null(input$select_domain_traj)) {
    selected_domains <- levels(organized_data()$all$domains_names)
  }else{
    selected_domains <- input$select_domain_traj
  }
  if (is.null(input$traj_quantity_expl)) {
    name_var <- input$choice_resp
  }else{
    name_var <- input$traj_quantity_expl
  }

  data_plot <- organized_data()$data[,c(input$choice_resp, input$choice_cov, input$time_col)]
  data_plot$Domain <- organized_data()$all$domains_names

  data_plot <- data_plot[data_plot$Domain %in% selected_domains,]
  plot2 <- ggplot2::ggplot(data_plot,
                           ggplot2::aes(x = .data[[input$time_col]], y = .data[[name_var]]),
                           ggplot2::aes(group = Domain)) +
    ggplot2::ylab(paste0(name_var)) + ggplot2::xlab(paste0(input$time_col)) +
    ggplot2::scale_x_continuous(breaks = unique(data_plot[,input$time_col])) +
    ggplot2::geom_line(ggplot2::aes(colour = Domain)) +
    ggplot2::geom_point(ggplot2::aes(colour = Domain)) +
    ggplot2::theme_bw(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3) +
    ggplot2::guides(colour=ggplot2::guide_legend(title = paste0("Domain")))

  plot2
})

### Output: plot and save -----

output$plot_traj_expl <- shiny::renderPlot({
  plot_time_traj()
}, bg = "transparent")


output$download_time_traj <- shiny::downloadHandler(
  filename = 'tipsae_time_traj.RData',
  content = function(file) {
    tipsae_time_traj <- plot_time_traj()
    save(tipsae_time_traj, file = file)
  }
)

output$save_pdf_time_traj = shiny::downloadHandler(
  filename = "tipsae_time_traj.pdf",
    content = function(file) {
      tipsae_time_traj <- plot_time_traj()
    ggplot2::ggsave(file, plot = tipsae_time_traj, device = "pdf")
  }
)




