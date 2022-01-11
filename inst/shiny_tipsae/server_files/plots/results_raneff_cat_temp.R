### Requested inputs ------

output$choose_time_cat <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_cat",
                            "Times to include",
                            choices = unique(res_model()$summary$raneff$temporal$Times),
                            selected = unique(res_model()$summary$raneff$temporal$Times)[1],
                            multiple = FALSE)
})

### Creating plot ------

plot_cat_temp <- shiny::reactive({
  if (is.null(input$select_time_cat)) {
    time_selected <- unique(res_model()$summary$raneff$temporal$Times)[1]
  }else{
    time_selected <- input$select_time_cat
  }
  data_reff_t_cat <- data.frame(Domain = res_model()$summary$raneff$temporal[, "Domains"],
                                time = res_model()$summary$raneff$temporal[, "Times"],
                                means = res_model()$summary$raneff$temporal[, "mean"],
                                q_L = res_model()$summary$raneff$temporal[, 5],
                                q_U = res_model()$summary$raneff$temporal[, 7])
  data_reff_t_cat <- data_reff_t_cat[data_reff_t_cat$time == time_selected, ]

  ggplot2::ggplot(data_reff_t_cat, ggplot2::aes_(x = ~ reorder(Domain, means))) +
    ggplot2::geom_point(ggplot2::aes_(y = ~ means)) +
    ggplot2::geom_linerange(ggplot2::aes_(ymin = ~ q_L, ymax = ~ q_U)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2) +
    ggplot2::ylab("Temporal random effect") + ggplot2::xlab("") + ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))


  })

### Output: plot and save -----

output$cat_temp <- shiny::renderPlot({
  plot_cat_temp()
}, bg = "transparent")


output$download_cat_temp <- shiny::downloadHandler(
  filename = 'tipsae_cat_temp.RData',
  content = function(file) {
    tipsae_cat_temp <- plot_cat_temp()
    save(tipsae_cat_temp, file = file)
  }
)

output$save_pdf_cat_temp <- shiny::downloadHandler(
  filename = "tipsae_cat_temp.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_cat_temp(), device = "pdf")
  }
)




