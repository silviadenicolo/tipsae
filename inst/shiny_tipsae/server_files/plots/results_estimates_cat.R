### Requested inputs ------

output$time_cat_estimates <- shiny::renderUI({
  shinyWidgets::pickerInput("select_time_cat_estimates",
                            "Time to include",
                            choices = unique(organized_data()$time),
                            selected = unique(organized_data()$time)[1],
                            multiple = FALSE)
})

shiny::observe({
  if (!is.null(estimates()$out_of_sample)) {
    shinyjs::show(id = "oos_in_cat")
  }else{
    shinyjs::hide(id = "oos_in_cat")
  }
})



### Creating plot ------

plot_cat_estimates <- shiny::reactive({

  if(!is.null(organized_data()$time)){
    if(is.null(input$select_time_cat_estimates)){
      selected_times <- unique(organized_data()$time)[1]
    }else{
      selected_times <- input$select_time_cat_estimates
    }


    data_sub <- estimates()$in_sample[estimates()$in_sample$Times == selected_times,]
    pos_quant <- c(6, 8)
  }else{
    data_sub <- estimates()$in_sample
    pos_quant <- c(5, 7)
  }
    if (input$oos_in_cat == "Yes") {
      if (!is.null(organized_data()$time)) {
        data_oos <- estimates()$out_of_sample[estimates()$out_of_sample$Times == selected_times, ]
      }else{
        data_oos <- estimates()$out_of_sample
      }

      data_sub <- rbind(data_sub, data_oos)
    }

  data_reff_t_cat <- data.frame(Domain = data_sub$Domains,
                                means = data_sub[, "HB est."],
                                q_L = data_sub[, pos_quant[1]],
                                q_U = data_sub[, pos_quant[2]])


  ggplot2::ggplot(data_reff_t_cat, ggplot2::aes_(x = ~ reorder(Domain, means))) +
    ggplot2::geom_point(ggplot2::aes_(y = ~ means)) +
    ggplot2::geom_linerange(ggplot2::aes_(ymin = ~ q_L, ymax = ~ q_U)) +
    ggplot2::ylab("H.B. Estimates") + ggplot2::xlab("") + ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))



})

### Output: plot and save -----

output$plot_cat_estimates <- shiny::renderPlot({
  plot_cat_estimates()
}, bg = "transparent")


output$download_cat_estimates <- shiny::downloadHandler(
  filename = 'tipsae_map_estimates.RData',
  content = function(file) {
    tipsae_cat_estimates <- plot_cat_estimates()
    save(tipsae_cat_estimates, file = file)
  }
)

output$save_pdf_cat_estimates = shiny::downloadHandler(
  filename = "tipsae_map_estimates.pdf",
    content = function(file) {
    tipsae_cat_estimates <- plot_cat_estimates()
    ggplot2::ggsave(file, plot = tipsae_cat_estimates, device = "pdf")
  }
)




