### Requested inputs ------

# none

### Creating plot ------

plot_ppc <- shiny::reactive({
  if (input$kind_ppc == "Density") {
    n_dens <- min(c(nrow(res_model()$y_pred), 25))
    plot_out <- bayesplot::ppc_dens_overlay(y = res_model()$summary$direct_est,
                                          yrep =  res_model()$summary$y_rep[1:n_dens, ]) +
      ggplot2::theme_classic(base_size = 15)
  }
  if (input$kind_ppc == "Mean") {
    plot_out <- bayesplot::ppc_stat(y = res_model()$summary$direct_est,
                                  yrep =  res_model()$summary$y_rep,
                                  stat = "mean") + ggplot2::theme_classic(base_size = 15)
  }
  if (input$kind_ppc == "S.D.") {
    plot_out <- bayesplot::ppc_stat(y = res_model()$summary$direct_est,
                                  yrep =  res_model()$summary$y_rep,
                                  stat = "sd") + ggplot2::theme_classic(base_size = 15)
  }
  plot_out + ggplot2::theme(aspect.ratio = 2/3)
})

### Output: plot and save -----

output$ppc_post <- shiny::renderPlot({
  plot_ppc()
}, bg = "transparent")


output$download_ppc <- shiny::downloadHandler(
  filename = 'tipsae_ppc.RData',
  content = function(file) {
    tipsae_ppc <- plot_ppc()
    save(tipsae_ppc, file = file)
  }
)

output$save_pdf_ppc = shiny::downloadHandler(
  filename = "tipsae_ppc.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_ppc(), device = "pdf")
  }
)




