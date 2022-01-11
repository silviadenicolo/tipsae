### Requested inputs ------

# none

### Creating plot ------

plot_hist_pval <- shiny::reactive({
  ggplot2::ggplot(data = res_model()$data_ris, ggplot2::aes_(x = ~ bayes_pvalues)) +
    ggplot2::xlim(0, 1) + ggplot2::theme_bw(base_size = 15) +
    ggplot2::xlab("Bayesian p-values") + ggplot2::ylab("Counts") +
    ggplot2::geom_histogram(bins = 30,
                            color = "black",
                            fill = "white")
})

### Output: plot and save -----

output$hist_pval <- shiny::renderPlot({
  plot_hist_pval()
}, bg = "transparent")


output$download_hist_pval <- shiny::downloadHandler(
  filename = 'tipsae_hist_resid.RData',
  content = function(file) {
    tipsae_hist_pval <- plot_hist_pval()
    save(tipsae_hist_pval, file = file)
  }
)

output$save_pdf_hist_pval = shiny::downloadHandler(
  filename = "tipsae_hist_resid.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_hist_pval(), device = "pdf")
  }
)




