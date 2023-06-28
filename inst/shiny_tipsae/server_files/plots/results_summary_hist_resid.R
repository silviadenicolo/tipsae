### Requested inputs ------

# none

### Creating plot ------

plot_hist_resid <- shiny::reactive({
  plot <- ggplot2::ggplot(data = res_model()$data_ris, ggplot2::aes(x = Residuals)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::xlab("Residuals") + ggplot2::ylab("Counts") +
    ggplot2::geom_histogram(bins = 30,
                            color = "black",
                            fill = "white")
  return(plot)
})

### Output: plot and save -----

output$hist_resid <- shiny::renderPlot({
  plot_hist_resid()
}, bg = "transparent")


output$download_hist_resid <- shiny::downloadHandler(
  filename = 'tipsae_hist_resid.RData',
  content = function(file) {
    tipsae_hist_resid <- plot_hist_resid()
    save(tipsae_hist_resid, file = file)
  }
)

output$save_pdf_hist_resid = shiny::downloadHandler(
  filename = "tipsae_hist_resid.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_hist_resid(), device = "pdf")
  }
)




