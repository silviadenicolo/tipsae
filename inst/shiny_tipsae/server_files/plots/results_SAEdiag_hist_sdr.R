### Requested inputs ------

# none

### Creating plot ------

plot_hist_sdr <- shiny::reactive({
  ggplot2::ggplot(data = res_model()$data_ris, ggplot2::aes(y = Sd_reduction)) +
    ggplot2::theme_bw(base_size = 15) + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::ylab("S.D. Reduction") + ggplot2::xlab("") +
    ggplot2::geom_boxplot(color = "black",
                            fill = "white")

})

### Output: plot and save -----

output$hist_sdr <- shiny::renderPlot({
  plot_hist_sdr()
}, bg = "transparent")


output$download_hist_sdr <- shiny::downloadHandler(
  filename = 'tipsae_hist_sdr.RData',
  content = function(file) {
    tipsae_hist_sdr <- plot_hist_sdr()
    save(tipsae_hist_sdr, file = file)
  }
)

output$save_pdf_hist_sdr = shiny::downloadHandler(
  filename = "tipsae_hist_sdr.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_hist_sdr(), device = "pdf")
  }
)




