### Requested inputs ------

# none

### Creating plot ------

plot_box_sdr <- shiny::reactive({
  data_plot <- data.frame(Sd=c(res_model()$data_ris$Mod_Est_SD, res_model()$data_ris$Sd_direct),
                          Est = rep(c("Model S.D.", "Direct S.D."), each = nrow(res_model()$data_ris)))
  ggplot2::ggplot(data = data_plot, ggplot2::aes(y = Sd, x= Est)) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::xlab("") + ggplot2::ylab("S.D.") +
    ggplot2::geom_boxplot(fill="white", ggplot2::aes(color = Est)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::scale_color_manual(values = c("Direct S.D." = "black", "Model S.D." =
                                             "steelblue")) + ggplot2::theme(aspect.ratio = 2/3)
})

### Output: plot and save -----

output$boxplot_sdr <- shiny::renderPlot({
  plot_box_sdr()
}, bg = "transparent")


output$download_box_sdr <- shiny::downloadHandler(
  filename = 'tipsae_box_sdr.RData',
  content = function(file) {
    tipsae_box_sdr <- plot_box_sdr()
    save(tipsae_box_sdr, file = file)
  }
)

output$save_pdf_box_sdr = shiny::downloadHandler(
  filename = "tipsae_box_sdr.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_box_sdr(), device = "pdf")
  }
)




