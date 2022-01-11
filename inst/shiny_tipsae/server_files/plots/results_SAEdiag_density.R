### Requested inputs ------

# none

### Creating plot ------

plot_density <- shiny::reactive({
  ggplot2::ggplot(res_model()$data_ris) +
    ggplot2::stat_density(ggplot2::aes_(x = ~ Direct, color = "Direct est."),
                          geom = "line", position = "identity") +
    ggplot2::stat_density(ggplot2::aes_(x = ~ Mod_Est_Mean, color = "Model est."),
                          geom = "line", position = "identity") +
    ggplot2::theme_bw(base_size = 15) + ggplot2::xlab("Estimates") +
    ggplot2::ylab("Density") + ggplot2::labs(color = "") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_color_manual(values = c("Direct est." = "black", "Model est." =
                                             "steelblue"))
})

### Output: plot and save -----

output$plot_density <- shiny::renderPlot({
  plot_density()
}, bg = "transparent")


output$download_density <- shiny::downloadHandler(
  filename = 'tipsae_density.RData',
  content = function(file) {
    tipsae_density <- plot_density()
    save(tipsae_density, file = file)
  }
)

output$save_pdf_density = shiny::downloadHandler(
  filename = "tipsae_density.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_density(), device = "pdf")
  }
)




