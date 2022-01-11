### Requested inputs ------

# none

### Creating plot ------

plot_shrinkage <- shiny::reactive({
  lims_axis <- range(c(res_model()$summary$direct_est, res_model()$summary$post_means))
  ggplot2::ggplot(data = res_model()$data_ris, ggplot2::aes_(x = ~ Direct, y = ~ Mod_Est_Mean)) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::xlim(lims_axis) + ggplot2::ylim(lims_axis) +
    ggplot2::theme(aspect.ratio = 1) +
    ggplot2::ylab("Model estimates") +
    ggplot2::xlab("Direct estimates") +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::geom_point(
      shape = 20,
      size = 2.5,
      alpha = 0.7
    )
})

### Output: plot and save -----

output$plot_shrinkage <- shiny::renderPlot({
  plot_shrinkage()
}, bg = "transparent")


output$download_shrinkage <- shiny::downloadHandler(
  filename = 'tipsae_shrinkage.RData',
  content = function(file) {
    tipsae_shrinkage <- plot_shrinkage()
    save(tipsae_shrinkage, file = file)
  }
)

output$save_pdf_shrinkage = shiny::downloadHandler(
  filename = "tipsae_shrinkage.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_shrinkage(), device = "pdf")
  }
)




