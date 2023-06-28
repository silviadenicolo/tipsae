### Requested inputs ------

# none

### Creating plot ------

plot_dens_unstr <- shiny::reactive({
  data_reff_u <- data.frame(reff = scale(res_model()$summary$raneff$unstructured[, "mean"]))
  xlim_reff <- range(c(data_reff_u$reff, 2, -2))


  ggplot2::ggplot(data_reff_u, ggplot2::aes(x = reff)) +
    ggplot2::geom_function(fun = dnorm,  ggplot2::aes(color = "Standard normal")) +
    ggplot2::stat_density(ggplot2::aes(color = "Scaled random effects"),
                          geom = "line", position = "identity") +
    ggplot2::ylab("Density") + ggplot2::xlim(xlim_reff) +
    ggplot2::theme_bw(base_size = 15) + ggplot2::labs(color = "") +
    ggplot2::scale_color_manual(values = c(
      "Scaled random effects" = "black",
      "Standard normal" = "grey")) +
    ggplot2::xlab("Unstructured random effect") +
    ggplot2::theme(aspect.ratio = 2/3)
  })

### Output: plot and save -----

output$dens_unstr <- shiny::renderPlot({
  plot_dens_unstr()
}, bg = "transparent")


output$download_dens_unstr <- shiny::downloadHandler(
  filename = 'tipsae_dens_unstr.RData',
  content = function(file) {
    tipsae_dens_unstr <- plot_dens_unstr()
    save(tipsae_dens_unstr, file = file)
  }
)

output$save_pdf_dens_unstr = shiny::downloadHandler(
  filename = "tipsae_dens_unstr.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_dens_unstr(), device = "pdf")
  }
)




