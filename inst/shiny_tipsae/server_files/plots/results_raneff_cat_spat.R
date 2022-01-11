### Requested inputs ------

# none

### Creating plot ------

plot_cat_spat <- shiny::reactive({
  data_reff_s <- data.frame(Domain = res_model()$summary$raneff$spatial[, "Domains"],
                            means = res_model()$summary$raneff$spatial[, "mean"],
                            q_L = res_model()$summary$raneff$spatial[, 4],
                            q_U = res_model()$summary$raneff$spatial[, 6])


  ggplot2::ggplot(data_reff_s, ggplot2::aes_(x = ~ reorder(Domain, means))) +
    ggplot2::geom_point(ggplot2::aes_(y = ~ means)) +
    ggplot2::geom_linerange(ggplot2::aes_(ymin = ~ q_L, ymax = ~ q_U)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2) +
    ggplot2::ylab("Spatial random effect") + ggplot2::xlab("") + ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))


  })

### Output: plot and save -----

output$cat_spat <- shiny::renderPlot({
  plot_cat_spat()
}, bg = "transparent")


output$download_cat_spat <- shiny::downloadHandler(
  filename = 'tipsae_cat_spat.RData',
  content = function(file) {
    tipsae_cat_spat <- plot_cat_spat()
    save(tipsae_cat_spat, file = file)
  }
)

output$save_pdf_cat_spat = shiny::downloadHandler(
  filename = "tipsae_cat_spat.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_cat_spat(), device = "pdf")
  }
)




