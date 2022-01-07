### Requested inputs ------

# none

### Creating plot ------

plot_cat_unstr <- shiny::reactive({
  data_reff_u <- data.frame(Domain = res_model()$summary$raneff$unstructured[, "Domains"],
                            means = res_model()$summary$raneff$unstructured[, "mean"],
                            q_L = res_model()$summary$raneff$unstructured[, 4],
                            q_U = res_model()$summary$raneff$unstructured[, 6])

  ggplot2::ggplot(data_reff_u, ggplot2::aes_(x = ~reorder(Domain, means))) +
    ggplot2::geom_point(ggplot2::aes_(y = ~ means)) +
    ggplot2::geom_linerange(ggplot2::aes_(ymin = ~ q_L, ymax = ~ q_U)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2) +
    ggplot2::ylab("Unstructured random effect") + ggplot2::xlab("") + ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))


  })

### Output: plot and save -----

output$cat_unstr <- shiny::renderPlot({
  plot_cat_unstr()
}, bg = "transparent")


output$download_cat_unstr <- shiny::downloadHandler(
  filename = 'tipsae_cat_unstr.RData',
  content = function(file) {
    tipsae_cat_unstr <- plot_cat_unstr()
    save(tipsae_cat_unstr, file = file)
  }
)

output$save_pdf_cat_unstr = shiny::downloadHandler(
  filename = "tipsae_cat_unstr.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_cat_unstr(), device = "pdf")
  }
)




