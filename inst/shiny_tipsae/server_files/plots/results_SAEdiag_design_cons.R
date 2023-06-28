### Requested inputs ------

# none

### Creating plot ------

plot_design_cons <- shiny::reactive({
  if(!is.null(res_model()$data_ris$size)){
    ggplot2::ggplot(data = res_model()$data_ris, ggplot2::aes(x = size, y = Residuals)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw(base_size = 15) +
      ggplot2::geom_point(
        shape = 20,
        size = 2.5,
        alpha = 0.7
      ) + ggplot2::ylab("Residuals") +
      ggplot2::xlab("Domain sample size") +
      ggplot2::theme(aspect.ratio = 2/3)

  }

  })

### Output: plot and save -----

output$design_cons <- shiny::renderPlot({
  plot_design_cons()
}, bg = "transparent")


output$download_design_cons <- shiny::downloadHandler(
  filename = 'tipsae_design_cons.RData',
  content = function(file) {
    tipsae_design_cons <- plot_design_cons()
    save(tipsae_design_cons, file = file)
  }
)

output$save_pdf_design_cons = shiny::downloadHandler(
  filename = "tipsae_design_cons.pdf",
  content = function(file) {
    ggplot2::ggsave(file, plot = plot_design_cons(), device = "pdf")
  }
)




