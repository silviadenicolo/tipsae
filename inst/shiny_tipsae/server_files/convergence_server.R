### renderUI for Inputs -----
output$choose_var_diag <- shiny::renderUI({
  pars <- names(model_res$results$stanfit)
  pars <- pars[grepl("^beta", pars)|grepl("^sigma", pars)]
  shinyWidgets::pickerInput(
    "var_diag",
    "Parameter to check",
    choices = pars,
    selected = pars[1],
    multiple = FALSE
  )
})


## Output: visual/data outcomes -------

# Plot of R hat
output$plot_rhat <- shiny::renderPlot({
  pars <- names(model_res$results$stanfit)
  pars <- pars[grepl("^beta", pars) | grepl("^sigma", pars)]
  bayesplot::mcmc_rhat(bayesplot::rhat(model_res$results$stanfit, pars = pars), size = 2) +
    bayesplot::yaxis_text(hjust = 1) + ggplot2::theme_classic(base_size = 15)
}, bg = "transparent")

# Plot of the effective sample size
output$plot_neff <- shiny::renderPlot({
  pars <- names(model_res$results$stanfit)
  pars <- pars[grepl("^beta", pars) | grepl("^sigma", pars)]
  bayesplot::mcmc_neff(bayesplot::neff_ratio(model_res$results$stanfit, pars = pars), size = 2) +
    bayesplot::yaxis_text(hjust = 1) + ggplot2::theme_classic(base_size = 15)
}, bg = "transparent")

# Single diagnostic plot
output$plot_single <- shiny::renderPlot({
  pars <- names(model_res$results$stanfit)
  pars <- pars[grepl("^beta", pars) | grepl("^sigma", pars) | grepl("^lambda", pars)]

  if (is.null(input$var_diag)) {
    parameter <- pars[1]
  }else{
    parameter <- input$var_diag
  }
  if (is.null(input$kind_plot)) {
    plot_type <- "Trace-plot"
  }else{
    plot_type <- input$kind_plot
  }
  post_draws <- as.array(model_res$results$stanfit, pars = pars)
  if (plot_type == "Trace-plot") {
    plot_f <- bayesplot::mcmc_trace(x = post_draws, pars = parameter) +
      ggplot2::theme_classic(base_size = 15)
  }
  if (plot_type == "ACF") {
    plot_f <- bayesplot::mcmc_acf(x = post_draws, pars = parameter, lags = 20) +
      ggplot2::theme_classic(base_size = 15)
  }
  if (plot_type == "Density") {
    plot_f <- bayesplot::mcmc_dens_chains(x = post_draws, pars = parameter) +
      ggplot2::theme_classic(base_size = 15) + ggplot2::theme(aspect.ratio = 2/3)
  }
  if (plot_type == "Rank plots") {
    plot_f <- bayesplot::mcmc_rank_hist(x = post_draws, pars = parameter) +
      ggplot2::theme_classic(base_size = 15)
  }
  plot_f

}, bg = "transparent")
