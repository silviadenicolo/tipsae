### Create Reactive objects -----

smoothing_output <- shiny::reactive({
  if (organized_data()$need_smooth == "Yes") {
    out_smoothing <- tipsae::smoothing(
      data = organized_data()$data,
      direct_estimates = input$choice_resp,
      raw_variance = input$choice_var,
      areas_sample_sizes = input$size_col,
      method = input$smooth_type
    )
    if (input$var_type_smooth == "neff") {
      return(list(disp = out_smoothing$phi,
                  disp_type = "neff",
                  reg = out_smoothing$regression))
    }else{
      return(list(disp = out_smoothing$vars,
                  disp_type = "var",
                  reg = out_smoothing$regression))
    }
  }else{
    return(NULL)
  }
})

## Output: logical statements ------

## Condition: TRUE if no smoothing procedure is requestes
output$cond_no_smooth <- shiny::reactive({
  if (!is.null(organized_data())) {
    organized_data()$need_smooth == "No"
  }
})
shiny::outputOptions(output, 'cond_no_smooth', suspendWhenHidden = FALSE)

## Condition: True if the smoothing procedure provides a result
output$cond_smoothing_ok <- shiny::reactive({
  !is.null(smoothing_output()) || organized_data()$need_smooth == "No"
})
shiny::outputOptions(output, 'cond_smoothing_ok', suspendWhenHidden = FALSE)


## Output: plot ------


output$plot_smoothing <- shiny::renderPlot({
  regdata <- data.frame(GVF = (organized_data()$data[ ,input$choice_resp] *
                               (1 - organized_data()$data[ ,input$choice_resp])) /
                          organized_data()$data[,input$choice_var],
                        n = organized_data()$data[,input$size_col])
  ggplot2::ggplot(regdata, ggplot2::aes_(x = ~ n, y = ~ GVF)) + ggplot2::theme_bw(base_size = 15)  +
    ggplot2::geom_point() +  ggplot2::geom_abline(intercept = 0, slope = smoothing_output()$reg$coefficients) +
    ggplot2::ylab("Generalized Variance Function")
}, bg = "transparent")


output$bp_smoothing <- shiny::renderPlot({
  dispdata <- data.frame(disp = smoothing_output()$disp)
  plot <- ggplot2::ggplot(dispdata, ggplot2::aes_(y = ~ disp)) + ggplot2::theme_bw(base_size = 15)  +
    ggplot2::geom_boxplot() + ggplot2::theme(axis.ticks = ggplot2::element_blank())
  if (smoothing_output()$disp_type == "neff") {
    plot + ggplot2::ylab("Effective Sample Size")
  }else{
    plot + ggplot2::ylab("Variance")
  }

}, bg = "transparent")

