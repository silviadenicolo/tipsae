### Sources for plots -----

source("server_files/plots/data_summary_plot_distribution.R", local = TRUE)

source("server_files/plots/data_summary_plot_logit.R", local = TRUE)

source("server_files/plots/data_summary_plot_dispersion.R", local = TRUE)

source("server_files/plots/data_summary_plot_map.R", local = TRUE)

source("server_files/plots/data_summary_plot_traj.R", local = TRUE)

### Text outputs ------

# Size dataset
output$is_size <- shiny::renderText({
  if(input$time_eff == "No"){
    paste0(length(organized_data()$all$y_is), " observed domains.")
  }else{
    paste0(length(organized_data()$all$y_is), " observations from ",
           length(unique(organized_data()$all$domains_names)), " domains in ",
           length(unique(organized_data()$time))," distinct times.")
  }
})

# Out of samples
output$oos_size <- shiny::renderText({
  if (input$time_eff == "No") {
    paste0(sum(organized_data()$all$is_oos), " out-of-sample domains.")
  } else {
    paste0(sum(organized_data()$all$is_oos), " out-of-sample observations.")
  }
})

# Boundary values
output$zerones <- shiny::renderText({
  if (sum(organized_data()$all$y_is == 0 | organized_data()$all$y_is == 1) == 0) {
    paste0("0 observations equal to the boundaries (0 or 1).")
  }else{
    paste0(sum(organized_data()$all$y_is == 0 | organized_data()$all$y_is == 1),
           " observations equal to the boundaries (0 or 1). A model able to
             account for this feature must be chosen.")
  }

})

