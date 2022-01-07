### Create reactive objects --------

estimates <- shiny::reactive({
  if (is.null(res_model())) {
    return(NULL)
  } else {
    tipsae::extract(x = res_model()$summary)
  }
})


### Sources for plots -----

source("server_files/plots/results_estimates_map.R", local = TRUE)
source("server_files/plots/results_estimates_cat.R", local = TRUE)
source("server_files/plots/results_estimates_traj.R", local = TRUE)



### Observe expressions -------
shiny::observe({
  if (!is.null(estimates()$out_of_sample)) {
    shinyjs::show(id = "panel_tab_oos")
  }else{
    shinyjs::hide(id = "panel_tab_oos")
  }
})



### Text / table outputs ------

output$tab_is <- shiny::renderDataTable(
  options = list(pageLength = 10, scrollX = TRUE), {
    estimates()$in_sample
  })


output$tab_oos <- shiny::renderDataTable(
  options = list(pageLength = 10, scrollX = TRUE), {
    estimates()$out_of_sample
  })


output$download_estimates <- shiny::downloadHandler(
  filename = function(){"Model_estimates.csv"},
  content = function(fname){
    tipsae::export(x = estimates(), file = fname)

  }
)

