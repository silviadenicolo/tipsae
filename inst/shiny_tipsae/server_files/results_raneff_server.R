### Create Reactive objects -----



### Observers ------


## Output: logical statements ------

# Condition: TRUE if an unstructured random effect is present
output$cond_unstr <- shiny::reactive({
  any(names(res_model()$summary$raneff) == "unstructured")
})
shiny::outputOptions(output, 'cond_unstr', suspendWhenHidden = FALSE)

# Condition: TRUE if a spatial random effect is present
output$cond_spat <- shiny::reactive({
  any(names(res_model()$summary$raneff) == "spatial")
})
shiny::outputOptions(output, 'cond_spat', suspendWhenHidden = FALSE)

# Condition: TRUE if a temporal random effect is present
output$cond_temp <- shiny::reactive({
  any(names(res_model()$summary$raneff) == "temporal")
})
shiny::outputOptions(output, 'cond_temp', suspendWhenHidden = FALSE)




### Sources for plots -----

source("server_files/plots/results_raneff_dens_unstr.R", local = TRUE)
source("server_files/plots/results_raneff_dens_spat.R", local = TRUE)
source("server_files/plots/results_raneff_dens_temp.R", local = TRUE)
source("server_files/plots/results_raneff_cat_unstr.R", local = TRUE)
source("server_files/plots/results_raneff_cat_spat.R", local = TRUE)
source("server_files/plots/results_raneff_cat_temp.R", local = TRUE)
source("server_files/plots/results_raneff_map_unstr.R", local = TRUE)
source("server_files/plots/results_raneff_map_spat.R", local = TRUE)
source("server_files/plots/results_raneff_map_temp.R", local = TRUE)
source("server_files/plots/results_raneff_traj_temp.R", local = TRUE)


### Text / table outputs ------






