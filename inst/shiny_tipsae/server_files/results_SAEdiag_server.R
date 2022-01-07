### Create Reactive objects -----



### Observers ------


## Output: logical statements ------



### Sources for plots -----

source("server_files/plots/results_SAEdiag_shrinkage.R", local = TRUE)
source("server_files/plots/results_SAEdiag_density.R", local = TRUE)
source("server_files/plots/results_SAEdiag_hist_sdr.R", local = TRUE)
source("server_files/plots/results_SAEdiag_box_sdr.R", local = TRUE)
source("server_files/plots/results_SAEdiag_design_cons.R", local = TRUE)
### Text / table outputs ------

# Shrinking bound rate
output$sbr <- shiny::renderText({
  paste0("The Shrinking Bound rate (i.e. the proportion of model estimates included between the synthetic and direct estimates) is: ", res_model()$summary$shrink_rate)
})

# Summary standard deviatio reduction
output$tab_sdr <- shiny::renderTable(expr = {
  tab <- as.matrix(res_model()$summary$diag_summ["S.D. Reduction",])
  names_tab <- dimnames(tab)
  out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  dimnames(out) <- names_tab
  out
 }, rownames = TRUE, colnames = FALSE)

