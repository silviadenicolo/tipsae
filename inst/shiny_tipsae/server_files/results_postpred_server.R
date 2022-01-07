### Sources for plots -----

source("server_files/plots/results_postpred_hist_pval.R", local = TRUE)
source("server_files/plots/results_postpred_ppc.R", local = TRUE)


### Text / table outputs ------


output$tab_pval <- shiny::renderTable(expr = {
  tab <- as.matrix(res_model()$summary$diag_summ["Bayesian p-value",])
  names_tab <- dimnames(tab)
  out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  dimnames(out) <- names_tab
  out
}, rownames = TRUE, colnames = FALSE, digits = 4)



