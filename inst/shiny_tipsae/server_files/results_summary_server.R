### Create Reactive objects -----

# results model
res_model <- shiny::eventReactive(input$compute_results,{
  #using summary
  cred_lev <- as.numeric(input$level_cred)
  out_summary <- summary(model_res$results, compute_loo = FALSE,
                         probs = c((1 - cred_lev) / 2, 0.5,
                                   cred_lev + (1 - cred_lev) / 2),
                         digits = input$digits)

  data <- data.frame(Domain = out_summary$data_obj$domains_names)
  if (out_summary$model_settings$temporal_error) {
    data$Time <- out_summary$data_obj$times
  }

  data$Direct <- out_summary$data_obj$y
  data$is_oos <- out_summary$is_oos
  if (is.null(out_summary$data_obj$domain_size_n)) {
    data$size <- NA
  }else{
    data$size[!out_summary$is_oos] <- out_summary$data_obj$domain_size_n
  }
  data[, c("Sd_direct", "Sd_reduction", "Mod_Est_Mean",
           "Mod_Est_SD", "Mod_Est_q1", "Mod_Est_q2",
           "C_Mod_Est_Mean", "C_Mod_Est_SD", "C_Mod_Est_q1",
           "C_Mod_Est_q2","Residuals","bayes_pvalues")] <- NA



  # sd
  data[!out_summary$is_oos, "Sd_direct"] <- out_summary$sd_dir
  data[!out_summary$is_oos, "Sd_reduction"] <- out_summary$sd_reduction
  # model estimates (only is)
  data[!out_summary$is_oos, c("Mod_Est_Mean", "Mod_Est_SD",
                              "Mod_Est_q1", "Mod_Est_q2")] <- out_summary$model_estimates[,!(colnames(out_summary$model_estimates) %in% c("Domains", "Times", "50%"))]
  # model estimates (with oos)
  data[ , c("C_Mod_Est_Mean", "C_Mod_Est_SD", "C_Mod_Est_q1", "C_Mod_Est_q2")] <- data[ , c("Mod_Est_Mean", "Mod_Est_SD", "Mod_Est_q1", "Mod_Est_q2")]
  data[out_summary$is_oos , c("C_Mod_Est_Mean", "C_Mod_Est_SD", "C_Mod_Est_q1", "C_Mod_Est_q2")] <- out_summary$model_estimates_oos[,!(colnames(out_summary$model_estimates_oos) %in% c("Domains", "Times", "50%"))]
  # residual
  data[!out_summary$is_oos, "Residuals"] <- out_summary$residuals
  # Bayesian p-values
  data[!out_summary$is_oos, "bayes_pvalues"] <- out_summary$bayes_pvalues


  return(list(summary = out_summary,
              data_ris = data))

})


# Computes LOOIC
LOOIC_out <- shiny::eventReactive(input$compute_LOOIC,{
  log_lik <- loo::extract_log_lik(model_res$results$stanfit, merge_chains = FALSE)
  rel_n_eff <- loo::relative_eff(exp(log_lik))
  loo::loo(log_lik, r_eff = rel_n_eff)
})

### Observers ------

# Display output LOOIC if available
shiny::observe({
  if (is.null(LOOIC_out())) {
    shinyjs::hide(id = "tab_LOOIC")
  }else{
    shinyjs::show(id = "tab_LOOIC")
  }
})

# Display choice times if required
shiny::observe({
  if (is.null(res_model()$data_ris$Time)) {
    shinyjs::hide(id = "button_time_resid")
  }else{
    shinyjs::show(id = "button_time_resid")
  }
})


## Output: logical statements ------

# Condition: TRUE if the results are computed
output$cond_res_show <- shiny::reactive({
  !is.null(res_model())
})
shiny::outputOptions(output, 'cond_res_show', suspendWhenHidden = FALSE)



### Sources for plots -----

source("server_files/plots/results_summary_hist_resid.R", local = TRUE)
source("server_files/plots/results_summary_map_resid.R", local = TRUE)


### Text / table outputs ------

# summary regression coefficients
output$tab_betas <- shiny::renderTable(expr = {
  cred_lev <- as.numeric(input$level_cred)
  tab <- res_model()$summary$fixed_coeff
  colnames(tab) <- c("Mean", "S.D.", paste0("Perc. ", 100 * (1 - cred_lev) / 2),
                     "Median", paste0("Perc. ", 100 * (cred_lev + (1 - cred_lev) / 2)))
  names_tab <- dimnames(tab)
  if(nrow(tab) == 1){
    out <- t(as.character(round(tab, digits = input$digits)))
  }else{
    out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  }
  dimnames(out) <- names_tab
  out
}, striped = TRUE, rownames = TRUE)

# Summary variance components
output$tab_SDs <- shiny::renderTable(expr = {
  cred_lev <- as.numeric(input$level_cred)
  tab <- res_model()$summary$var_comp
  colnames(tab) <- c("Mean", "S.D.", paste0("Perc. ", 100 * (1 - cred_lev) / 2),
                     "Median", paste0("Perc. ", 100 * (cred_lev + (1 - cred_lev) / 2)))
  names_tab <- dimnames(tab)
  if(nrow(tab) == 1){
    out <- t(as.character(round(tab, digits = input$digits)))
  }else{
    out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  }
  dimnames(out) <- names_tab
  out

}, striped = TRUE, rownames = TRUE)

# summary LOOIC
output$tab_LOOIC <- shiny::renderTable(expr = {
  tab <- LOOIC_out()$estimates
  names_tab <- dimnames(tab)
  out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  dimnames(out) <- names_tab
  out
}, rownames = TRUE)

# Summary residuals
output$tab_resid <- shiny::renderTable(expr = {
  tab <- as.matrix(res_model()$summary$diag_summ[1,])
  names_tab <- dimnames(tab)
  out <- apply(round(tab, digits = input$digits), MARGIN = 2, FUN = as.character)
  dimnames(out) <- names_tab
  out
}, rownames = TRUE, colnames = FALSE)




