### Create Reactive objects -----

# Object with estimable models
allowed_lik <- shiny::reactive({

  #dispersion type
  disp_type <- input$type_disp
  if (input$need_smooth == "Yes") {
    disp_type <- smoothing_output()$disp_type
  }

  # possible structured random effects
  str_reff <- "None"
  if (!is.null(map_shp_matching())) {
    str_reff <- c(str_reff, "Spatial")
  }
  if (!is.null(organized_data()$time)) {
    str_reff <-  "Temporal"
  }
  if (!is.null(organized_data()$time) && !is.null(map_shp_matching())) {
    str_reff <- c(str_reff, "Spatio-Temporal")
  }

  # likelihood and outputs
  if (sum(organized_data()$all$y_is %in% c(0,1)) != 0) {
    if (sum(organized_data()$all$y_is == 0) != 0 && sum(organized_data()$all$y_is == 1) != 0) {
      lik <- "0/1 Inflated Beta"
    }else{
      if (sum(organized_data()$all$y_is == 0) != 0 && sum(organized_data()$all$y_is == 1) == 0) {
        lik <- "0 Inflated Beta"
      }else{
        lik <- "1 Inflated Beta"
      }
    }
    if (disp_type == "var") {# no models
      return(NULL)
    }else{
      return(list(lik = lik, disp_type = disp_type, str_reff = str_reff))
    }
  }else{
    if (disp_type == "neff") {
      lik <- c("Beta")
    }else{
      lik <- c("Beta","Flexible Beta")
    }
    return(list(lik = lik, disp_type = disp_type, str_reff = str_reff))
  }
})


# Inits objects useful to show progress
temp_file <- tempfile(fileext = ".txt")
model_res <- reactiveValues(
  progress_mtime = -1
)


### Observer -----

# Show options for parallel computation
observe({
  if (input$multiple_chains) {
    shinyjs::show(id = "parallel_panel")
  }else{
    shinyjs::hide(id = "parallel_panel")
  }
})

# Show options for number of cores
observe({
  if (input$parallel) {
    shinyjs::show(id = "cores")
  }else{
    shinyjs::hide(id = "cores")
  }
})

# main code: fit the model in a parallel session
observeEvent(input$fit_model, {
  # likelihood
  model_res$results <- NULL
  cov_lik <- cbind(c("0/1 Inflated Beta","0 Inflated Beta", "1 Inflated Beta", "Beta", "Flexible Beta"),
                   c("Infbeta01","Infbeta0","Infbeta1","beta", "flexbeta"))
  lik_model <- cov_lik[cov_lik[,1] == input$lik_fit, 2]

  # dataset
  data_fit <- organized_data()$data
  if(organized_data()$need_smooth == "Yes"){
    data_fit[,input$choice_var] <- smoothing_output()$disp
  }
  domain_size <- NULL
  if(input$size_col != "Unspecified"){
    domain_size <- input$size_col
  }
  print(domain_size)
  domain_name <- NULL
  print(input$domain_col)
  if(input$domain_col != "Unspecified"){
    domain_name <- input$domain_col
  }
  spatial_error <- FALSE
  spatial_df <- NULL
  temporal_error <- FALSE
  temporal_variable <- NULL
  if(!is.null(allowed_lik()$str_reff)){
    if(input$str_reff_fit == "Spatial" || input$str_reff_fit == "Spatio-Temporal"){
      spatial_error <- TRUE
      spatial_df <- map_shp_matching()$sorted_spatial_df
    }
    if(input$str_reff_fit == "Temporal" || input$str_reff_fit == "Spatio-Temporal"){
      temporal_error <- TRUE
      temporal_variable <- input$time_col
    }
  }
  if(input$multiple_chains == FALSE){
    cores <- 1
    chains <- 1
  }else{
    cores <- input$cores
    chains <- input$chains
    if(input$parallel){
      options(mc.cores=cores)
      parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
    }

  }
  input_stan <- list(
    formula_fixed = organized_data()$formula,
    domains = domain_name,
    disp_direct = input$choice_var,
    type_disp = allowed_lik()$disp_type,
    domain_size = domain_size,
    data = data_fit,
    likelihood = lik_model,
    prior_reff = input$prior_reff,
    prior_coeff = "normal",
    spatial_error = spatial_error,
    spatial_df = spatial_df,
    temporal_error = temporal_error,
    temporal_variable = temporal_variable,
    adapt_delta = input$adapt_delta,
    max_treedepth = input$max_tree,
    chains = chains,
    iter = input$iter,
    cores = cores)
  model_res$process2 <- callr::r_bg(
    func = function(input_stan) {
      tipsae::fit_sae(
        formula_fixed = input_stan$formula_fixed,
        domains = input_stan$domains,
        disp_direct = input_stan$disp_direct,
        type_disp = input_stan$type_disp,
        domain_size = input_stan$domain_size,
        data = input_stan$data,
        likelihood = input_stan$likelihood,
        prior_reff = input_stan$prior_reff,  #c("normal", "t", "VG")
        prior_coeff = input_stan$prior_coeff, #c("normal", "HorseShoe")
        spatial_error = input_stan$spatial_error,
        spatial_df = input_stan$spatial_df,
        temporal_error = input_stan$temporal_error,
        temporal_variable = input_stan$temporal_variable,
        adapt_delta = input_stan$adapt_delta,
        max_treedepth = input_stan$max_treedepth,
        chains = input_stan$chains,
        iter = input_stan$iter,
        cores = input_stan$cores,
        open_progress = FALSE
      )
    },
    args = list(input_stan = input_stan),
    stdout = temp_file,
    supervise = TRUE
  )
  model_res$poll <- TRUE
})


## observe status of bg process
shiny::observe({
  shiny::req(model_res$process2, model_res$poll)
  shiny::invalidateLater(millis = 100, session)
  mtime <- file.info(temp_file)$mtime
  if (mtime > model_res$progress_mtime) {
    model_res$progress <- readLines(temp_file)
    model_res$progress_mtime <- mtime
  }
  if (!model_res$process2$is_alive()) {
    model_res$results <- model_res$process2$get_result()
    model_res$poll <- FALSE ## stop polling bg process
  }
})



### renderUI for Inputs -----

# Choice likelihood
output$choice_lik <- shiny::renderUI({
  shiny::radioButtons("lik_fit",
                      "Select the distributional assumption for your model:",
                      choices = allowed_lik()$lik,
                      width = '600px')
})

# Choice structured Random effects
output$choice_str_reff <- shiny::renderUI({
  shiny::radioButtons("str_reff_fit",
                      "Select an additional structured random effect to incorporate in the model:",
                      choices = allowed_lik()$str_reff,
                      width = '600px')
})

## Output: logical statements ------

# Condition: TRUE if at least one model can be fitted
output$cond_est1 <- shiny::reactive({
  print(!is.null(allowed_lik()))
  !is.null(allowed_lik())
})
shiny::outputOptions(output, 'cond_est1', suspendWhenHidden = FALSE)

# Condition: TRUE if at least one structured random effect can be chosen
output$cond_est2 <- shiny::reactive({
  !is.null(allowed_lik()$str_reff)
})
shiny::outputOptions(output, 'cond_est2', suspendWhenHidden = FALSE)

# Condition: TRUE if the progress is not NULL
output$cond_model_fitting <- shiny::reactive({
  !is.null(model_res$progress)
})
shiny::outputOptions(output, 'cond_model_fitting', suspendWhenHidden = FALSE)

# Condition: TRUE if there is a fitted model
output$cond_model_fitted <- shiny::reactive({
  !is.null(model_res$results)
})
shiny::outputOptions(output, 'cond_model_fitted', suspendWhenHidden = FALSE)


## Output: visual/data outcomes -------

# Warning if no models can be fitted
output$warning_fit1 <- shiny::renderText({
  "<font color=\"#d9534f\"><b>Warning!</b></font> Yor data contains 0s or 1s:
  you need a zero/one inflated Beta model.\n
    It requires the 'Effective sample size' dispersion parameter: if you don't
    have it, you can use the smoothing procedure starting from your variances."
})

# Message about the divergent transitions
output$show_div <- shiny::renderPrint({
  if (!is.null(model_res$results$stanfit))
    cat(paste(capture.output(rstan::check_divergences(model_res$results$stanfit),
                             type = "message"), collapse = " "))

})

# Message about the tree depth
output$show_tree <- shiny::renderPrint({
  if (!is.null(model_res$results$stanfit))
    cat(paste(capture.output(rstan::check_treedepth(model_res$results$stanfit),
                             type = "message"), collapse = " "))

})

## print progress
output$show_prog <- shiny::renderText({
  shiny::req(model_res$progress)
  paste(model_res$progress, collapse = "\n")
})


