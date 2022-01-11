source("utils_server.R", local = TRUE)

server <- function(input, output, session) {

  ### Main page 1: data ------------------------

  #### LOAD DATA ----------------------------------
  source("server_files/data_load_server.R", local = TRUE)

  #### SMOOTHING DISPERSION PARAMETERS ----------------------------------
  source("server_files/data_smoothing_server.R", local = TRUE)

  #### LOADING SPATIAL STRUCTURE ----------------------------------
  source("server_files/data_shapefile_server.R", local = TRUE)

  #### EXPLORATORY OUTPUTS -----------------------------
  source("server_files/data_summary_server.R", local = TRUE)


  ### Main page 2: fit model ------------------------
  source("server_files/fit_server.R", local = TRUE)

  ### Main page 3: assess convergence ------------------------
  source("server_files/convergence_server.R", local = TRUE)

  ### Main page 3: assess convergence ------------------------

  #### SUMMARY ----------------------------------
  source("server_files/results_summary_server.R", local = TRUE)

  #### POSTERIOR PRED ----------------------------------
  source("server_files/results_postpred_server.R", local = TRUE)

  #### SAE diagnostics ------
  source("server_files/results_SAEdiag_server.R", local = TRUE)

  #### Random effects --------
  source("server_files/results_raneff_server.R", local = TRUE)

  #### Model Estimates -------
  source("server_files/results_estimates_server.R", local = TRUE)






}
