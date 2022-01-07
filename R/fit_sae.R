# Save this file as `R/fit_sae.R`

#' Bayesian proportions/(0,1)-measures small area model with Stan
#'
#' @export
#' @param formula_fixed Linear regression formula at the linking level.
#' @param data Data frame containing direct estimates and covariates.
#' @param likelihood Selection between different likelihood assumptions at sampling level.
#' @param type Selection between different parametrizations.
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `fitsae` containing an object of class `stanfit`returned by `rstan::sampling`
#'
#'


fit_sae <- function(formula_fixed,
                    domains = NULL,
                    disp_direct,
                    type_disp = "neff", #c("neff", "var"),
                    domain_size = NULL,
                    data,
                    likelihood = "beta", # c("beta", "flexbeta", "Infbeta0","Infbeta1","Infbeta01","Infbeta0alt"),
                    prior_reff = "normal",  #c("normal", "t", "VG")
                    spatial_error = FALSE,
                    spatial_df = NULL,
                    temporal_error = FALSE,
                    temporal_variable = NULL,
                    adapt_delta = 0.95,
                    max_treedepth=10,
                    init="0",
                    ...) {

  prior_coeff <- "normal"
  p0_HorseShoe <- NULL

  call <- match.call()
  terms_formula <- terms(formula_fixed)

  # check parameters
  check_par_fit(formula_fixed,
                terms_formula,
                domains,
                disp_direct,
                type_disp,
                domain_size,
                data,
                likelihood,
                prior_reff,
                prior_coeff,
                p0_HorseShoe,
                spatial_error,
                spatial_df,
                temporal_error,
                temporal_variable,
                adapt_delta,
                max_treedepth,
                init)


  # creation data objects
  data_obj <- create_data(formula_fixed,terms_formula, data, domain_size, domains, disp_direct)

  # spatial error
  data_spatial <- arrange_spatial_structure(spatial_error, spatial_df, data_obj)

  # temporal error
  data_temporal <- arrange_temporal_structure(temporal_error, temporal_variable, data_obj, data)

  # check data objects
  check_data_fit(data_obj, likelihood, domain_size)

  # Creation stan data object
  standata <- list(
    y = data_obj$y_is,
    X = data_obj$X_scal[!data_obj$is_oos,],
    X_oos = data_obj$X_scal[data_obj$is_oos,],
    M_is = data_obj$M,
    M_oos = length(data_obj$y[data_obj$is_oos]),
    P = ncol(data_obj$X_scal),
    disp = data_obj$dispersion,
    prior_coeff = ifelse(prior_coeff == "normal", 0, 1),
    indices_is = data_obj$indices_is,
    indices_oos = data_obj$indices_oos
  )
  # adding intercepts of islands
  if (data_spatial$islands > 1) {
    standata$X <- cbind(data_spatial$intercept_islands[!data_obj$is_oos,], standata$X)
    standata$X_oos <- cbind(data_spatial$intercept_islands[data_obj$is_oos,], standata$X_oos)
    standata$P <- ncol(standata$X)
    data_obj$X_scal <- cbind(data_spatial$intercept_islands, data_obj$X_scal)
  }
  # adding temporal and spatial information
  data_spatial[["intercept_islands"]] <- NULL
  data_spatial[["islands"]] <- NULL
  standata <- c(standata, data_spatial, data_temporal)
  # consistent definition of matrix
  if (!is.matrix(standata$X_oos)) {
    standata$X_oos <- matrix(standata$X_oos, ncol = ncol(data_obj$X_scal))
  }
  if (!is.matrix(standata$X)) {
  standata$X <- matrix(standata$X, ncol = ncol(data_obj$X_scal))
  }
  ### creation dummy variables
  standata <- dummy_standata(standata, data_obj, terms_formula, type_disp,
                             likelihood, prior_reff, prior_coeff, p0_HorseShoe)



  # parameters of interest
  pars_interest <- target_parameters(standata, data_obj, likelihood, prior_reff)

  # fit model
  out <- rstan::sampling(
    stanmodels$global,
    data = standata,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    pars = pars_interest,
    init = init,
    ...
  )

  # list with model settings
  model_settings <- list(
    likelihood = likelihood,
    intercept = standata$intercept,
    type_disp = type_disp,
    prior_reff = prior_reff,
    spatial_error = spatial_error,
    temporal_error = temporal_error,
    spatio_temporal = standata$spatio_temporal
  )

  # adding time variable
  if (temporal_error) {
    data_obj[["times"]] <- data[, temporal_variable]
  }


  output <- list(model_settings = model_settings,
                 data_obj = data_obj,
                 stanfit = out,
                 pars_interest = pars_interest,
                 call = call)

  class(output) <- "fitsae"
  return(output)
}






