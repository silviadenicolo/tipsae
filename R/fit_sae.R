#' Fitting a Small Area Model
#'
#' `fit_sae()` is used to fit Beta-based small area models, such as the classical Beta, zero and/or one inflated Beta and Flexible Beta models. The random effect part can incorporate either a temporal and/or a spatial dependency structure devoted to the prior specification settings. In addition, different prior assumptions can be specified for the unstructured random effects, allowing for robust and shrinking priors and different parametrizations can be set up.
#'
#' @export
#' @param formula_fixed An object of class `"formula"` specifying the linear regression fixed part at the linking level.
#' @param data  An object of class `"data.frame"` containing all relevant quantities.
#' @param domains Data column name displaying the domain names. If `NULL` (default), the domains are denoted with a progressive number.
#' @param disp_direct Data column name displaying given values of sampling dispersion for each domain. In out-of-sample areas, dispersion must be `NA`.
#' @param type_disp Parametrization of the dispersion parameter. The choices are variance (`"var"`) or \eqn{\phi_d} + 1 (`"neff"`) parameter.
#' @param domain_size Data column name indicating domain sizes (optional). In out-of-sample areas, sizes must be `NA`.
#' @param likelihood Sampling likelihood to be used. The choices are `"beta"` (default), `"flexbeta"`, `"Infbeta0"`, `"Infbeta1"` and `"Infbeta01"`.
#' @param prior_reff Prior distribution of the unstructured random effect. The choices are: `"normal"`, `"t"`, `"VG"`.
#' @param spatial_error Logical indicating whether to include a spatially structured random effect.
#' @param spatial_df Object of class `SpatialPolygonsDataFrame` with the shapefile of the studied region. Required if `spatial_error = TRUE`.
#' @param domains_spatial_df Column name of the `spatial_df@data` object displaying the domain names. Required if `spatial_error = TRUE`.
#' @param temporal_error Logical indicating whether to include a temporally structured random effect.
#' @param temporal_variable Data column name indicating temporal variable. Required if `temporal_error = TRUE`.
#' @param scale_prior List with the values of the prior scales. 4 named elements must be provided: "Unstructured", "Spatial", "Temporal", "Coeff.". Default: all equal to 2.5.
#' @param adapt_delta HMC option: target average proposal acceptance probability. See \code{\link[rstan]{stan}} documentation.
#' @param max_treedepth HMC option: target average proposal acceptance probability. See \code{\link[rstan]{stan}} documentation.
#' @inheritParams rstan::sampling
#' @param ... Arguments passed to \code{\link[rstan]{sampling}} (e.g. iter, chains).
#' @return A list of class `fitsae` containing the following objects:
#' \describe{
#'   \item{`model_settings`}{A list summarizing all the assumptions of the model: sampling likelihood, presence of intercept, dispersion parametrization, random effects priors and possible structures.}
#'   \item{`data_obj`}{A list containing input objects including in-sample and out-of-sample relevant quantities.}
#'   \item{`stanfit`}{A `stanfit` object, outcome of \code{\link[rstan]{sampling}} function containing full posterior draws. For details, see \code{\link[rstan]{stan}} documentation.}
#'   \item{`pars_interest`}{A vector containing the names of parameters whose posterior samples are stored.}
#'   \item{`call`}{Image of the function call that produced the `fitsae` object.}
#' }
#' @seealso \code{\link[rstan]{sampling}} for sampler options and \code{\link{summary.fitsae}} for handling the output.
#'
#' @examples
#' library(tipsae)
#'
#' # loading toy cross sectional dataset
#' data("emilia_cs")
#'
#' # fitting a cross sectional model
#' fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
#'                     type_disp = "var", disp_direct = "vars", domain_size = "n",
#'                     # MCMC setting to obtain a fast example. Remove next line for reliable results.
#'                     chains = 1, iter = 300, seed = 0)
#'
#'
#' # Spatio-temporal model: it might require time to be fitted
#'\dontrun{
#' # loading toy panel dataset
#' data("emilia")
#' # loading the shapefile of the concerned areas
#' data("emilia_shp")
#'
#' # fitting a spatio-temporal model
#' fit_ST <- fit_sae(formula_fixed = hcr ~ x,
#'                   domains = "id",
#'                   disp_direct = "vars",
#'                   type_disp = "var",
#'                   domain_size = "n",
#'                   data = emilia,
#'                   spatial_error = TRUE,
#'                   spatial_df = emilia_shp,
#'                   domains_spatial_df = "NAME_DISTRICT",
#'                   temporal_error = TRUE,
#'                   temporal_variable = "year",
#'                   max_treedepth = 15,
#'                   seed = 0)
#'}
#'
#' @references
#'
#' \insertRef{janicki2020properties}{tipsae}
#'
#' \insertRef{carpenter2017stan}{tipsae}
#'
#' \insertRef{morris2019bayesian}{tipsae}
#'
#' \insertRef{DeNicolo2021}{tipsae}
#'
#'

fit_sae <- function(formula_fixed,
                    data,
                    domains = NULL,
                    disp_direct,
                    type_disp = c("neff", "var"),
                    domain_size = NULL,
                    likelihood = c("beta", "flexbeta", "Infbeta0","Infbeta1","Infbeta01"),
                    prior_reff = c("normal", "t", "VG"),
                    spatial_error = FALSE,
                    spatial_df = NULL,
                    domains_spatial_df = NULL,
                    temporal_error = FALSE,
                    temporal_variable = NULL,
                    scale_prior = list("Unstructured" = 2.5,
                                       "Spatial" = 2.5,
                                       "Temporal" = 2.5,
                                       "Coeff." = 2.5),
                    adapt_delta = 0.95,
                    max_treedepth=10,
                    init="0",
                    ...) {

  call <- match.call()

  type_disp <- match.arg(type_disp)
  likelihood <- match.arg(likelihood)
  prior_reff <- match.arg(prior_reff)

  prior_coeff <- "normal"
  p0_HorseShoe <- NULL

  # check parameters
  check_par_fit(formula_fixed,
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
                domains_spatial_df,
                temporal_error,
                temporal_variable,
                adapt_delta,
                max_treedepth,
                init)

  data <- as.data.frame(data)

  # check scale_prior
  check_scale_prior(scale_prior)

  # order dataset w.r.t. domain
  if (!is.null(domains)) {
    if (!temporal_error) {
      data <- data[order(data[,domains]), ]
    }else{# with temporal error: domain nested within times
      data <- data[order(data[,temporal_variable], data[,domains]), ]
    }
  }
  if (!is.null(spatial_df)) {
    spatial_df@data <- spatial_df@data[order(spatial_df@data[,domains_spatial_df]), ]
  }

  # creation data objects
  data_obj <- create_data(formula_fixed, data, domain_size, domains, disp_direct)

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
    M_is = data_obj$M_is,
    M_oos = length(data_obj$y[data_obj$is_oos]),
    P = ncol(data_obj$X_scal),
    disp = data_obj$dispersion,
    prior_coeff = ifelse(prior_coeff == "normal", 0, 1),
    indices_is = data_obj$indices_is,
    indices_oos = as.array(data_obj$indices_oos),
    intercept = data_obj$intercept,
    sigma_unstr = scale_prior[["Unstructured"]],
    sigma_spatial = scale_prior[["Spatial"]],
    sigma_temporal = scale_prior[["Temporal"]],
    sigma_coeff = scale_prior[["Coeff."]]
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
  standata <- dummy_standata(standata, data_obj, type_disp,
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

#' @export
#'

print.fitsae <- function(x, ...) {
  if (!inherits(x, "fitsae"))
  stop("Indicated object does not have 'fitsae' class.")
  cat("Results of the fitting process for SAE model \n")
  cat("\n")
  print(x$call)
  cat("\n")
  cat("--------------------------------------------------------------------------\n")
  cat("* Model likelihood:", x$model_settings$likelihood, "\n")
  cat("* Dispersion parameter type:", ifelse(x$model_settings$type_disp == "var", "variance", "effective sample size") , "\n")
  if (x$model_settings$spatio_temporal == 0) {
    cat("* Prior on unstructured random effects:", x$model_settings$prior_reff , "\n")
  }
  cat("* Spatial error:", as.logical(x$model_settings$spatial_error) , "\n")
  cat("* Temporal error:", as.logical(x$model_settings$temporal_error) , "\n")
  cat("--------------------------------------------------------------------------")
}




