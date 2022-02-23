

#' Benchmarking Procedure for Model-Based Estimates
#'
#' The `benchmark()` function gives the chance to perform a benchmarking procedure on model-based estimates. Benchmarking could target solely the point estimates (single benchmarking) or, alternatively, also the ensemble variability (double benchmarking). Furthermore, an estimate of the overall posterior risk is provided, aggregated for all areas. This value is only yielded when in-sample areas are treated and a single benchmarking is performed.
#'
#' The function allows performing three different benchmarking methods, according to the argument method.  \itemize{\item The `"ratio"` and `"raking"` methods provide benchmarked estimates that minimize the posterior expectation of the weighted squared error loss, see Datta et al. (2011) and `tipsae` vignette.  \item The `"double"` method accounts for a further benchmark on the weighted ensemble variability, where `H` is a prespecified value of the estimators variability.}
#'
#' @inheritParams plot.summary_fitsae
#' @param bench A numeric value denoting the benchmark for the whole set of areas or a subset of areas.
#' @param share A numeric vector of areas weights, in case of proportions it denotes the population shares.
#' @param method The method to be specified among `"raking"`, `"ratio"` and `"double"`, see details.
#' @param H A numeric value denoting an additional benchmark, to be specified when the  `"double"` method is selected, corresponding to the ensemble variability.
#' @param time A character string indicating the time period to be considered, in case of temporal models, where a benchmark can be specified only for one time period at a time.
#' @param areas If `NULL` (default option), benchmarking is done on the whole set of areas, alternatively it can be done on a subset of them by indicating a vector containing the names of subset areas.
#' @return A `benchmark_fitsae` object being a list of the following elements:
#' \describe{
#'   \item{`bench_est`}{A vector including the benchmarked estimates for each considered domain.}
#'   \item{`post_risk`}{A numeric value indicating an estimate of the overall posterior risk, aggregated for all areas. This value is only yielded when in-sample areas are treated and a single benchmarking is performed.}
#'   \item{`method`}{The benchmarking method performed as selected in the input argument.}
#'   \item{`time`}{The time considered as selected in the input argument.}
#'   \item{`areas`}{The areas considered as selected in the input argument.}
#'   \item{`data_obj`}{A list containing input objects including in-sample and out-of-sample relevant quantities.}
#'   \item{`model_settings`}{A list summarizing all the assumptions of the model: sampling likelihood, presence of intercept, dispersion parametrization, random effects priors and possible structures.}
#'   \item{`model_estimates`}{Posterior summaries of target parameters for in-sample areas.}
#'   \item{`model_estimates_oos`}{Posterior summaries of target parameters for out-of-sample areas.}
#'   \item{`is_oos`}{Logical vector defining whether each domain is out-of-sample or not.}
#'   \item{`direct_est`}{Vector of direct estimates for in-sample areas.}
#' }
#'
#' @seealso \code{\link{summary.fitsae}} to produce the input object.
#'
#' @references
#' \insertRef{datta2011bayesian}{tipsae}
#'
#' @examples
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # fitting a model
#' fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
#'                     type_disp = "var", disp_direct = "vars", domain_size = "n",
#'                     # MCMC setting to obtain a fast example. Remove next line for reliable results.
#'                     chains = 1, iter = 300, seed = 0)
#'
#' # check model diagnostics
#' summ_beta <- summary(fit_beta)
#'
#' # creating a subset of the areas whose estimates have to be benchmarked
#' subset <- c("RIMINI", "RICCIONE", "RUBICONE", "CESENA - VALLE DEL SAVIO")
#'
#' # creating population shares of the subset areas
#' pop <- emilia_cs$pop[emilia_cs$id %in% subset]
#' shares_subset <- pop / sum(pop)
#'
#' # perform benchmarking procedure
#' bmk_subset <- benchmark(x = summ_beta,
#'                         bench = 0.13,
#'                         share = shares_subset,
#'                         method = "raking",
#'                         areas = subset)
#'
#' # check benchmarked estimates and posterior risk
#' bmk_subset$bench_est
#' bmk_subset$post_risk
#'
#'
#'
#'
#'
#' @export
#'
benchmark <- function(x,
                      bench,
                      share,
                      method = "raking", #c("raking", "ratio", "double")
                      H = NULL,
                      time = NULL,
                      areas = NULL) {

  # check parameters
  check_bench(x,
              bench,
              share,
              method,
              H,
              time,
              areas)


  if (!is.null(areas)) {
    a = (x$data_obj$domains_names %in% areas)
  } else {
    a = rep(T, length(x$data_obj$domains_names))
    }

  estim <- NA
  estim[which(x$is_oos == 0)] = x$post_means
  if (sum(x$is_oos) != 0)
    estim[which(x$is_oos == 1)] = x$model_estimates_oos[,"mean"]

  dom_names <- NA
  dom_names[which(x$is_oos == 0)] = x$model_estimates[,"Domains"]
  if (sum(x$is_oos) != 0)
    dom_names[which(x$is_oos == 1)] = x$model_estimates_oos[,"Domains"]

  post_var <- NA
  post_var[which(x$is_oos == 0)] = x$model_estimates[, "sd"] ^ 2

  direct_est <- NA
  direct_est[which(x$is_oos == 0)] = x$direct_est

  if (x$model_settings$temporal_error) {
    a <- a & (x$data_obj$times %in% time)
  }

  estim <- estim[a]
  post_var <- post_var[a]
  direct_est <- direct_est[a]
  dom_names <- dom_names[a]

  check_share(share, estim)

  bench_est <- rep(NA, length(estim))
  post_risk <- NULL


  if (method == "raking") {
      bench_est <- estim + bench - sum(share * estim)

      if (sum(x$is_oos[a]) == 0)
        post_risk <-
          sum(share  * (post_var  + (bench - sum(
            share * direct_est
          )) ^ 2))

    } else if (method == "ratio") {
      phi <- share / estim

      bench_est <- estim + (1 / (sum(share ^ 2 / phi))) *
        (bench - sum(share * estim)) * (share / phi)

      if (sum(x$is_oos[a]) == 0)
        post_risk <- sum(phi * (
          post_var + 1 / sum(share ^ 2 / phi) ^ 2 * (bench - sum(share * direct_est)) ^ 2  * (share /
                                                                                                    phi) ^ 2
        ))

    } else if (method == "double") {
      bench_est <- bench + (estim - sum(share * estim)) *
        sqrt(H / sum(share * (estim - sum(share * estim)) ^ 2))
    }

  names(bench_est) <- dom_names

  out <- list(bench_est = bench_est, # vettore
              method = method,
              post_risk = post_risk,
              time = time,
              areas = areas,
              data_obj = x$data_obj,
              model_settings = x$model_settings,
              model_estimates = x$model_estimates,
              model_estimates_oos = x$model_estimates_oos,
              is_oos = x$is_oos,
              direct_est = x$direct_est)

  class(out) <- "benchmark_fitsae"

  return(out)
}


check_bench<-function(x,
                      bench,
                      share,
                      method,
                      H,
                      time,
                      areas){


  if (class(x) != "summary_fitsae")
    stop("Indicated object does not have 'summary_fitsae' class.")

  if (is.null(method) || is.null(share) || is.null(bench))
    stop(" 'method', 'share' and 'bench' has to be prespecified. ")

  if (method == "double" && is.null(H))
    stop("Variability benchmark has to be specified in 'double' method.")

  if(!is.null(areas) && any(!(areas %in% x$data_obj$domains_names)))
    stop("'areas' does not corresponds to a subset of 'summary_fitsae' domain names, check saefit$data_obj$domains_names.")

  tp = unique(x$data_obj$times)

  if(x$model_settings$temporal_error && !(time %in% tp))
    stop("Time value inserted does not correspond with time values in object 'summary_fitsae'.")

  if(x$model_settings$temporal_error && length(time) != 1)
    stop("Only one time value can to be inserted.")

  }

check_share<-function(share,
                      estim){

  if (length(share)!=length(estim))
    stop("Share length differs from the total number of selected areas. ")

  if(sum(share)!=1){
    warning("'share' vector does not sum to 1, benchmark estimates may be unreliable.")

  }

}


