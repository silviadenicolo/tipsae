

#' Bayesian proportions/(0,1)-measures small area model with Stan
#'
#' @param x Object of class 'summary_fitsae'.
#' @return 'benchmark' object.
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


