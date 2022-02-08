#' Summary Method for `fitsae` Objects
#'
#' Summarizing the small area model fitting through the distributions of estimated parameters and derived diagnostics using posterior draws.
#'
#' If printed, the produced summary displays: \itemize{\item Posterior summaries about the fixed effect coefficients and the scale parameters related to unstructured and possible structured random effects. \item Model diagnostics summaries of (a) model residuals; (b) standard deviation reductions; (c) Bayesian P-values obtained with the MCMC samples. \item Shrinking Bound Rate. \item \code{\link[loo]{loo}} information criteria and related diagnostics from the `loo` package.}
#'
#'
#' @param object An instance of class `fitsae`.
#' @param compute_loo Logical, indicating whether to compute \code{\link[loo]{loo}} diagnostics or not.
#' @param ... Currently unused.
#' @inheritParams rstan::`summary,stanfit-method`
#' @return A list of class `summary_fitsae` containing diagnostics objects:
#' \describe{
#'   \item{`raneff`}{A list of `data.frame` objects storing the random effects posterior summaries divided for each type: `$unstructured`, `$temporal`, and `$spatial`.}
#'   \item{`fixed_coeff`}{Posterior summaries of fixed coefficients.}
#'   \item{`var_comp`}{Posterior summaries of model variance parameters.}
#'   \item{`model_estimates`}{Posterior summaries of the parameter of interest \eqn{\theta_d} for each in-sample domain \eqn{d}.}
#'   \item{`model_estimates_oos`}{Posterior summaries of the parameter of interest \eqn{\theta_d} for each out-of-sample domain \eqn{d}.}
#'   \item{`is_oos`}{Logical vector defining whether each domain is out-of-sample or not.}
#'   \item{`direct_est`}{Vector of input direct estimates.}
#'   \item{`post_means`}{Model-based estimates, i.e. posterior means of the parameter of interest \eqn{\theta_d} for each domain \eqn{d}.}
#'   \item{`sd_reduction`}{Standard deviation reduction, see details section.}
#'   \item{`sd_dir`}{Standard deviation of direct estimates, given as input if `type_disp="var"`.}
#'   \item{`loo`}{The object of class `loo`, for details see `loo` package documentation. }
#'   \item{`shrink_rate`}{Shrinking Bound Rate, see details section.}
#'   \item{`residuals`}{Residuals related to model-based estimates.}
#'   \item{`bayes_pvalues`}{Bayesian p-values obtained via MCMC samples, see details section.}
#'   \item{`y_rep`}{An array with values generated from the posterior predictive distribution, enabling the implementation of posterior predictive checks.}
#'   \item{`diag_summ`}{Summaries of residuals, standard deviation reduction and Bayesian p-values across the whole domain set.}
#'   \item{`data_obj`}{A list containing input objects including in-sample and out-of-sample relevant quantities.}
#'   \item{`model_settings`}{A list summarizing all the assumptions of the input model: sampling likelihood, presence of intercept, dispersion parametrization, random effects priors and possible structures.}
#'   \item{`call`}{Image of the function call that produced the input `fitsae` object.}
#' }
#'
#' @seealso \code{\link{fit_sae}} to estimate the model and the generic methods \code{\link{plot.summary_fitsae}} and \code{\link{density.summary_fitsae}}, and functions \code{\link{map}}, \code{\link{benchmark}} and \code{\link{extract}}.
#'
#' @references
#'
#' \insertRef{janicki2020properties}{tipsae}
#'
#' \insertRef{vehtari2017practical}{tipsae}
#'
#' @examples \donttest{
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # fitting a model
#' fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
#'                     type_disp = "var", disp_direct = "vars", domain_size = "n",
#'                     seed = 0)
#'
#' # check model diagnostics via summary() function
#' summ_beta <- summary(fit_beta)
#' summ_beta}
#' @export
#'
summary.fitsae <-function(object,
           probs = c(0.025, 0.25, 0.50, 0.75, 0.975),
           compute_loo = TRUE,
           ...) {

    #`%!in%` <- Negate(`%in%`)
    if (class(object) != "fitsae") stop("Indicated object does not have 'fitsae' class.")
    #extracting model-based estimates of the main parameters
    pars_summary <- object$pars_interest[!(object$pars_interest %in% c("log_lik"))]
    smry <- rstan::summary(object$stanfit, pars = pars_summary, probs = probs)$summary
    par <- rownames(smry)
    col_take <- colnames(smry)[!(colnames(smry) %in% c("se_mean", "n_eff", "Rhat"))]
    # posterior summaries coefficients fixed effects
    mb_int <- NULL
    # intercept
    if (object$model_settings$intercept == 1) {
      mb_int <- par[grepl("^beta0", par)]
      post_summ_int <- smry[mb_int, col_take]
      post_summ_int <- matrix(post_summ_int, nrow = 1,
                             dimnames = list("(Intercept)", names(post_summ_int) ))
    }
    # regressors
    mb_est <- par[grepl("^beta", par)]
    if (!is.null(mb_int)) {
      mb_est <- mb_est[!(mb_est %in% mb_int)]
    }
    if (length(mb_est) > 1) {
      post_summ_coeff <- smry[mb_est, col_take]
      rownames(post_summ_coeff) <- colnames(object$data_obj$X_scal)
    }else{
      post_summ_coeff <- smry[mb_est, col_take]
      post_summ_coeff <- matrix(post_summ_coeff, nrow = 1,
                               dimnames = list(colnames(object$data_obj$X_scal),
                                               names(post_summ_coeff)))
    }
    if (!is.null(mb_int)) {
      post_summ_coeff <- rbind(post_summ_int, post_summ_coeff)
    }

    # posterior summaries variance components
    reffs <- list()
    post_var_comp <- NULL
    if (object$model_setting$spatio_temporal == 0) {
    if (object$model_settings$prior_reff == "VG") {
      post_var_comp <- smry["lambda[1]", col_take]
      post_var_comp <- matrix(post_var_comp, nrow = 1,
                              dimnames = list("lambda",
                                              names(post_var_comp)))
    } else {
      post_var_comp <- smry["sigma_v[1]", col_take]
      post_var_comp <- matrix(post_var_comp, nrow = 1,
                              dimnames = list("sigma_v",
                                              names(post_var_comp)))
      }

    # posterior summaries random effects (area specific)
    v_est <- par[grepl("^v", par)]
    post_summ_raneff <- smry[v_est, col_take]
    if (object$model_setting$temporal_error) {
      post_summ_raneff <- cbind(Domains = unique(object$data_obj$domains[!(object$data_obj$is_oos)]),
                                as.data.frame(post_summ_raneff))
    }else{
      post_summ_raneff <- cbind(Domains = object$data_obj$domains[!(object$data_obj$is_oos)],
                                as.data.frame(post_summ_raneff))
    }
    reffs <- list(unstructured = post_summ_raneff)
    }
    # temporal random effects
    if (object$model_setting$temporal_error) {
      t_est <- par[grepl("^t", par) & !grepl("^theta", par)]
      post_summ_raneff_t <- smry[t_est, col_take]
      post_summ_raneff_t <- cbind(Domains = object$data_obj$domains,
                                  Times = object$data_obj$times,
                                  as.data.frame(post_summ_raneff_t))
      reffs <- c(reffs, list(temporal = post_summ_raneff_t))
      post_var_comp <- rbind(post_var_comp, smry["sigma_t[1]", col_take])
      rownames(post_var_comp)[nrow(post_var_comp)] <- "sigma_t"

    }
    # spatial random effects
    if (object$model_setting$spatial_error) {
      s_est <- par[grepl("^s", par) & !grepl("^sigma_v", par) & !grepl("^sigma_s", par) & !grepl("^sigma_t", par)]
      post_summ_raneff_s <- smry[s_est, col_take]
      if (object$model_setting$temporal_error) {
        post_summ_raneff_s <- cbind(Domains = unique(object$data_obj$domains),
                                  as.data.frame(post_summ_raneff_s))
      }else{
        post_summ_raneff_s <- cbind(Domains = object$data_obj$domains,
                                  as.data.frame(post_summ_raneff_s))
      }
      reffs <- c(reffs, list(spatial = post_summ_raneff_s))
      post_var_comp <- rbind(post_var_comp, smry["sigma_s[1]", col_take])
      rownames(post_var_comp)[nrow(post_var_comp)] <- "sigma_s"
    }

    # posterior summaries area estimates
    theta_est <- par[grepl("^theta", par) & !grepl("^theta_oos", par)]
    post_summ_theta <- smry[theta_est, col_take]
    if (object$model_setting$temporal_error) {
      post_summ_theta <- cbind(Domains = object$data_obj$domains[!(object$data_obj$is_oos)],
                               Times = object$data_obj$times[!(object$data_obj$is_oos)],
                               as.data.frame(post_summ_theta))
    }else{
      post_summ_theta <- cbind(Domains = object$data_obj$domains[!(object$data_obj$is_oos)],
                               as.data.frame(post_summ_theta))
    }

    # out of samples
    if (sum(object$data_obj$is_oos) == 0) {
      post_summ_theta_oos <- NULL
    }else{
      theta_est_oos <- par[grepl("^theta_oos", par)]
      post_summ_theta_oos = smry[theta_est_oos, col_take]
      if (length(theta_est_oos) == 1) {
        post_summ_theta_oos = matrix(post_summ_theta_oos, nrow = 1,
                                     dimnames = list(NULL, names(post_summ_theta_oos)))
      }
        if (object$model_setting$temporal_error) {
          post_summ_theta_oos <- cbind(Domains = object$data_obj$domains[object$data_obj$is_oos],
                                       Times = object$data_obj$times[object$data_obj$is_oos],
                                       as.data.frame(post_summ_theta_oos))
        }else{
          post_summ_theta_oos <- cbind(Domains = object$data_obj$domains[object$data_obj$is_oos],
                                       as.data.frame(post_summ_theta_oos))
        }
    }

    # Model diagnistics: loo
    if (compute_loo) {
      log_lik <- loo::extract_log_lik(object$stanfit, merge_chains = FALSE)
      rel_n_eff <- loo::relative_eff(exp(log_lik))
      loobj <- loo::loo(log_lik, r_eff = rel_n_eff)
    }else{
      loobj <- NULL
    }

    # Standard deviation reduction
    sdr <- NULL
    if (object$model_settings$likelihood %in% c("beta", "flexbeta")) {
      if (object$model_settings$type_disp == "neff") {
        varfun <- apply(as.matrix(object$stanfit)[, theta_est], 2, function(x) {
          mean(x * (1 - x))
          })
        sd_d <- sqrt(varfun / (object$data_obj$dispersion + 1))
      } else {
        sd_d <- sqrt(object$data_obj$dispersion)
      }
      sdr <- 1 - post_summ_theta[ , "sd"] / sd_d### here controllare
    }
    if (object$model_settings$likelihood %in% c("Infbeta0", "Infbeta0alt", "Infbeta1", "Infbeta01") &
        object$model_settings$type_disp == "neff") {
      mu <- par[grepl("^mu", par)]
      m <- as.matrix(object$stanfit)[, mu]
      if (object$model_settings$likelihood == "Infbeta1") {
        p1 <- par[grepl("^p1", par)]
        p <- as.matrix(object$stanfit)[, p1]
        vars <- m * sweep((1 - m), MARGIN = 2, STATS = (object$data_obj$dispersion + 1), FUN = '/') *
           (1 - p) + p * (1 - p)*(1 - m)^2
      }
      if (object$model_settings$likelihood %in% c("Infbeta0", "Infbeta0alt")) {
        if (object$model_settings$likelihood == "Infbeta0") {
          p0 <- par[grepl("^p0", par)]
          p <- as.matrix(object$stanfit)[, p0]
        }else{#Infbeta0alt
          p <- sweep((1 - m), MARGIN = 2, STATS = object$data_obj$domain_size, FUN = '^')
        }
        vars <- m * sweep((1 - m), MARGIN = 2, STATS = (object$data_obj$dispersion + 1), FUN = '/') *
          (1 - p) + p * (1 - p) * m ^ 2
      }
      if (object$model_settings$likelihood == "Infbeta01") {
        p1 <- par[grepl("^p1", par)]
        p_1 <- as.matrix(object$stanfit)[, p1]
        p0 <- par[grepl("^p0", par)]
        p_0 <- as.matrix(object$stanfit)[, p0]
        p_tot <- p_1 + p_0
        g <- p_1/p_tot
        vars <- m * sweep((1 - m), MARGIN = 2, STATS = (object$dispersion + 1), FUN = '/') *
           (1 - p_tot) + p_tot * g * (1 - g) + p_tot * (1 - p_tot) * (g - m) ^ 2
      }
      sd_d <- sqrt(apply(vars, 2, mean))
      sdr <- 1 - post_summ_theta[, "sd"] / sd_d
    }

    # Residuals
    residuals <- object$data_obj$y_is - post_summ_theta[,"mean"]

    # Shrinking rate
    if (object$model_settings$intercept == 1) {
      linpred <- cbind(1, object$data_obj$X_scal[,]) %*% matrix(post_summ_coeff[, "mean"], ncol = 1)
    }else{
      linpred <- object$data_obj$X_scal[,] %*% matrix(post_summ_coeff[ , "mean"], ncol = 1)
    }
    p_star <- exp(linpred) / (1 + exp(linpred))
    rate <- ifelse(
      post_summ_theta[,"mean"] >= min(object$data_obj$y_is, p_star) &
        post_summ_theta[,"mean"] <= max(object$data_obj$y_is, p_star),
      1,
      0
    )
    shrink_rate <- mean(rate)

    # Posterior p_values
    post_y_rep <- as.matrix(object$stanfit)[, par[grepl("^y_rep", par)]]
    bayes_pvalues <- sapply(1:length(object$data_obj$y_is), function(w)
      mean(post_y_rep[, w] > object$data_obj$y_is[w]))

    # Summary diagnostic measures
    diag_summ <- rbind("Residuals" = summary(residuals),
                       "S.D. Reduction" = summary(sdr),
                       "Bayesian p-value" = summary(bayes_pvalues))
    # Output
    out = list(
      raneff = reffs,
      fixed_coeff = post_summ_coeff,
      var_comp = post_var_comp,
      model_estimates = post_summ_theta,
      model_estimates_oos = post_summ_theta_oos,
      is_oos = object$data_obj$is_oos,
      direct_est = object$data_obj$y_is,
      post_means = post_summ_theta[, "mean"],
      sd_reduction = sdr,
      sd_dir = sd_d,
      loo = loobj,
      shrink_rate = shrink_rate,
      residuals = residuals,
      bayes_pvalues = bayes_pvalues,
      y_rep = post_y_rep,
      diag_summ = diag_summ,
      data_obj = object$data_obj,
      model_settings = object$model_settings,
      call = object$call
    )

    class(out) <- "summary_fitsae"
    return(out)
}

#' @export
#'

print.summary_fitsae <- function(x, digits = 3L, ...) {
  if (class(x) != "summary_fitsae")
    stop("Indicated object does not have 'summary_fitsae' class.")
  cat("Summary for the SAE model call:\n ")
  print(x$call)
  cat("\n")


  cat("----- S.D. of the random effects: posterior summaries -----\n")
  cat("\n")
  print(round(x$var_comp,digits = digits))
  cat("\n")


  cat("----- Fixed effects coefficients: posterior summaries -----\n")
  cat("\n")
  print(round(x$fixed_coeff,digits = digits))
  cat("\n")

  cat("--------------- Model diagnostics summaries ---------------\n")

  cat("\n")
  print(round(x$diag_summ,digits = digits))
  cat("\n")

  cat("Shrinkage Bound Rate:",
      x$shrink_rate * 100,
      "%")

  cat("\n")
  if (!is.null(x$loo)) {
    cat("\n")
    cat("LOO Information Criterion:",
        "\n",
       as.vector(sapply(1:4, function(w)
          c(capture.output(round(
           x$loo$estimates, digits = digits
         ))[w], "\n")))
    )
    }
}

