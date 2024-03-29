#' Variance Smoothing and Effective Sample Sizes Estimation
#'
#' The `smoothing()` function implements three methods, all yielding refined estimates of either variance or effective sample size, to account for indicators with different variance functions. The output estimates are ready to be used as known parameters in an area-level model, and they need to be added to the analysed `data.frame` object. All the implemented methods enable the estimation of the effective sample sizes, whereas `"ols"` and `"gls"` also perform a variance smoothing procedure.
#'
#' @param data A `data.frame` object including the direct estimates.
#' @param direct_estimates Character string specifying the variable in `data` denoting the direct estimates.
#' @param method The method to be used. The choices are `"kish"`,`"ols"` and `"gls"`.
#' @param area_id Character string indicating the variable with domain names included in `data`, to be specified if method `"kish"` is selected.
#' @param raw_variance Character string indicating the variable name for raw variance estimates included in `data` object, to be specified if methods `"ols"` or `"gls"` are selected.
#' @param areas_sample_sizes Character string indicating the variable name for domain sample sizes included in `data` object, to be specified if methods `"ols"` or `"gls"` are selected.
#' @param additional_covariates A vector of character strings indicating the variable names of possible additional covariates, included in `data`, to be added to the smoothing procedure if methods `"ols"` or `"gls"` are selected.
#' @param var_function  An object of class `function` denoting the variance function of the response variable. The default option (`NULL`) matches the proportion case being equal to `function(x) x * (1 - x)`. If an alternative function is specified, only variance estimates are provided.
#' @param survey_data An additional dataset to be specified when method `"kish"` is selected, defined at sampling unit level (e.g., households) and comprising sampling weights, unit sizes and domain names.
#' @param survey_area_id Character string indicating the variable denoting the domain names included in the `survey_data` object.
#' @param weights Character string indicating the variable including sampling weights in `survey_data` object.
#' @param sizes Character string indicating the variable including unit sizes in `survey_data` object.
#'
#' @return An object of class `smoothing_fitsae`, being a list of vectors including dispersion estimates: the variances and, when no alternative variance functions are specified, the effective sample sizes. When `"ols"` or `"gls"` method has been selected, the list incorporates also an object of class \code{\link[nlme]{gls}} from `nlme` package.
#'
#' @seealso \code{\link[nlme]{gls}} for details on estimation procedure for `"ols"` and `"gls"` methods.
#'
#' @references
#' \insertRef{kish1992weighting}{tipsae}
#'
#' \insertRef{fabrizi2011hierarchical}{tipsae}
#'
#'  \insertRef{JSS}{tipsae}
#'
#' @examples
#'
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # perform smoothing procedure
#' smoo <- smoothing(emilia_cs, direct_estimates = "hcr", area_id = "id",
#'                   raw_variance = "vars", areas_sample_sizes = "n",
#'                   var_function = NULL, method = "ols")
#'
#' @export
#'

smoothing <- function(data,
                      # ordered as area id factor!
                      direct_estimates,
                      area_id = NULL,
                      raw_variance = NULL,
                      areas_sample_sizes = NULL,
                      additional_covariates = NULL,
                      method = c("ols", "gls", "kish"),
                      var_function = NULL,
                      survey_data = NULL,
                      survey_area_id = NULL,
                      weights = NULL,
                      sizes = NULL) {

  method <- match.arg(method)
  check_smoo(
    data,
    direct_estimates,
    area_id,
    raw_variance,
    areas_sample_sizes,
    additional_covariates,
    method,
    var_function,
    survey_data,
    survey_area_id,
    weights,
    sizes
  )

  default_variance_function = "no"

  colnames(data)[which(colnames(data) == direct_estimates)] <-
    "direct_estimates"

  if (is.null(var_function)) {
    var_function = function(mu)
      mu * (1 - mu)
    message("Proportions variance function specified.")
    default_variance_function = "yes"
  }

  if (method == "kish") {
    if (!is.null(additional_covariates) ||
        !is.null(raw_variance) ||
        !is.null(areas_sample_sizes))
      warning(
        "'additional_covariates', 'raw_variance', 'areas_sample_sizes' will not be considered when choosing 'kish' method."
      )
    colnames(data)[which(colnames(data) == area_id)] <- "area_id"

    colnames(survey_data)[which(colnames(survey_data) == survey_area_id)] <-
      "area_id"
    colnames(survey_data)[which(colnames(survey_data) == weights)] <-
      "weights"
    colnames(survey_data)[which(colnames(survey_data) == sizes)] <-
      "sizes"

    agg_area <-
      stats::aggregate(survey_data$weight,
                       by = list(survey_data$area_id),
                       FUN = sum)
    names(agg_area) = c("area_id", "N_i")

    agg <- merge(survey_data, agg_area, by = "area_id")
    agg$tot = (agg$weights / agg$N_i) ^ 2 / agg$sizes

    s <-
      stats::aggregate(agg$tot, by = list(agg$area_id), FUN = sum)
    colnames(s) <- c("area_id", "deff")

    data <- merge(data, s, by = "area_id")

    data$inv_deff = 1 / data$deff

    if (default_variance_function == "yes") {
      out = list(
        method = method,
        default_variance_function = default_variance_function,
        var_function = var_function,
        phi = data$inv_deff - 1,
        vars = var_function(data$direct_estimates) / data$inv_deff
      )
    } else{
      out = list(
        method = method,
        default_variance_function = default_variance_function,
        var_function = var_function,
        vars = var_function(data$direct_estimates) / data$inv_deff
      )
    }



  }

  if (method %in% c("ols", "gls")) {
    colnames(data)[which(colnames(data) == areas_sample_sizes)] <- "n"
    colnames(data)[which(colnames(data) == raw_variance)] <-
      "raw_variance"


    regdata <-
      data.frame(y = var_function(data$direct_estimates) / data$raw_variance,
                 n = data$n)

    if (!is.null(additional_covariates))
      regdata <- cbind(regdata, data[, additional_covariates])

    nam <- names(regdata)[-1]
    str <-
      paste(sapply(1:length(nam), function(x)
        paste0("+", nam[x])), collapse = '')

    if (method == "gls") {
      reg <- nlme::gls(
        as.formula(paste0("y ~ -1", str)),
        data = regdata,
        weights = nlme::varPower(),
        na.action = na.omit,
        control = list(singular.ok = TRUE)
      )
    } else{
      reg <- nlme::gls(as.formula(paste0("y ~ -1", str)),
                       data = regdata,
                       na.action = na.omit,
                       control = list(singular.ok = TRUE))
    }

    inv_deff <-
      reg$coefficients[which(names(reg$coefficients) == "n")]


    if (default_variance_function == "yes") {
      out <- list(
        method = method,
        default_variance_function = default_variance_function,
        var_function = var_function,
        regression = reg,
        phi = data$n * inv_deff - 1,
        vars = var_function(data$direct_estimates) / (data$n * inv_deff),
        raw_vars = data$raw_variance,
        n = data$n
      )
    } else{
      out <- list(
        method = method,
        default_variance_function = default_variance_function,
        var_function = var_function,
        regression = reg,
        vars = var_function(data$direct_estimates) / (data$n * inv_deff),
        raw_vars = data$raw_variance,
        n = data$n
      )
    }
  }

  class(out) <- "smoothing_fitsae"
  return(out)
}



check_smoo <- function(data,
                       direct_estimates,
                       area_id,
                       raw_variance,
                       areas_sample_sizes,
                       additional_covariates,
                       method,
                       var_function,
                       survey_data,
                       survey_area_id,
                       weights,
                       sizes) {
  if (!inherits(data, "data.frame"))
    stop("'data' is not a dataframe object.")

  if (!(direct_estimates %in% colnames(data)))
    stop("'direct_estimates' names if specified must be valid columns names of 'data'.")

  if (!is.null(var_function) && !inherits(var_function, "function"))
    stop("'var_function' defined is not a function.")

  if (sum(data[,raw_variance] <= 0, na.rm = T) != 0)
    stop("Raw variances uncorrectly specified with negative or null values.")

  if (method == "kish") {
    if (!(area_id %in% colnames(data)))
      stop("'area_id' must be valid columns names of 'data'.")

    if (is.null(survey_data))
      stop("'survey_data' object has to be indicated when choosing 'kish' method.")

    if (!inherits(survey_data, "data.frame"))
      stop("survey_data is not a dataframe object.")

    if (any(!(
      c(survey_area_id, weights, sizes) %in% colnames(survey_data)
    )))
      stop(
        "'survey_area_id', 'sizes' and 'weights' names must be valid columns names of 'survey_data'."
      )

    if (length(unique(survey_data[, survey_area_id])) != dim(data)[1])
      stop("Number of areas in 'data' different from areas in 'survey_data'.")

    if (any(!(unique(survey_data[, survey_area_id]) %in% unique(data[, area_id]))))
      stop("Number of areas in 'data' different from areas in 'survey_data'.")



  }

  if (method %in% c("ols", "gls")) {
    if ((!is.null(raw_variance) &&
         !(raw_variance %in% colnames(data))) ||
        (!is.null(areas_sample_sizes) &&
         !(areas_sample_sizes %in% colnames(data))) ||
        (!is.null(additional_covariates) &&
         any(!(
           additional_covariates %in% colnames(data)
         ))))
      stop(
        "'raw_variance', 'areas_sample_sizes', 'additional_covariates' names, if specified, must be valid columns names of 'data'."
      )

  }


}

#' Print Method for a `smoothing_fitsae` Object
#'
#' The generic method `print()` allow to explore relevant outputs of the input object
#'
#' @param x Object of class `smoothing_fitsae`.
#' @param digits Number of digits to display.
#' @param ... Currently unused.
#'
#' @return Printed information on a `smoothing_fitsae` object.
#'
#' @export
#'

print.smoothing_fitsae <- function(x, digits = 3L, ...) {
  if (!inherits(x, "smoothing_fitsae"))
    stop("Indicated object does not have 'smoothing_fitsae' class.")
  cat("Smoothing procedure for the dispersion parameters \n")
  cat("\n")
  cat("* Adopted method:", x$method, "\n")
  cat("* Variance function:\n")
  cat("function(mu) {\n")
  print(body(x$var_function))
  cat("}\n")

  if (x$method %in% c("ols", "gls")) {
    cat("---------------------------------------------------------------------\n")
    cat("Generalized Variance Function regression: \n")
    cat("\n")
    print(summary(x$regression), digits = digits)
    cat("---------------------------------------------------------------------\n")

    cat("\n")
  }

  cat("Summaries of involved quantities\n")
  cat("\n")
  cat("* Smoothed variance estimates: \n")
  print(summary(x$vars), digits = digits)
  cat("\n")

  if (x$method %in% c("ols", "gls")) {
    cat("* Differences between smoothed and raw variances: \n")
    print(summary(x$raw_vars - x$vars, digits = digits))
    cat("\n")
  }

  if (x$default_variance_function == "yes") {
    cat("* Smoothed Phi: \n")
    print(summary(x$phi, digits = digits))
    cat("\n")
  }

}

#' Plot Method for `smoothing_fitsae` Object
#'
#' The `plot()` method provides (a) the boxplot of variance estimates, when effective sample sizes are estimated through `kish` method; (b) a scatterplot of both original and smoothed estimates versus the area sample sizes, when variance smoothing is performed through methods `ols` and `gls`.
#'
#' @param x A `smoothing_fitsae` object.
#' @param size Aesthetic option denoting the size of scatterplots points, see \code{\link[ggplot2]{geom_point}} documentation.
#' @param alpha Aesthetic option denoting the opacity of scatterplots points, see \code{\link[ggplot2]{geom_point}} documentation.
#' @param ... Currently unused.
#'
#' @return A `ggplot2` object.
#'
#' @seealso \code{\link{smoothing}} to produce the input object.
#'
#' @examples
#'
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # perform smoothing procedure
#' smoo <- smoothing(emilia_cs, direct_estimates = "hcr", area_id = "id",
#'                   raw_variance = "vars", areas_sample_sizes = "n",
#'                   var_function = NULL, method = "ols")
#' plot(smoo)
#'
#' @export
#'

plot.smoothing_fitsae <- function(x,
                                  size = 2.5,
                                  alpha = 0.8,
                                  ...) {
  if (!inherits(x, "smoothing_fitsae"))
    stop("Indicated object does not have 'smoothing_fitsae' class.")

  # Plot original vs smoother variance estimates
  if (x$method == "kish") {
    xydata <- data.frame(y = x$vars)
    #lims_axis <- range(x$vars)
    plot_s <- ggplot2::ggplot(data = xydata, ggplot2::aes_(y = ~ y)) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::ylab("Kish variance est.") +
      ggplot2::theme_bw() +
      ggplot2::geom_boxplot() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
  } else{
    xydata <- data.frame(
      y = c(x$raw_vars, x$vars),
      n = c(x$n, x$n),
      Type = c(rep("Raw", length(x$raw_vars)),
               rep("Smoothed", length(x$vars)))
    )
    # scatter with n size and smoothed/original estimates

    plot_s <-
      ggplot2::ggplot(data = xydata, ggplot2::aes_(
        x = ~ n,
        y = ~ y,
        color = ~ Type
      )) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::ylab("Estimates") +
      ggplot2::xlab("n") +
      ggplot2::theme_bw() +
      ggplot2::geom_point(shape = 20,
                          size = size,
                          alpha = alpha) +
      ggplot2::scale_color_manual(values = c("#E69F00", "deepskyblue4"))
  }

  plot_s
}
