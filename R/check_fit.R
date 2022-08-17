
# Only internal

check_par_fit <- function(formula_fixed,
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
                          init) {
  # formula_fixed
  if (is.null(formula_fixed) || !inherits(formula_fixed, "formula")) {
    stop("Argument 'formula_fixed' must contain a formula object.")
  }
  # data
  if (is.null(data) || !is.data.frame(data)) {
    stop("The argument 'data' must contain a data.frame object.")
  }
  # domains
  if (!is.null(domains) && (length(domains) != 1 ||
                            !(domains %in% colnames(data)))) {
    stop(
      "The argument 'domain' must contain a unique character that determines
         a column in the 'data' object with the domains denominations"
    )
  }
  if (is.null(disp_direct) || (length(disp_direct) != 1 ||
                               !(disp_direct %in% colnames(data)))) {
    stop(
      "The argument 'disp_direct' must contain a unique character that determines
         a column in the 'data' object with the direct estimates of the desired dispersion measure"
    )
  }
  if (is.null(prior_coeff) || !(prior_coeff == "normal"
                                || prior_coeff == "HorseShoe")) {
    stop(
      "The argument 'prior_coeff' must contain a string among the following: 'normal', 'HorseShoe'"
    )
  }
  # if (is.null(domain_size) && likelihood == "Infbeta0alt") {
  #   stop("To use the alternative 0 inflated model the sizes of the areas must be specified")
  # }
  if (!is.null(domain_size) && (length(domain_size) != 1 ||
                                !(domain_size %in% colnames(data)))) {
    stop(
      "The argument 'domain_size' must contain a unique character that determines
         a column in the 'data' object with the size of the domains"
    )
  }
  if (prior_coeff == "HorseShoe" && is.null(p0_HorseShoe)) {
    stop(
      "The 'HorseShoe' prior for the regression coefficients requires
         the initial guess of non-zero coefficients 'p0_HorseShoe' to be specified."
    )
  }
  if (spatial_error && is.null(spatial_df)) {
    stop(
      "When a spatial error is included in the model, a 'SpatialPolygonsDataFrame' must
      be passed to the argument 'spatial_df'"
    )
  }
  if (spatial_error) {
    if (!inherits(spatial_df, "SpatialPolygonsDataFrame")) {
      stop(
        "The input of the argument 'spatial_df' must be of class
        'SpatialPolygonsDataFrame' (see 'sp' package)"
      )
    }
    if (!temporal_error) {
      if (nrow(spatial_df) != nrow(data)) {
        stop("The input of the argument 'spatial_df' must be have the same number of rows of 'data'")
      }
    }
  }
  if (temporal_error && is.null(temporal_variable)) {
    stop(
      "When a temporal error is included in the model, a temporal variable has to
      be included in data and its name has to be passed through argument 'temporal_variable'."
    )
    if (length(temporal_variable) != 1 || !(temporal_variable %in% colnames(data))) {
      stop("The argument 'temporal_variable' must be a string indicating a valid name in the data.")
    }
  }
  if (temporal_error && is.null(domains)) {
    stop("When a temporal random effect is considered, the argument 'domains' must be specified.")
  }
  if (temporal_error && spatial_error) {
    if (nrow(spatial_df) != length(unique(data[,domains]))) {
      stop("The input of the argument 'spatial_df' must be have the same number of rows of the number of domains in 'data'")
    }
  }
  if (likelihood %in% c("Infbeta0", "Infbeta01",  "Infbeta1") & type_disp == "var") { #"Infbeta0alt",
    stop("With zero and one inflated models 'type_disp' can be only 'neff'.")
  }
  if (likelihood %in% c("flexbeta") & type_disp == "neff") {
    stop("With the flexible beta model 'type_disp' can be only 'var'.")
  }

}






check_data_fit <- function(data_obj, likelihood, domain_size) {
  if (!all(complete.cases(data_obj$X_scal))) {
    stop("The covariates must not have NAs.")
  }
  if (ncol(data_obj$X_scal) == 0) {
    stop("At least one covariate must be included in the model.")
  }
  # check domain of direct estimates
  if (!all(data_obj$y_is >= 0 & data_obj$y_is <= 1)) {
    stop("The direct estimates of proportions or indices must be between 0 and 1")
  }
  if (likelihood %in% c("beta", "flexbeta") &&
      (!all(data_obj$y_is != 0) || !all(data_obj$y_is != 1))) {
    stop("To deal with direct estimates equal to 0 or 1 an inflated model must be chosen")
  }
  if (!all(data_obj$y_is != 1) &&
      !(likelihood %in% c("Infbeta1", "Infbeta01"))) {
    stop(
      "To deal with direct estimates equal to 1, models 'Infbeta1' or 'Infbeta01' must be chosen"
    )
  }
  if (!all(data_obj$y_is != 0) &&
      !(likelihood %in% c("Infbeta0",  "Infbeta01"))) { #"Infbeta0alt",
    stop(
      "To deal with direct estimates equal to 1, models 'Infbeta0',
          or 'Infbeta01' must be chosen"
    ) #'Infbeta0alt',
  }

  if (!all(!is.na(data_obj$dispersion[!data_obj$is_oos]))) {
    stop("The direct estimates of the dispersion parameters must not have NAs")
  }
  if (!all(!is.na(data_obj$dispersion[data_obj$y_is != 0 &
                                      data_obj$y_is != 1]))) {
    stop("The direct estimates of the dispersion parameters must not have NAs")
  }
  if (sum(data_obj$dispersion <= 0, na.rm = T) != 0)
    stop("Dispersion parameter uncorrectly specified with negative values.")
  if (!is.null(domain_size)) {
    if (sum(data_obj$domain_size_n <= 0, na.rm = T) != 0)
      stop("Sizes of the domains uncorrectly specified with negative values.")
  }


}


