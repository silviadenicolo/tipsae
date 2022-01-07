#' Bayesian proportions/(0,1)-measures small area model with Stan
#'
#'
#' @param direct_estimates Linear regression formula at the linking level.
#' @return An object of class `smoothing.fitsae`.
#' @export
#'
smoothing <- function(data,  # ordered as area id factor!
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
                      sizes= NULL) {

  check_smoo(data,
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
             sizes)

  colnames(data)[which(colnames(data) == direct_estimates)] <- "direct_estimates"


  if (is.null(var_function)) {
    var_function = function(mu)
      mu * (1 - mu)
    message("Proportions variance function specified.")
  }

  if(method == "kish"){

    if (!is.null(additional_covariates) ||
        !is.null(raw_variance) ||
        !is.null(areas_sample_sizes)
    )
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
      stats::aggregate(
        survey_data$weight,
        by = list(survey_data$area_id),
        FUN = sum
      )
    names(agg_area) = c("area_id", "N_i")

    agg <- merge(survey_data, agg_area, by = "area_id")
    agg$tot = (agg$weights / agg$N_i) ^ 2 / agg$sizes

    s <- stats::aggregate(agg$tot, by = list(agg$area_id), FUN = sum)
    colnames(s) <- c("area_id", "deff")

   data <- merge(data, s, by = "area_id")

   data$inv_deff = 1 / data$deff

   out = list(
         phi = data$inv_deff - 1,
         vars = var_function(data$direct_estimates) / data$inv_deff
     )

  }

  if (method %in% c("ols", "gls")){

   colnames(data)[which(colnames(data) == areas_sample_sizes)] <- "n"
   colnames(data)[which(colnames(data) == raw_variance)] <-
        "raw_variance"


   regdata <- data.frame(y = var_function(data$direct_estimates) / data$raw_variance,
                   n = data$n)

   if (!is.null(additional_covariates))
        regdata <- cbind(regdata, data[, additional_covariates])

    nam <- names(regdata)[-1]
    str <-
        paste(sapply(1:length(nam), function(x)
          paste0("+", nam[x])), collapse = '')

    if (method == "gls") {
        reg <- nlme::gls(as.formula(paste0("y ~ -1", str)),
                         data = regdata,
                         weights = nlme::varPower(), na.action = na.omit)
      } else{
        reg <- nlme::gls(as.formula(paste0("y ~ -1", str)),
                         data = regdata, na.action = na.omit)}


      print(summary(reg))

      inv_deff <- reg$coefficients[which(names(reg$coefficients) == "n")]

      out <- list(
        regression = reg,
        phi = data$n * inv_deff - 1,
        vars = var_function(data$direct_estimates) / (data$n * inv_deff)
      )
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

  if (class(data) != "data.frame")
    stop("'data' is not a dataframe object.")

  if (!(direct_estimates %in% colnames(data)))
    stop(
      "'direct_estimates' names if specified must be valid columns names of 'data'."
    )

  if (!is.null(var_function) &&
      class(var_function) != "function")
    stop("'var_function' defined is not a function.")

  if (!(method %in% c("ols", "gls", "kish")))
    stop("'method' specified is not included in c('ols', 'gls', 'kish').")

  if (method == "kish") {

    if (!(area_id %in% colnames(data)))
      stop(
        "'area_id' must be valid columns names of 'data'."
      )

    if (is.null(survey_data))
      stop(
        "'survey_data' object has to be indicated when choosing 'kish' method."
      )

    if (class(survey_data) != "data.frame")
      stop("survey_data is not a dataframe object.")

    if (any(!(c(survey_area_id, weights, sizes) %in% colnames(survey_data))))
      stop("'survey_area_id', 'sizes' and 'weights' names must be valid columns names of 'survey_data'.")

    if (length(unique(survey_data[,survey_area_id])) != dim(data)[1])
      stop("Number of areas in 'data' different from areas in 'survey_data'.")

    if ( any(!(unique(survey_data[,survey_area_id]) %in% unique(data[,area_id]))))
      stop("Number of areas in 'data' different from areas in 'survey_data'.")

    }

  if(method %in% c("ols", "gls")){


   if( (!is.null(raw_variance) &&
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
