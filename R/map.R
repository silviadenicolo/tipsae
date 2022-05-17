#' Map Relevant Quantities from a Small Area Model
#'
#' The `map()` function enables to plot maps containing relevant model outputs by accounting for their geographical dimension. The shapefile of the area must be provided via a `SpatialPolygonsDataFrame` object.
#'
#' @param x An object of class `summary_fitsae` or `benchmark_fitsae`.
#' @param spatial_df A object of class `SpatialPolygonsDataFrame` (spatial polygons object) from `sp` package, accounting for the geographical dimension of the domains.
#' @param spatial_id_domains A character string indicating the name of `spatial_df` variable containing area denominations, in order to correctly match the areas.
#' @param match_names An encoding two-columns `data.frame`: the first with the original data coding (domains) and the second one with corresponding `spatial_df` object labels. This argument has to be specified only if `spatial_df` object labels do not match the ones provided through the original dataset.
#' @param color_palette A vector with two color strings denoting the extreme bounds of colors range to be used.
#' @param quantity A string indicating the quantity to be mapped. When a `summary_fitsae` is given as input, it can be selected among `"HB_est"` (model-based estimates), `"SD"`(posterior standard deviations) and `"Direct_est"`(direct estimates). While when a `benchmark_fitsae` class object is given as input, this argument turns automatically to `"Bench_est"`, displaying the benchmarked estimates.
#' @param time A string indicating the year of interest for the quantities to be treated, in case of temporal or spatio-temporal objects.
#' @return A map `ggplot2` object with colors scaled legend.
#'
#' @seealso \code{\link{summary.fitsae}} to produce the input object and \code{\link[sp]{SpatialPolygonsDataFrame}} to manage the shapefile.
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
#' # load shapefile of concerned areas
#' data("emilia_shp")
#'
#' # plot the map using model diagnostics and areas shapefile
#' map(x = summ_beta,
#'    spatial_df = emilia_shp,
#'    spatial_id_domains = "NAME_DISTRICT")
#' @export


map <- function(x,
                spatial_df,
                spatial_id_domains,
                match_names = NULL,
                color_palette = c("snow2","deepskyblue4"),
                quantity = "HB_est",
                time = NULL) {

  check_map_sae(x,
                spatial_df,
                spatial_id_domains,
                match_names,
                color_palette,
                quantity,
                time)


  # define data.frame with quantities to plot
  map_data <- data.frame("Domains" = rep(NA, nrow(spatial_df)),
                         "mean_HB" = rep(NA, nrow(spatial_df)),
                         "sd_HB" = rep(NA, nrow(spatial_df)),
                         "Direct" = rep(NA, nrow(spatial_df)),
                         "Bench_est" = rep(NA, nrow(spatial_df))
  )

  if (inherits(x, "benchmark_fitsae") & length(x$time) == 1) {
    if (is.null(time) || time != x$time) message("Time argument is forced to be the benchmarking time period.")
    time <- x$time
  }

  # select desired time point
  if (!x$model_settings$temporal_error) {
    dat_is <- x$model_estimates
    dat_oos <- x$model_estimates_oos
    is_oos <- x$data_obj$is_oos
    direct <- x$direct_est
  } else { # Subsetting by time
    dat_is <- x$model_estimates[x$model_estimates$Times == time, ]
    dat_oos <- x$model_estimates_oos[x$model_estimates_oos$Times == time, ]
    is_oos <- x$data_obj$is_oos[x$model_estimates$Times == time]
    direct <- x$direct_est[x$model_estimates$Times == time]
  }


  # fill the data.frame
  if (inherits(x, "summary_fitsae")) {
    map_data$Domains[!is_oos] <- dat_is$Domains
    map_data$mean_HB[!is_oos] <- dat_is$mean
    map_data$sd_HB[!is_oos] <- dat_is$sd
    map_data$Direct[!is_oos] <- direct
    if (sum(is_oos) > 0) {
      map_data$Domains[is_oos] <- dat_oos$Domains
      map_data$mean_HB[is_oos] <- dat_oos$mean
      map_data$sd_HB[is_oos] <- NA
      map_data$Direct[is_oos] <- NA
    }
  } else if (inherits(x, "benchmark_fitsae")) {

    if (quantity %in% c("HB_est", "Direct_est", "SD")) {
      quantity <- "Bench_est"
      message("With 'benchmark_fitsae' object, default quantity is 'Bench_est'.")
    }
    map_data$Bench_est <- x$bench_est
    map_data$Domains[!is_oos] <- dat_is$Domains

    if (length(x$data_obj$indices_oos) > 0) {
      map_data$Domains[is_oos] <- dat_oos$Domains }

  }

  # match model estimates and shapefile
  if (is.null(match_names)) {
    indices_match <- match(spatial_df@data[spatial_id_domains][,1], map_data$Domains)
    if (any(is.na(indices_match))) {
      if (all(is.na(indices_match))) {
        stop("Domains names provided in 'fit_sae' and those in the 'spatial_df' do not match.")
      } else {
        warnings("At least one Domain's name provided in 'fit_sae' does not match to names in 'spatial_df'.
                 Consider specifying 'match_names' as input.")
      }
    }

  } else {
    map_data <- merge(x = map_data, y = match_names,
                      by.x = "Domains", by.y = names(match_names)[1])
    indices_match <- match(spatial_df@data[spatial_id_domains][,1],
                           map_data[, names(match_names)[2]])
    if (any(is.na(indices_match))) {
      if (all(is.na(indices_match))) {
        stop("Domains names provided in 'fit_sae' and those in the 'spatial_df' do not match.")
      } else {
        warnings("At least one Domain's name provided in 'fit_sae' does not match to names in 'spatial_df'.
                 Consider specifying 'match_names' as input.")
      }
    }
    map_data <- map_data[,!colnames(map_data) %in% c("Domain",
                                                     spatial_id_domains,
                                                     names(match_names)), drop = F]
  }
  map_data <- map_data[indices_match,]

  # fortify
  spatial_df@data[colnames(map_data)] <- map_data
  spatial_df_tidy <- broom::tidy(spatial_df, region = spatial_id_domains)
  spatial_df_tidy <- merge(spatial_df_tidy,
                           spatial_df@data,
                           by.x = "id",
                           by.y = spatial_id_domains)
  colnames(spatial_df_tidy)[colnames(spatial_df_tidy) %in% c("mean_HB","sd_HB","Direct")] <- c("HB_est", "SD", "Direct_est")


  spatial_df_tidy[quantity][, 1][!is.finite(spatial_df_tidy[quantity][, 1])] <- NA

  # ggplot
  map <- ggplot2::ggplot(spatial_df_tidy,
                         ggplot2::aes_(x = ~long, y = ~lat, group = ~ group,
                                       fill = spatial_df_tidy[quantity][, 1])) +
    ggplot2::geom_polygon(color = "gray47", size = 0.1)  +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(
      name = quantity,
      low = color_palette[1],
      high = color_palette[2],
      limits = range(spatial_df_tidy[quantity][, 1]),
      guide = "colourbar"
    ) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank()) +
    ggplot2::coord_sf()

  if (x$model_settings$temporal_error) {
    map <- map + ggplot2::ggtitle(paste0("Time t = ", time))
  }

  map

}

check_map_sae <- function(x,
                          spatial_df,
                          spatial_id_domains,
                          match_names,
                          color_palette,
                          quantity,
                          time){


  if (!inherits(x, "summary_fitsae") & !inherits(x, "benchmark_fitsae")) {
    stop("Indicated object does not have 'summary_fitsae' or 'benchmark_fitsae' class.")
  }
  if (length(quantity) != 1) {
    stop("'quantity' must be a vector of length 1.")
  }
  if (!quantity %in% c("HB_est", "Direct_est", "SD")) {
    stop("Argument 'quantity' must be fixed equal to 'HB_est', 'Direct_est', or 'SD'")
  }
  if (!inherits(spatial_df, "SpatialPolygonsDataFrame")) {
    stop("'spatial_df' is not of class SpatialPolygonsDataFrame from the 'sp' package")
  }
  if (!x$model_settings$temporal_error) {
    if (nrow(spatial_df) != length(x$data_obj$y)) {
      stop("The input of the argument 'spatial_df' must be have the same number of rows
          of the 'data' object in the fit_sae function.")
    }
  }else{
    if (nrow(spatial_df) != table(x$data_obj$times)[1]) {
      stop("The input of the argument 'spatial_df' must be have the same number of domains
          of the 'data' object in the fit_sae function.")
    }
  }
  if (x$model_settings$temporal_error & ((is.null(time) & inherits(x, "summary_fitsae")) || (inherits(x, "benchmark_fitsae") & length(x$time) != 1))) {
    stop("When a temporal error is included in the model, the argument 'time' must be specified.")
  }
  if (!is.null(time)) {
    if (length(time) != 1) {
      stop("Only one value of 'time' must be considered.")
    }
    if (!(time %in% unique(x$data_obj$times)) & !(inherits(x, "benchmark_fitsae") & length(x$time) == 1)) {
      stop("'time' must contain a value containing a time included in the input data.")
    }
  }
  if (length(color_palette) != 2 || !is.vector(color_palette)) {
    stop(
      "'color_palette' needs to be a vector of length 2
       defining the lower and upper colors of the palette range."
    )
  }
}
