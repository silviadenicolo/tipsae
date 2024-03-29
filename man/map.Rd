% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{map}
\alias{map}
\title{Map Relevant Quantities from a Small Area Model}
\usage{
map(
  x,
  spatial_df,
  spatial_id_domains,
  match_names = NULL,
  color_palette = c("snow2", "deepskyblue4"),
  quantity = c("HB_est", "Direct_est", "SD"),
  time = NULL,
  style = "quantile",
  ...
)
}
\arguments{
\item{x}{An object of class \code{summary_fitsae} or \code{benchmark_fitsae}.}

\item{spatial_df}{A object of class \code{SpatialPolygonsDataFrame} (spatial polygons object) from \code{sp} package or \code{sf} from the \code{sf} package, accounting for the geographical dimension of the domains.}

\item{spatial_id_domains}{A character string indicating the name of \code{spatial_df} variable containing area denominations, in order to correctly match the areas.}

\item{match_names}{An encoding two-columns \code{data.frame}: the first with the original data coding (domains) and the second one with corresponding \code{spatial_df} object labels. This argument has to be specified only if \code{spatial_df} object labels do not match the ones provided through the original dataset.}

\item{color_palette}{A vector with two color strings denoting the extreme bounds of colors range to be used.}

\item{quantity}{A string indicating the quantity to be mapped. When a \code{summary_fitsae} is given as input, it can be selected among \code{"HB_est"} (model-based estimates), \code{"SD"}(posterior standard deviations) and \code{"Direct_est"}(direct estimates). While when a \code{benchmark_fitsae} class object is given as input, this argument turns automatically to \code{"Bench_est"}, displaying the benchmarked estimates.}

\item{time}{A string indicating the year of interest for the quantities to be treated, in case of temporal or spatio-temporal objects.}

\item{style}{Method to process the color scale, see \code{\link[tmap]{tmap}} documentation.}

\item{...}{Arguments passed to \code{\link[tmap]{tm_fill}} (e.g. n, breaks).}
}
\value{
A\code{tmap} object.
}
\description{
The \code{map()} function enables to plot maps containing relevant model outputs by accounting for their geographical dimension. The shapefile of the area must be provided via a \code{SpatialPolygonsDataFrame} or \code{sf} object.
}
\examples{
\dontrun{
library(tipsae)

# loading toy dataset
data("emilia_cs")

# fitting a model
fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
                    type_disp = "var", disp_direct = "vars", domain_size = "n",
                    # MCMC setting to obtain a fast example. Remove next line for reliable results.
                    chains = 1, iter = 150, seed = 0)

# check model diagnostics
summ_beta <- summary(fit_beta)

# load shapefile of concerned areas
data("emilia_shp")

# plot the map using model diagnostics and areas shapefile
map(x = summ_beta,
   spatial_df = emilia_shp,
   spatial_id_domains = "NAME_DISTRICT")
 }
}
\seealso{
\code{\link{summary.fitsae}} to produce the input object and \code{\link[sp]{SpatialPolygonsDataFrame}} to manage the shapefile.
}
