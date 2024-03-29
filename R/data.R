
#' Poverty in Emilia-Romagna (Italy) Health Districts
#'
#' The \code{emilia} dataset consists of a panel on poverty mapping concerning 38 health districts within the Emilia-Romagna region, located in North-East of Italy, with annual observations recorded from 2014 to 2018.
#'
#' It has been built starting from model-based estimates and related CV freely available on Emilia-Romagna region \href{https://statistica.regione.emilia-romagna.it/documentazione/pubblicazioni/documenti_catalogati/stima-poverta-2009-2018-distretti-sociosanitari-province-emilia-romagna}{website}. Since it is used for illustrative purposes only, such estimates are assumed to be unreliable direct estimates, requiring a SAE procedure.
#'
#' @format Dataframe with 190 observations and 8 variables.
#'
#' \describe{
#' \item{\code{id}}{Character, name of the health district.}
#' \item{\code{prov}}{Character, name of NUTS-3 region related to the district.}
#' \item{\code{year}}{Numeric, year of the observation.}
#' \item{\code{hcr}}{Numeric, head-count ratio estimate (used as response variable).}
#' \item{\code{vars}}{Numeric, sampling variance of head-count ratio estimator.}
#' \item{\code{n}}{Numeric, area sample size.}
#' \item{\code{x}}{Numeric, fake covariate.}
#' \item{\code{pop}}{Numeric, population size of the area.}
#' }
#'
#' @examples \donttest{library(tipsae)
#' data("emilia")}
#'
#'
#'@keywords datasets
"emilia"

#' Poverty in Emilia-Romagna (Italy) Health Districts in 2016
#'
#' The \code{emilia} dataset consists of a dataset on poverty mapping concerning 38 health districts within the Emilia-Romagna region, located in North-East of Italy, with observations recorded in 2016.
#'
#' It has been built starting from model-based estimates and related CV freely available on Emilia-Romagna region \href{https://statistica.regione.emilia-romagna.it/documentazione/pubblicazioni/documenti_catalogati/stima-poverta-2009-2018-distretti-sociosanitari-province-emilia-romagna}{website}. Since it is used for illustrative purposes only, such estimates are assumed to be unreliable direct estimates, requiring a SAE procedure.
#'
#' @format Dataframe with 38 area observations and 8 variables.
#'
#' \describe{
#' \item{\code{id}}{Character, name of the health district.}
#' \item{\code{prov}}{Character, name of NUTS-3 region related to the district.}
#' \item{\code{year}}{Numeric, year of the observation.}
#' \item{\code{hcr}}{Numeric, head-count ratio estimate (used as response variable).}
#' \item{\code{vars}}{Numeric, sampling variance of head-count ratio estimator.}
#' \item{\code{n}}{Numeric, area sample size.}
#' \item{\code{x}}{Numeric, fake covariate.}
#' \item{\code{pop}}{Numeric, population size of the area.}
#' }
#'
#' @seealso \code{\link{emilia}} for the panel dataset including observation from 2014 to 2018.
#' @examples \donttest{library(tipsae)
#' data("emilia_cs")}
#'
#' @keywords datasets
"emilia_cs"

#' Shapefile of Emilia-Romagna (Italy) Health Districts
#'
#' The \code{emilia_shp} shapefile consists of a `SpatialPolygonsDataFrame` object of 38 health districts within the Emilia-Romagna region, located in the North-East of Italy.
#'
#' @format A shapefile of class `SpatialPolygonsDataFrame`.
#'
#' \describe{
#' \item{\code{COD_DIS_SA}}{Code of the health district.}
#' \item{\code{NAME_DISTRICT}}{Name of the health district. It can be linked to the variable \code{id} in \code{\link{emilia}} and \code{\link{emilia_cs}}}
#' }
#'
#' @seealso \code{\link{emilia}} and \code{\link{emilia_cs}} for the provided datasets.
#' @examples \donttest{
#' library(tipsae)
#' library(sp)
#' data("emilia_shp")
#' }
#'
#' @keywords datasets
#'
"emilia_shp"
