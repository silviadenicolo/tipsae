#' The 'tipsae' Package.
#'
#' @description It provides tools for mapping proportions and indicators defined on the unit interval, widely used to measure, for instance, unemployment, educational attainment and also disease prevalence. It implements Beta-based small area methods, particularly indicated for unit interval responses, comprising the classical Beta regression models, the Flexible Beta model and Zero and/or One Inflated extensions. Such methods, developed within a Bayesian framework, come equipped with a set of diagnostics and complementary tools, visualizing and exporting functions. A customized parallel computing is built-in to reduce the computational time. The features of the tipsae package assist the user in carrying out a complete SAE analysis through the entire process of estimation, validation and results presentation, making the application of Bayesian algorithms and complex SAE methods straightforward. A Shiny application with a user-friendly interface can be launched to further simplify the process.
#'
#' @useDynLib tipsae, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import shiny
#' @importFrom rstan sampling
#' @importFrom grDevices devAskNewPage
#' @importFrom stats setNames as.formula complete.cases model.matrix na.omit terms var density dnorm model.extract model.frame na.pass update
#' @importFrom utils capture.output write.csv
#' @importFrom Rdpack reprompt
#'
#'@author Silvia De Nicolò, \email{silvia.denicolo@@unibo.it}
#'@author Aldo Gardini, \email{aldo.gardini@@unibo.it}
#'
#' @references
#'
#' \insertRef{JSS}{tipsae}
#'
#' \insertRef{rstan}{tipsae}
#'
#' \insertRef{carpenter2017stan}{tipsae}
#'
#' \insertRef{janicki2020properties}{tipsae}
#'
#' \insertRef{vehtari2017practical}{tipsae}
#'
#' \insertRef{datta2011bayesian}{tipsae}
#'
#' \insertRef{kish1992weighting}{tipsae}
#'
#' \insertRef{fabrizi2011hierarchical}{tipsae}
#'
#' \insertRef{morris2019bayesian}{tipsae}
#'
#' \insertRef{DeNicolo2021}{tipsae}
#'
#' \insertRef{shiny}{tipsae}
#'
#'

"_PACKAGE"
