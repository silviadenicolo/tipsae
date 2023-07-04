#' Exporting Results of a Small Area Model Fitting
#'
#' The function `export()` allows for exporting model estimates in CSV format.
#'
#' @param x An object of class `estimates_fitsae`.
#' @param file  A character string indicating the path (if different from the working directory) and filename of the CSV to be created. It should end with .csv.
#' @param type An option between `"in"`, `"out"` and `"all"`, indicating whether to export only in or out-of-sample areas or both.
#' @param ... Additional arguments of \code{\link[utils]{write.csv}} function from `utils` package can be indicated.
#' @return A CSV file is created in the working directory, or at the given path, exporting the `estimates_fitsae` object given as input.
#'
#' @seealso \code{\link{extract}} to produce the input object and \code{\link[utils]{write.csv}}.
#'
#' @examples \dontrun{
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # fitting a model
#' fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
#'                     type_disp = "var", disp_direct = "vars", domain_size = "n",
#'                     # MCMC setting to obtain a fast example. Remove next line for reliable results.
#'                     chains = 1, iter = 150, seed = 0)
#'
#' # check model diagnostics
#' summ_beta <- summary(fit_beta)
#'
#' # extract model estimates
#' HB_estimates <- extract(summ_beta)
#'
#' # export model estimates
#' export(HB_estimates, file = "results.csv", type = "all")
#' }
#'
#'
#' @export
#'
#'

export <- function(x,
                   file,
                   type = "all",
                   ...) {

  if (!inherits(x, c("estimates_fitsae", "list"))) # & any(class(x) != "benchmark_fitsae"))
    stop("Indicated object does not have 'estimates_fitsae' class.")
  if (!type %in% c("in", "out", "all"))
    stop("Type has to be specified as 'in', 'out' or 'all'.")

  if (is.null(x$out_of_sample) & type == "all")
    type <- "in"

  if (type == "all") {
    out <- rbind(x$in_sample, x$out_of_sample)
  } else {
    if (type == "out") {
      out <- x$out_of_sample
    } else {
      out <- x$in_sample
    }
  }


  write.csv(out, file, ...)
}
