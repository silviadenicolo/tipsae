#' Bayesian proportions/(0,1)-measures small area model with Stan
#'
#' @param x Object of class 'estimates_fitsae' or 'benchmark_fitsae'.
#' @return to do
#' @export
#'
#'

export <- function(x,
                             file,
                             type = "all", ...) {

  if (any(class(x) != c("estimates_fitsae", "list"))) # & any(class(x) != "benchmark_fitsae"))
    stop("Indicated object does not have 'estimates_fitsae' class.")
  if (!type %in% c("in", "out", "all"))
    stop("Type has to be specified as 'in', 'out' or 'all'.")

  if (is.null(x$out_of_sample) & type == "all")
    type <- "in"

  if (any(class(x) == "benchmark_fitsae")) {
    ex <- extract(x)
  } else {
    ex <- x
  }


  if (type == "all") {
    out <- rbind(ex$in_sample, ex$out_of_sample)
  } else {
    if (type == "out") {
      out <- ex$out_of_sample
    } else {
      out <- ex$in_sample
    }
  }


  write.csv(out, file, ...)
}
