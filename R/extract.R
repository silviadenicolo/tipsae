#' @export
#'

extract <- function(x) {
  if (!(class(x) %in% c("summary_fitsae", "benchmark_fitsae")))
    stop("Indicated object does not have 'summary_fitsae' class.")

  if(class(x)=="benchmark_fitsae" & !is.null(x$time))
    stop("extract_estimates function cannot be used for 'benchmark_fitsae' object not comprising all areas and/or times.")

  if (x$model_settings$temporal_error) {
    out <- list()
    out$in_sample <- cbind(x$model_estimates[,1:2], x$direct_est, x$model_estimates[,-c(1:2)])
    out$in_sample <- as.data.frame(out$in_sample)
    names(out$in_sample)[which(names(out$in_sample) == "mean")] <- "HB est."
    names(out$in_sample)[3] <- "Direct est."
    names(out$in_sample)[1] <- "Domains"
    rownames(out$in_sample) <- NULL

    if(any(class(x) == "benchmark_fitsae")){
      if(is.null(x$areas)){
        tm <-(x$data_obj$times==x$time)[!x$is_oos]
        mt <- (!x$is_oos)[x$data_obj$times==x$time]
        out$in_sample$"Bench est." <- NA
        out$in_sample$"Bench est."[tm] <- x$bench_est[mt]
      } else{
        stop("benchmark time x area to fix")
      }}

    if (sum(x$is_oos) > 0) {
      out$out_of_sample <- cbind(x$model_estimates_oos[,1:2], NA, x$model_estimates_oos[,-c(1:2)])
      out$out_of_sample<- as.data.frame(out$out_of_sample)
      names(out$out_of_sample)[which(names(out$out_of_sample) == "mean")] <- "HB est."
      names(out$out_of_sample)[3] <- "Direct est."
      names(out$out_of_sample)[1] <- "Domains"
      rownames(out$out_of_sample) <- NULL

      if(any(class(x) == "benchmark_fitsae")){
        if(is.null(x$areas)){
          tm <-(x$data_obj$times==x$time)[x$is_oos]
          mt <- (x$is_oos)[x$data_obj$times==x$time]
          out$out_of_sample$"Bench est." <- NA
          out$out_of_sample$"Bench est."[tm] <- x$bench_est[mt]
        } else{
          stop("benchmark time x area to fix")
        }}

    }
  }else{
    out <- list()
    out$in_sample <- cbind(x$model_estimates[,1], x$direct_est, x$model_estimates[,-1])

    out$in_sample <- as.data.frame(out$in_sample)
    names(out$in_sample)[which(names(out$in_sample) == "mean")] <- "HB est."
    names(out$in_sample)[2] <- "Direct est."
    names(out$in_sample)[1] <- "Domains"

    if(any(class(x) == "benchmark_fitsae")){
      if(is.null(x$areas)){
        out$in_sample$"Bench est." <- x$bench_est[!x$is_oos]
      } else{
        mat <- match(x$areas, out$in_sample$"Domains")
        out$in_sample$"Bench est." <- NA
        out$in_sample$"Bench est."[mat[!is.na(mat)]]<- x$bench_est[!is.na(mat)]
      }}
    rownames(out$in_sample) <- NULL


    if (sum(x$is_oos) > 0) {
      out$out_of_sample <- cbind(x$model_estimates_oos[,1], NA, x$model_estimates_oos[,-1])

      out$out_of_sample <- as.data.frame(out$out_of_sample)
      names(out$out_of_sample)[which(names(out$out_of_sample) == "mean")] <- "HB est."
      names(out$out_of_sample)[2] <- "Direct est."
      names(out$out_of_sample)[1] <- "Domains"

      if(any(class(x) == "benchmark_fitsae")){
        if(is.null(x$areas)){
          out$out_of_sample$"Bench est." <- x$bench_est[x$is_oos]
        } else{
          mat <- match(x$areas, out$out_of_sample$"Domains")
          out$out_of_sample$"Bench est." <- NA
          out$out_of_sample$"Bench est."[mat[!is.na(mat)]]<- x$bench_est[!is.na(mat)]
        }}
      rownames(out$out_of_sample) <- NULL
    }


  }
  class(out) <- c("estimates_fitsae", "list")
  return(out)

}

#' @export
#'
#'
#'


print.estimates_fitsae <- function(x, digits = 3L, ...) {
  if (any(class(x) != c("estimates_fitsae", "list")))
    stop("Indicated object does not have 'estimates_fitsae' class.")

  cat("Target parameter's posterior summaries:")
  cat("\n")
  cat("\n")
  print(cbind(x$in_sample[colnames(x$in_sample) %in% c("Domains", "Times")],
              round(x$in_sample[!(colnames(x$in_sample) %in% c("Domains", "Times"))], digits = digits)
  )
  )
  if (length(x) > 1 ) {
    cat("\n")
    cat("Target parameter's posterior summaries for out-of-sample areas:")
    cat("\n")
    cat("\n")
    print(cbind(x$out_of_sample[colnames(x$out_of_sample) %in% c("Domains", "Times")],
                round(x$out_of_sample[!(colnames(x$out_of_sample) %in% c("Domains", "Times"))], digits = digits)
    )
    )
  }
}



