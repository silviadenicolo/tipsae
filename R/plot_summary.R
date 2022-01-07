#' Bayesian proportions/(0,1)-measures small area model with Stan
#'
#' @param x Object of class 'summary.fitsae'.
#' @param areas_sample_size Areas-specific sample sizes.
#' @param size, denoting size of scatterplots points (geom_point).
#' @param alpha, denoting  opacity of scatterplots points (geom_point).
#' @param n_bins, denoting number of bins used for histogram.
#' @param grid, logical, indicating whether displaying plots in a grid (TRUE) or in sequence (FALSE).
#' @param label_names, model name to display in boxplot x-axis label.
#' @return Four ggplot2 objects stored in a grid.
#' @export
#'

plot.summary_fitsae <- function(x,
                   size = 2.5,
                   alpha = 0.8,
                   n_bins = 15,
                   grid = TRUE,
                   label_names = NULL,
                   ...
                   ){
  if (class(x) != "summary_fitsae")
    stop("Indicated object does not have 'summary_fitsae' class.")

  if (is.null(x$data_obj$domain_size_n))
    message("To check for the design consistency the domains sample size is required. \n
            Specify the argument 'domain_size' of the fit_sae() function.")

  if (!grid) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))}

    # Arranging dataset
    xydata <- data.frame(x = x$direct_est,
                         y = x$post_means,
                         res = x$residuals,
                         bp = x$bayes_pvalues,
                         ind = ifelse(!is.null(label_names), label_names, "Mod1")
                         )
    if (is.null(x$data_obj$domain_size_n)) {
      xydata$n <- NA
    }else{
      xydata$n <- x$data_obj$domain_size_n
    }
    if (is.null(x$sd_reduction)) {
      xydata$sdr <- NA
    }else{
      xydata$sdr <- x$sd_reduction
    }

    # Boxplot for standard deviation reduction
    boxplot_sdr <- ggplot2::ggplot(xydata, ggplot2::aes_(y = ~ sdr, x = ~ ind)) +
      ggplot2::theme_bw() + ggplot2::xlab("") + ggplot2::ylab("S.D. Reduction") +
      ggplot2::geom_boxplot() +
      ggplot2::theme(axis.text.x =  ggplot2::element_blank())

    if (0 >= min(xydata$sdr) & 0 <= max(xydata$sdr))
      boxplot_sdr <- boxplot_sdr + ggplot2::geom_hline(yintercept = 0)

    # Plot direct vs model estimates
    lims_axis <- range(c(x$direct_est, x$post_means))
    scatter_s <- ggplot2::ggplot(data = xydata, ggplot2::aes_(x = ~ x, y = ~ y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0) +
      ggplot2::xlim(lims_axis) + ggplot2::ylim(lims_axis) +
      ggplot2::theme(aspect.ratio = 1) +
      ggplot2::ylab("HB est.") +
      ggplot2::xlab("Direct est.") +
      ggplot2::theme_bw() +
      ggplot2::geom_point(
        shape = 20,
        size = size,
        alpha = alpha
      )

    # Histogram Bayesian p-values
    hist_bp <- ggplot2::ggplot(data = xydata, ggplot2::aes_(x = ~ bp)) +
      ggplot2::xlim(0, 1) + ggplot2::theme_bw() +
      ggplot2::xlab("Bayesian p-values") + ggplot2::ylab("") +
      ggplot2::geom_histogram(bins = n_bins,
                              color = "black",
                              fill = "white")

    # Design consistency
    if (!is.null(x$data_obj$domain_size_n)) {
      scatter_dc <- ggplot2::ggplot(data = xydata, ggplot2::aes_(x = ~ n, y = ~ res)) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::theme_bw() +
        ggplot2::geom_point(
          shape = 20,
          size = size,
          alpha = alpha
        ) + ggplot2::ylab("Residuals") +
        ggplot2::xlab("Domain sample size")

    if (grid) {
      gridExtra::grid.arrange(scatter_s, hist_bp,
                              boxplot_sdr, scatter_dc,
                              ncol = 2)
    }else{
    print(scatter_s)
    print(hist_bp)
    print(boxplot_sdr)
    print(scatter_dc)
    }
  }else{
    if (grid) {
      gridExtra::grid.arrange(scatter_s, hist_bp,
                              boxplot_sdr,
                              ncol = 2)
      }else{
        print(scatter_s)
        print(hist_bp)
        print(boxplot_sdr)
      }
  }
}
