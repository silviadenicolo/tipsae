#' Density Plot Function for a `summary_fitsae` Object
#'
#' The method `density()` provides, in a grid (default) or sequence, the density plot of direct estimates versus HB model estimates and the density plot of standardized posterior means of the random effects versus standard normal.
#'
#' @inheritParams plot.summary_fitsae
#' @return Two `ggplot2` objects in a grid or in sequence.
#' @seealso \code{\link{summary.fitsae}} to produce the input object.
#' @examples \donttest{
#' library(tipsae)
#'
#' # loading toy dataset
#' data("emilia_cs")
#'
#' # fitting a model
#' fit_beta <- fit_sae(formula_fixed = hcr ~ x, data = emilia_cs, domains = "id",
#'                     type_disp = "var", disp_direct = "vars", domain_size = "n",
#'                     seed = 0)
#'
#' # check model diagnostics
#' summ_beta <- summary(fit_beta)
#'
#' # visualize estimates and random effect densities via density() function
#' density(summ_beta)}
#' @export
#'

density.summary_fitsae <- function(x,
                                   grid = TRUE,
                                   ...) {
  if (class(x) != "summary_fitsae")
    stop("Indicated object does not have 'summary_fitsae' class.")

  if (!grid) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  # Random effects
  dens_reff <- NULL
  if (!is.null(x$raneff$unstructured)) {
  data_reff_u <- data.frame(reff = scale(x$raneff$unstructured[, "mean"]))
  xlim_reff <- range(c(data_reff_u$reff, 2, -2))


  dens_reff <- ggplot2::ggplot(data_reff_u, ggplot2::aes_(x = ~ reff)) +
    ggplot2::geom_function(fun = dnorm,  ggplot2::aes(color = "Standard normal")) +
    ggplot2::stat_density(ggplot2::aes(color = "Scaled random effects"),
                          geom = "line", position = "identity") +
    ggplot2::ylab("Density") + ggplot2::xlim(xlim_reff) +
    ggplot2::theme_bw() + ggplot2::labs(color = "") +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_color_manual(values = c(
      "Scaled random effects" = "black",
      "Standard normal" = "grey")) +
    ggplot2::xlab("Unstructured random effect")
  }

  # Comparison estimates
  data_est <- data.frame(
    direct = x$direct_est,
    HB = x$post_means
  )

  xlim_dens = range(c(data_est$direct, data_est$HB))

  dens_est <- ggplot2::ggplot(data_est) +
    ggplot2::stat_density(ggplot2::aes_(x = ~ direct, color = "Direct est."),
                          geom = "line", position = "identity") +
    ggplot2::stat_density(ggplot2::aes_(x = ~ HB, color = "HB est."),
                          geom = "line", position = "identity") +
    ggplot2::theme_bw() + ggplot2::xlab("Estimates") +
    ggplot2::ylab("Density") + ggplot2::labs(color = "") +
    ggplot2::xlim(xlim_dens) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_color_manual(values = c("Direct est." = "black", "HB est." =
                                             "steelblue"))

  # Possible second kind of random effects
  dens_reff2 <- NULL
  if (!is.null(x$raneff$spatial)) {
    data_reff_s <- data.frame(reff = scale(x$raneff$spatial[, "mean"]))
    xlim_reff <- range(c(data_reff_s$reff, 2, -2))

    dens_reff2 <- ggplot2::ggplot(data_reff_s, ggplot2::aes_(x = ~ reff)) +
      ggplot2::geom_function(fun = dnorm,  ggplot2::aes(color = "Standard normal")) +
      ggplot2::stat_density(ggplot2::aes(color = "Scaled random effects"),
                            geom = "line", position = "identity") +
      ggplot2::ylab("Density") + ggplot2::xlim(xlim_reff) +
      ggplot2::theme_bw() + ggplot2::labs(color = "") +
      ggplot2::scale_color_manual(values = c(
        "Scaled random effects" = "black",
        "Standard normal" = "grey")) +
      ggplot2::xlab("Spatial random effect")
    if(is.null(dens_reff)){
      dens_reff <- dens_reff2
    }
  }
  if (!is.null(x$raneff$temporal)) {
    data_reff_t <- data.frame(reff = scale(x$raneff$temporal[, "mean"]))
    xlim_reff <- range(c(data_reff_t$reff, 2, -2))

    dens_reff2 <- ggplot2::ggplot(data_reff_t, ggplot2::aes_(x = ~ reff)) +
      ggplot2::geom_function(fun = dnorm,  ggplot2::aes(color = "Standard normal")) +
      ggplot2::stat_density(ggplot2::aes(color = "Scaled random effects"),
                            geom = "line", position = "identity") +
      ggplot2::ylab("Density") + ggplot2::xlim(xlim_reff) +
      ggplot2::theme_bw() + ggplot2::labs(color = "") +
      ggplot2::scale_color_manual(values = c(
        "Scaled random effects" = "black",
        "Standard normal" = "grey")) +
      ggplot2::xlab("Temporal random effect")
  }

  if (grid) {
    if (is.null(dens_reff2)) {
      gridExtra::grid.arrange(dens_reff, dens_est, ncol = 2)
    }else{
      leg <- ggpubr::get_legend(dens_reff)
      dens_reff <- dens_reff + ggplot2::theme(legend.position = "none")
      dens_reff2 <- dens_reff2 + ggplot2::theme(legend.position = "none")
      gridExtra::grid.arrange(dens_reff, dens_reff2, leg, dens_est,
                              ncol = 2, layout_matrix = rbind(c(1,2), c(3,3), c(4,4)),
                              heights = c(1,0.2,1.2))
      }
  } else{
    print(dens_est)
    print(dens_reff)
    if (!is.null(dens_reff2)) {
      print(dens_reff2)
    }
  }

}
