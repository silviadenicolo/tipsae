
create_data_shiny <-  function(formula_fixed,
                               terms_formula,
                               data,
                               domain_size,
                               domains,
                               disp_direct) {
  if (any(class(data) == "tbl")) {
    data <- as.data.frame(data)
  }
  # scaled auxiliary variables
  X <- data[, attr(terms_formula, which = 'term.labels')]
  if (length(attr(terms_formula, which = 'term.labels')) == 1) {
    X <- matrix(X, ncol = 1)
  }
  scaled.aux <- scale(X)
  colnames(scaled.aux) <- attr(terms_formula, which = 'term.labels')
  ## Response
  y <-
    data[, all.vars(formula_fixed)[!(all.vars(formula_fixed) %in%
                                       attr(terms_formula, which = 'term.labels'))]]
  ## Logical for out of sample areas
  is_oos <- is.na(y)
  y_is <- y[!is_oos]
  # ## indices
  indices <- 1:(length(y))
  indices_is <- indices[!is_oos]
  indices_oos <- indices[is_oos]
  ## Dispersion and domain size
  dispersion <- data[!is_oos, disp_direct]
  if (domain_size == "Unspecified") {
    domain_size_n <- NULL
  } else{
    domain_size_n <- data[!is_oos, domain_size]
  }

  if (domains == "Unspecified") {
    domains_names <- 1:(nrow(data))
  } else{
    domains_names <- data[, domains]
  }
  domains_names <- factor(domains_names)
  # output
  return(
    list(
      y = y,
      X_scal = scaled.aux,
      y_is = y_is,
      is_oos = is_oos,
      dispersion = dispersion,
      domain_size_n = domain_size_n,
      domains_names = domains_names,
      indices_is = indices_is,
      indices_oos = indices_oos,
      M_is = length(y_is)
    )
  )

}



