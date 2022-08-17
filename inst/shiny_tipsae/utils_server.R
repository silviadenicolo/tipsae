
create_data_shiny <-  function(formula_fixed,
                               terms_formula,
                               data,
                               domain_size,
                               domains,
                               disp_direct) {
  if (inherits(data, "tbl")) {
    data <- as.data.frame(data)
  }
  # Model frame
  mf <- model.frame(formula_fixed, data, na.action = na.pass)
  type_var <- attr(attr(mf, "terms"), "dataClasses")[-1]

  # Design matrix
  X <- model.matrix(update(formula_fixed, NULL ~ .) , data)
  ass <- attr(X, "assign")
  type_var_X <- type_var[ass] # type of column
  # intercept removed: in case added later
  intercept <- 0
  if (ass[1] == 0) {
    X <- X[, -1, drop = FALSE]
    intercept <- 1
  }
  # if the column is numeric, it is standardised
  for (i in 1:ncol(X)) {
    if (type_var_X[i] == "numeric") {
      X[,i] <- scale(X[,i])
    }
  }
  ## Response
  y <- model.extract(mf, "response")
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
      X_scal = X,
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



