# Internal functions


create_data <-  function(formula_fixed,
                         data,
                         domain_size,
                         household_size,
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
  if (is.null(domain_size)) {
    domain_size_n <- NULL
  } else{
    domain_size_n <- data[!is_oos, domain_size]
  }

  if (is.null(household_size)) {
    household_size_n <- NULL
  } else{
    household_size_n <- data[!is_oos, household_size]
  }

  if (is.null(domains)) {
    domains_names <- 1:(nrow(data))
  } else{
    domains_names <- data[, domains]
  }

  # output
  return(
    list(
      y = y,
      X_scal = X,
      intercept = intercept,
      y_is = y_is,
      is_oos = is_oos,
      dispersion = dispersion,
      domain_size_n = domain_size_n,
      household_size_n = household_size_n,
      domains_names = domains_names,
      indices_is = indices_is,
      indices_oos = indices_oos,
      M_is = length(y_is)
    )
  )

}








arrange_spatial_structure <- function(spatial_error,
                                      spatial_df,
                                      data_obj) {
  if (spatial_error) {
    NB <- spdep::poly2nb(spatial_df) # list of neighbours
    comp <- spdep::n.comp.nb(NB)
    message("Building the spatial structure.")
    # disconnected graphs
    nc <- comp$nc
    # groups size
    group_sizes <- as.vector(table(comp$comp.id))
    # design matrix with island-specific intercepts
    intercept_islands <- NULL
    islands <- sum(group_sizes > 1)
    if (islands > 1) {
      intercept_islands <- model.matrix( ~ factor(comp$comp.id) - 1)
      sizes_islands <- colSums(intercept_islands)
      # biggest island as general intercept
      intercept_islands <- intercept_islands[, -which.max(sizes_islands)]
      if (!is.matrix(intercept_islands)) {
        intercept_islands <- matrix(intercept_islands, ncol = 1)
      }
      sizes_islands <- colSums(intercept_islands)
      # remove singletons
      intercept_islands <- intercept_islands[,sizes_islands != 1]
      if (!is.matrix(intercept_islands)) {
        intercept_islands <- matrix(intercept_islands, ncol = 1)
      }
      colnames(intercept_islands) <- paste0("intercept_island", 2:islands)

    }

    # ordering with respect to graphs
    indices_eff <- order(comp$comp.id)  # align the spatial effects to the rest of the data
    spatial_df_sort <- spatial_df[indices_eff,]
    spatial_df_sort$comp.id <- sort(comp$comp.id)
    NB_sort <-
      spdep::poly2nb(spatial_df_sort) # list of neighbours
    adj_matrix <-
      spdep::nb2mat(NB_sort,  style = "B", zero.policy = TRUE) # adj matrix
    # overall precision matrix
    nneighs <- rowSums(adj_matrix)#D
    # precision matrix: D-W
    K_location <- diag(nneighs) - adj_matrix
    scales <- numeric(0)
    for (i in 1:nc) {
      if (group_sizes[i] == 1) {
        scales <- c(scales, 1)
      } else{
        # scaling factor to account for the graph intrinsic variance
        eig_K <-
          eigen(K_location[spatial_df_sort$comp.id == i,
                           spatial_df_sort$comp.id == i],
                only.values = T)$values
        eig_Q <- 1 / eig_K[1:(group_sizes[i] - 1)]
        scale_factor <- sqrt(mean(eig_Q))
        scales <- c(scales, rep(sqrt(mean(eig_Q)), group_sizes[i]))
      }
    }

    # object with list of edges
    WB_object <- spdep::nb2WB(NB_sort)
    # vectors defining the edges
    N <- sum(WB_object$num != 0)
    N_edges <- sum(WB_object$num) / 2
    node1 <- numeric(N_edges)
    node2 <- numeric(N_edges)
    nums <- WB_object$num
    adjs <- WB_object$adj

    iAdj = 0
    iEdge = 0
    for (i in 1:N) {
      for (j in 1:nums[i]) {
        iAdj = iAdj + 1
        if (i < adjs[iAdj]) {
          iEdge = iEdge + 1
          node1[iEdge] = i
          node2[iEdge] = adjs[iAdj]
        }
      }
    }

    return(
      list(
        spatial_err = 1,
        N_edges = N_edges,
        N_comp = nc,
        dim_c = as.array(group_sizes),
        indices_spat = as.array(indices_eff),
        scales_ICAR = scales,
        node1 = node1,
        node2 = node2,
        intercept_islands = intercept_islands,
        islands = islands
      )
    )
  } else{
    return(
      list(
        spatial_err = 0,
        N_edges = 0,
        N_comp = 0,
        dim_c = numeric(0),
        indices_spat = as.array(1:length(unique(data_obj$domains_names))),
        scales_ICAR = as.array(1:length(unique(data_obj$domains_names))),
        node1 = numeric(0),
        node2 = numeric(0),
        intercept_islands = 0,
        islands = 0
      )
    )
  }

}




arrange_temporal_structure <-
  function(temporal_error,
           temporal_variable,
           data_obj,
           data) {
    if (temporal_error) {
      # time identifier variable
      if (!is.numeric(data[, temporal_variable])) {
        stop("The temporal variable must be numeric.")
      }
      time <- as.factor(data[, temporal_variable])
      TP <- length(levels(time)) # time periods
      D <- nrow(data) / TP # areas

      if (TP==1){
        stop("The dataset contains only one time period: a temporal effect cannot be specified.")
      }
      if (D %% 1 != 0) {
        stop("Each area should have the same observed time periods.")
      }

      num <- rep(0, length(levels(time)))
      indices_temp <- matrix(ncol = 2, nrow = nrow(data))
      mat_oos <- matrix(nrow = D, ncol = TP)
      for (i in 1:nrow(data)) {
        tl <- which(levels(time) == time[i])
        indices_temp[i, 2] <- tl
        num[tl] <- num[tl] + 1
        indices_temp[i, 1] <- num[tl]

        mat_oos[indices_temp[i, 1], indices_temp[i, 2]] <- data_obj$is_oos[i]
      }

      disc <- apply(mat_oos, 1, sum)
      cat <- matrix(nrow = D, ncol = TP)
      cat[disc == 0,] = rep(0, TP) # complete
      cat[disc == TP,] = rep(1, TP) # all missing
      mat_oos_lag <- cbind(TRUE, mat_oos[,-TP])
      for (i in which(disc < TP & disc > 0)) {
        cat[i,] <- ifelse(!mat_oos[i,], 0, cat[i,])
        cat[i,] <- ifelse(mat_oos[i,], ifelse(mat_oos_lag[i,], 2, 3), cat[i,]) # mancante in mezzo
      }

      # ricostruisco
      cat_ios <- numeric(nrow(data))
      for (i in 1:nrow(data)) {
        cat_ios[i] <- cat[indices_temp[i, 1], indices_temp[i, 2]]
      }

      # compute scaling factor
      eig_K_RW1 <-
        eigen(spam::precmat.RW1(TP), only.values = T)$values
      eig_Q_RW1 <- 1 / eig_K_RW1[1:(TP - 1)]
      scale_factor_RW1 <- sqrt(mean(eig_Q_RW1))

      return(
        list(
          temporal_err = 1,
          TP = TP,
          D = D,
          indices_temp = indices_temp,
          node1_t = 1:(TP - 1),
          node2_t = 2:TP,
          scale_factor_RW1 = scale_factor_RW1,
          cat_ios = as.array(cat_ios[data_obj$indices_oos])

        )
      )

    } else{
      return(
        list(
          temporal_err = 0,
          TP = 1,
          D = nrow(data),
          indices_temp = matrix(1, nrow = length(data_obj$y), ncol = 2),
          node1_t = numeric(0),
          node2_t = numeric(0),
          scale_factor_RW1 = 0,
          cat_ios = as.array(numeric(sum(data_obj$is_oos)))
        )
      )



    }


  }



dummy_standata <-
  function(standata,
           data_obj,
           type_disp,
           likelihood,
           prior_reff,
           prior_coeff,
           p0_HorseShoe) {
    # Deff
    standata$deff <- ifelse(type_disp == "neff", yes = 1, no = 0)

    # spatiotemporal
    standata$spatio_temporal <- ifelse(standata$temporal_err == 1 && standata$spatial_err == 1, yes = 1, no = 0)

    # Likelihood
    if (likelihood == "beta") {
      standata$likelihood = 0
    }
    if (likelihood == "flexbeta") {
      standata$likelihood = 1
    }
    # inits dummy inflation
    standata$inflation <- 0
    if (likelihood == "Infbeta0") {
      standata$likelihood = 2
    }
    if (likelihood == "Infbeta1") {
      standata$likelihood = 2
      standata$inflation <- 1
    }
    if (likelihood == "Infbeta01") {
      standata$likelihood = 2
      standata$inflation <- 2
    }
    if (likelihood == "ExtBeta") {
      standata$likelihood = 3
      standata$m_d <- data_obj$household_size_n
    }
    if (likelihood != "ExtBeta") {
      standata$m_d <- rep(0, length(data_obj$y_is))
    }
    # Prior reff
    if (prior_reff == "normal") {
      standata$prior_reff = 0
    }
    if (prior_reff == "t") {
      standata$prior_reff = 1
    }
    if (prior_reff == "VG") {
      standata$prior_reff = 2
    }

    # prior coeff
    if (prior_coeff == "normal") {
      standata$sigma_HS <- 0.5
      standata$p0_HS <- 0.5
      standata$slab_scale <- 0.5
      standata$slab_df <- 0.5
    } else{
      z_HS <-
        log((data_obj$y_is[data_obj$y_is > 0 | data_obj$y_is < 1]) /
              (1 - data_obj$y_is[data_obj$y_is > 0 | data_obj$y_is < 1]))
      mu_bar_HS <- exp(mean(z_HS)) / (1 + exp(mean(z_HS)))
      standata$sigma_HS <-  sqrt(var(data_obj$y_is - mean(data_obj$y_is)) /
                                   (mu_bar_HS * (1 - mu_bar_HS)) ^ 2)
      standata$p0_HS <- p0_HorseShoe
      standata$slab_scale <- 1
      standata$slab_df <- 7
    }
    return(standata)
  }



target_parameters <- function(standata,
           data_obj,
           likelihood,
           prior_reff) {

  pars_interest <- NULL
  if (standata$intercept == 1) {
    pars_interest <- c(pars_interest, "beta0")
  }
  pars_interest <-
      c(pars_interest, "beta", "theta", "y_rep", "log_lik")
  if (standata$spatio_temporal == 0) {
    pars_interest <- c(pars_interest,"v")
    if (prior_reff == "normal") {
      pars_interest <- c(pars_interest, "sigma_v")
    }
    if (prior_reff == "t") {
      pars_interest <- c(pars_interest, "sigma_v", "nu")
    }
    if (prior_reff == "VG") {
      pars_interest <- c(pars_interest, "psi_d", "lambda")
    }
  }
  if (likelihood == "flexbeta") {
      pars_interest <- c(pars_interest, "p", "lambda1", "lambda2")
  }
    if (likelihood == "Infbeta0") {
      if (standata$intercept == 1) {
        pars_interest <- c(pars_interest, "gamma0_p0")
      }
      pars_interest <- c(pars_interest, "gamma_p0", "p0", "mu")
    }
    if (likelihood == "Infbeta1") {
      if (standata$intercept == 1) {
        pars_interest <- c(pars_interest, "gamma0_p1")
      }
      pars_interest <- c(pars_interest, "gamma_p1", "p1", "mu")
    }
    if (likelihood == "Infbeta01") {
      if (standata$intercept == 1) {
        pars_interest <- c(pars_interest, "gamma0_p0", "gamma0_p1")
      }
      pars_interest <-
        c(pars_interest, "gamma_p0", "gamma_p1", "p0", "p1", "mu")
    }

    if (likelihood == "ExtBeta") {
      pars_interest <- c(pars_interest, "mu", "p0", "p1", "lambda_EB")
    }


    if (length(data_obj$y[data_obj$is_oos]) != 0 & likelihood != "flexbeta")
      pars_interest <- c(pars_interest, "theta_oos")
    if (standata$spatial_err == 1) {
      pars_interest <- c(pars_interest, "sigma_s", "s")
    }
    if (standata$temporal_err == 1) {
      pars_interest <- c(pars_interest, "sigma_t", "t")
    }

    return(pars_interest)
  }
