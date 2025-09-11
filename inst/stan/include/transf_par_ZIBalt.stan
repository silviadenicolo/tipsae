
for(i in 1:M_is) {
  if(intercept == 0) {
    mu[i] = inv_logit(X[i, ] * beta + reffs[i]);
  }else{
    mu[i] = inv_logit(beta0[1] + X[i, ] * beta + reffs[i]);
  }
  #include /include/transf_par_ab.stan
}
  // Lower bound lambda parameter
  which_min_EB[1] = 0;
  which_min_EB[2] = max((2 * mu - 1) ./ mu);
  min_lambda_EB[1] = max(which_min_EB);
  // Definition lambda parameter
  lambda_EB[1] = min_lambda_EB[1] + (1 - min_lambda_EB[1]) * lambda_star_EB[1];
  // Other model parameters depending on theta
  for (i in 1:M_is){
    // Probabilities of observing zeros and ones
    p0[i] = exp((m_d[i] - 1.0) * log(1 + mu[i] * (lambda_EB[1] - 2)) - (m_d[i] - 2) * log(1 - mu[i]));
    p1[i] = exp(log(mu[i]) + (m_d[i] - 1) * log(lambda_EB[1]));
    // Expectation
    theta[i] = mu[i] * (1 - p0[i] - p1[i]) + p1[i];
  }



