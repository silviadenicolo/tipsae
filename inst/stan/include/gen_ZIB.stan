for(i in 1:M_oos) { // OOS units
#include /inst/stan/include/gen_reff_OOS.stan
   if(inflation == 0) {
    if(intercept == 0) {
      theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p0)) *
        inv_logit(X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]);
    }else{
      theta_oos[i] = (1 - inv_logit(gamma0_p0[1] + X_oos[i, ] * gamma_p0)) *
        inv_logit(beta0[1] + X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]);
    }
  }else if(inflation == 1) {
    if(intercept == 0) {
      theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p1)) *
        inv_logit(X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]) +
        inv_logit(X_oos[i, ] * gamma_p1);
    }else{
      theta_oos[i] = (1 - inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1)) *
        inv_logit(beta0[1] + X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]) +
        inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1);
    }
  }else if(inflation == 2) {
    if(intercept == 0) {
      theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p0) -
        inv_logit(X_oos[i, ] * gamma_p1)) * inv_logit(X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]) +
        inv_logit(X_oos[i, ] * gamma_p1);
    }else{
      theta_oos[i] = (1 - inv_logit(gamma0_p0[1] + X_oos[i, ] * gamma_p0) -
        inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1)) *
        inv_logit(beta0[1] + X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]) +
        inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1);
    }
  }
}
for(i in 1:M_is) {
  if(y[i] == 0 && inflation != 1) {
    log_lik[i] = log(p0[i]);
  }else if(y[i] == 1 && inflation != 0) {
    log_lik[i] = log(p1[i]);
  }else{
    if(inflation == 0) {
      log_lik[i] = log(1 - p0[i]) + beta_lpdf(y[i] | a1[i], b1[i]);
    }else if(inflation == 1) {
      log_lik[i] = log(1 - p1[i]) + beta_lpdf(y[i] | a1[i], b1[i]);
    }else if(inflation == 2) {
      log_lik[i] = log(1 - p0[i] - p1[i]) + beta_lpdf(y[i] | a1[i], b1[i]);
    }
  }
  if(inflation == 0) {
    label_mixt = bernoulli_rng(p0[i]);
    if(label_mixt == 1) {
      y_rep[i] = 0;
    }else{
      y_rep[i] = beta_rng(a1[i], b1[i]);
    }
  }else if(inflation == 1) {
    label_mixt = bernoulli_rng(p1[i]);
    if(label_mixt == 1) {
      y_rep[i] = 1;
    }else{
      y_rep[i] = beta_rng(a1[i], b1[i]);
    }
  }else if(inflation == 2) {
    probs[1] = p0[i];
    probs[2] = 1 - p0[i] - p1[i];
    probs[3] = p1[i];
    label_mixt = categorical_rng(probs);
    if(label_mixt == 1) {
      y_rep[i] = 0;
    }else if(label_mixt == 2) {
      y_rep[i] = beta_rng(a1[i], b1[i]);
    }else if(label_mixt == 3) {
      y_rep[i] = 1;
    }
  }
}
