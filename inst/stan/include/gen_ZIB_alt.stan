for(i in 1:M_oos) {
#include /include/gen_reff_OOS.stan
  if(intercept == 0) {
    theta_oos[i] = inv_logit(X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]);
  }else{
    theta_oos[i] = inv_logit(beta0[1] + X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]);
  }
}
for (i in 1:M_is) {
  if(y[i] == 0){
    log_lik[i] = m_d[i] * log(1 - mu[i]);
  }else{
    log_lik[i] = log(1 - pow(1 - mu[i], m_d[i])) + beta_lpdf(y[i] |a1[i], b1[i]);
  }
  y_rep[i] = beta_rng(a1[i], b1[i]);
}
for (i in 1:M_is) {
  label_mixt = bernoulli_rng(pow(1 - mu[i], m_d[i]));
  if(label_mixt == 1) {
    y_rep[i] = 0.0;
  }else{
    y_rep[i] = beta_rng(a1[i], b1[i]);
  }
}
