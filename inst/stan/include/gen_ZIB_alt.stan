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
      log_lik[i] = log(p0[i]);
    }else if(y[i] == 1){
      log_lik[i] = log(p1[i]);
    }else{
      log_lik[i] = log(1 - p0[i] - p1[i]) + beta_lpdf(y[i] | a1[i], b1[i]);
    }
}
for (i in 1:M_is) {
  probs[1] = p0[i];
  probs[2] = 1 - p0[i] - p1[i];
  probs[3] = p1[i];
  label_mixt = categorical_rng(probs);
    if(label_mixt == 1){
        y_rep[i] = 0;
      }else if(label_mixt == 2){
        y_rep[i] = beta_rng(a1[i], b1[i]);
      }else if(label_mixt == 3){
        y_rep[i] = 1;
    }
}
