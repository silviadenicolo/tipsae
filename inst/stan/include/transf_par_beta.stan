// for(i in 1:M_oos) { // OOS units
//   if(intercept == 0) {
//     theta_oos[i] = inv_logit(X_oos[i,] * beta);
//   }else{
//     theta_oos[i] = inv_logit(beta0[1] + X_oos[i, ] * beta);
//   }
//   if(spatial_err == 1 || temporal_err == 1) {
//     theta_oos[i] = inv_logit(logit(theta_oos[i]) + reffs_oos[i]);
//   }
// }
for(i in 1:M_is) { // IS units
  if(intercept == 0) {
    mu[i] = inv_logit(X[i, ] * beta + reffs[i]);
  }else{
    mu[i] = inv_logit(beta0[1] + X[i, ] * beta + reffs[i]);
  }
  theta[i] = mu[i];
  // definition of parameters a and b
#include /include/transf_par_ab.stan
}
