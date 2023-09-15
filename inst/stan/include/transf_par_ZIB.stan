// for(i in 1:M_oos) { // OOS units
//    if(inflation == 0) {
//     if(intercept == 0) {
//       theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p0)) *
//         inv_logit(X_oos[i, ] * beta);
//     }else{
//       theta_oos[i] = (1 - inv_logit(gamma0_p0[1] + X_oos[i, ] * gamma_p0)) *
//         inv_logit(beta0[1] + X_oos[i, ] * beta);
//     }
//   }else if(inflation == 1) {
//     if(intercept == 0) {
//       theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p1)) *
//         inv_logit(X_oos[i, ] * beta) +
//         inv_logit(X_oos[i, ] * gamma_p1);
//     }else{
//       theta_oos[i] = (1 - inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1)) *
//         inv_logit(beta0[1] + X_oos[i, ] * beta) +
//         inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1);
//     }
//   }else if(inflation == 2) {
//     if(intercept == 0) {
//       theta_oos[i] = (1 - inv_logit(X_oos[i, ] * gamma_p0) -
//         inv_logit(X_oos[i, ] * gamma_p1)) * inv_logit(X_oos[i, ] * beta) +
//         inv_logit(X_oos[i, ] * gamma_p1);
//     }else{
//       theta_oos[i] = (1 - inv_logit(gamma0_p0[1] + X_oos[i, ] * gamma_p0) -
//         inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1)) *
//         inv_logit(beta0[1] + X_oos[i, ] * beta) +
//         inv_logit(gamma0_p1[1] + X_oos[i, ] * gamma_p1);
//     }
//   }
//   if(spatial_err==1 || temporal_err==1) {// adding reff if a structure is included in the model
//     theta_oos[i] = inv_logit(logit(theta_oos[i]) + reffs_oos[i]);
//   }
// }
for(i in 1:M_is) { //IS units
  if(intercept == 0) {
    mu[i] = inv_logit(X[i, ] * beta + reffs[i]);
  }else{
    mu[i] = inv_logit(beta0[1] + X[i, ] * beta + reffs[i]);
  }
  if(inflation != 1) {// 0 and 0-1 inflation
    if(intercept == 0) {
      p0[i] = inv_logit(X[i, ] * gamma_p0);
    }else{
      p0[i] = inv_logit(gamma0_p0[1] + X[i, ] * gamma_p0);
    }
  }
  if(inflation != 0) {// 1 and 0-1 inflation
    if(intercept == 0) {
      p1[i] = inv_logit(X[i, ] * gamma_p1);
    }else{
      p1[i] = inv_logit(gamma0_p1[1] + X[i, ] * gamma_p1);
    }
  }
  if(inflation == 0) {
    theta[i] = (1 - p0[i]) * mu[i];
  }else if(inflation == 1) {
    theta[i] = (1 - p1[i]) * mu[i] + p1[i];
  }else if(inflation == 2) {
    theta[i] = (1 - p0[i] - p1[i]) * mu[i] + p1[i];
  }
#include /include/transf_par_ab.stan
}
