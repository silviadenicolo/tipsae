data {
#include /inst/stan/include/data.stan
 }
transformed data{
  int<lower=0> M_overall = M_is + M_oos; // number of observations (is+oos)
  // for HS
  real<lower=0> slab_scale2 = square(slab_scale);
  real<lower=0> half_nu = 0.5 * slab_df;
  real<lower=0> tau0;
  int<lower=0> length_v;
  if(P==0){
    tau0 = 1;
  }else{
    tau0 = (p0_HS / (1.0 * P - p0_HS)) * (sigma_HS / sqrt(1.0 * M_is));
  }
  if(temporal_err == 0){
    length_v = M_is;
  }else{
    if(spatio_temporal == 1){
      length_v = 0;
    }else{
      length_v = D;
    }
  }
}
parameters {
#include /inst/stan/include/parameters.stan
}

transformed parameters {
#include /inst/stan/include/transf_par_declaration.stan

// Regression coefficients - Fixed effect
  if(prior_coeff==0) { // normal prior
    beta = z_beta;
  }else if (prior_coeff==1) {// HS prior
    tau[1] = tau0 * z_tau[1];
    c2[1] = slab_scale2 * z_c2[1];
    lambda_tilde = sqrt(c2[1] * square(lambda_HS) ./ (c2[1] + square(tau[1]) * square(lambda_HS)));
    beta = tau[1] * lambda_tilde .* z_beta;
  }

// Unstructured random effects
  if(spatio_temporal == 0){
    if(prior_reff==0){
      v = sigma_v[1] * v_raw;
    }else if(prior_reff==1){
      v = sigma_v[1] * v_raw;
    }else if(prior_reff==2){
      v = (sqrt(psi_d)*lambda[1]) .* v_raw;
    }
  }

// Overall random effect part
#include /inst/stan/include/transf_par_reff.stan

  // diversi theta a seconda delle verosimiglianze
  if(likelihood==0){ // beta
#include /inst/stan/include/transf_par_beta.stan
  }else if(likelihood==1){ // FB
  // da capire OOS//
  /////////////////
#include /inst/stan/include/transf_par_FB.stan
  }else if(likelihood==2){ // ZIB
#include /inst/stan/include/transf_par_ZIB.stan
  }else if(likelihood==3){// beta FFT
#include /inst/stan/include/transf_par_ZIBalt.stan
  }
}
model{
  // Auxiliary quantity
  int pos;
  pos = 1;

  // Prior fixed effects
  if(intercept==1) {
    beta0[1] ~ normal(0, sigma_coeff);
  }
  if(prior_coeff == 0){
    z_beta ~ normal(0, sigma_coeff);
  }else if (prior_coeff == 1) {
    z_beta ~ normal(0,1);
    lambda_HS ~ cauchy(0, 1);
    z_tau[1] ~ cauchy(0, 1);
    z_c2[1] ~ inv_gamma(half_nu, half_nu);
  }

  // Prior unstructured random effect
  if(spatio_temporal == 0){
    if(prior_reff == 0) {// std normal
      v_raw ~ std_normal();
      sigma_v[1] ~ normal(0, sigma_unstr)T[0, ];
    }else if(prior_reff == 1) {// half t (3 dof)
      v_raw ~ student_t(nu[1], 0, 1);
      sigma_v[1] ~ normal(0, sigma_unstr)T[0, ];
      nu[1] ~ exponential(0.1);
    }else if(prior_reff == 2) {//VG
      v_raw ~ std_normal();
      psi_d ~ gamma(0.5, 1);
      lambda[1] ~ normal(0, sigma_unstr);
    }
  }
  // Prior Spatial random effect
  if(spatial_err == 1) {
    sigma_s[1] ~ normal(0, 2.5)T[0, ];
    for (k in 1:N_comp) {
      if(dim_c[k] == 1){ //singletons
         segment(s_raw, pos, dim_c[k]) ~ normal(0, 1);
      }
      if(dim_c[k] > 1) { // connected areas
        sum(segment(s_raw, pos, dim_c[k])) ~ normal(0, 0.001 * dim_c[k]);
      }
    pos = pos + dim_c[k];
    }
    target += - 0.5 * dot_self(s_raw[node1] - s_raw[node2]);
  }

  // Prior Temporal random effect
  if(temporal_err == 1) {
     sigma_t[1] ~ normal(0, 2.5)T[0, ];
     for (i in 1:D) {
        sum(t_raw [i, ]) ~ normal(0, 0.001 * TP);
        target += - 0.5 * dot_self(t_raw[i,node1_t] - t_raw[i, node2_t]);
     }
  }

  // Priors additional parameters
  if(likelihood == 1) {//FB
    p[1] ~ beta(2, 2);
    w[1] ~ uniform(0.001, 0.999);
  }
  if(likelihood == 2) {// ZIB
    if(inflation != 1) {
      gamma_p0 ~ normal(0, 2.5);
      if(intercept == 1) {
        gamma0_p0[1] ~ normal(0, 2.5);
      }
    }
    if(inflation != 0) {
      gamma_p1 ~ normal(0, 2.5);
      if(intercept == 1) {
        gamma0_p1[1] ~ normal(0, 2.5);
      }
    }
  }

  // Likelihoods
  if(likelihood == 0) { // beta
    for (i in 1:M_is) {
      target += beta_lpdf(y[i] | a1[i], b1[i]);
    }
  }else if(likelihood==1) { // FB
    for (i in 1:M_is) {
      target += log_mix(p[1], beta_lpdf(y[i] | a2[i], b2[i]),
        beta_lpdf(y[i] | a1[i], b1[i]));
    }
  }else if(likelihood == 2) { // ZIB
    for (i in 1:M_is) {
      if(y[i] == 0 && inflation != 1) {
        target += log(p0[i]);
      }else if(y[i] == 1 && inflation != 0) {
        target += log(p1[i]);
      }else{
        target += beta_lpdf(y[i] | a1[i], b1[i]);
        if(inflation == 0) {
          target += log(1 - p0[i]);
        }else if(inflation == 1) {
          target += log(1 - p1[i]);
        }else if(inflation == 2) {
          target += log(1 - p0[i] - p1[i]);
        }
      }
    }
  }else if(likelihood == 3) {// ZIBalt
    for (i in 1:M_is) {
      if(y[i] == 0) {
        target += m_d[i] * log(1 - mu[i]);
      }else{
        target += log(1 - pow(1 - mu[i], m_d[i])) +
          beta_lpdf(y[i] | a1[i], b1[i]);
      }
    }
  }
}

generated quantities{
  vector[M_is] log_lik;
  vector[M_is] y_rep;
  real psi_OOS[1];
  real v_oos[1];
  int label_mixt;
  vector[inflation == 2 ? 3:0] probs;
  vector<lower=0,upper=1>[M_oos]  theta_oos;


  // different likelihoods
  if(likelihood == 0) {//beta
    for (i in 1:M_is) {
      log_lik[i] = beta_lpdf(y[i] | a1[i], b1[i]);
      y_rep[i] = beta_rng(a1[i], b1[i]);
    }
  // add
    for(i in 1:M_oos) { // OOS units
#include /inst/stan/include/gen_reff_OOS.stan
      if(intercept == 0) {
        theta_oos[i] = inv_logit(X_oos[i,] * beta + reffs_oos[i] + v_oos[1]);
      }else{
        theta_oos[i] = inv_logit(beta0[1] + X_oos[i, ] * beta + reffs_oos[i] + v_oos[1]);
      }
    }
  }else if(likelihood == 1){ //FB
#include /inst/stan/include/gen_FB.stan
  }else if(likelihood == 2) { //ZIB
#include /inst/stan/include/gen_ZIB.stan
  }else if(likelihood==3) {//FFT beta
#include /inst/stan/include/gen_ZIB_alt.stan
  }
}




