//fixed effects
vector[P] beta;
  // HS
  vector<lower=0>[prior_coeff==1 ? 1:0] tau;
  vector<lower=0>[prior_coeff==1 ? 1:0] c2;
  vector<lower=0>[prior_coeff==1 ? P:0] lambda_tilde;

//predictors
vector<lower=0,upper=1>[M_is]  theta;
//vector<lower=0,upper=1>[M_oos]  theta_oos;

// Beta parameters
vector<lower=0>[M_is] b1;
vector<lower=0>[M_is] a1;
vector<lower=0,upper=1>[M_is]  mu;

// likelihood=1
vector<lower=0>[likelihood==1 ? M_is:0] phi;
vector<lower=0,upper=1>[likelihood==1 ? M_is:0]  lambda1;
vector<lower=0,upper=1>[likelihood==1 ? M_is:0]  lambda2;
vector<lower=0>[likelihood==1 ? M_is:0] b2;
vector<lower=0>[likelihood==1 ? M_is:0] a2;

// likelihood=2 (and 3)
vector<lower=0,upper=1>[((likelihood==2 && inflation != 1) || likelihood==3) ? M_is:0] p0;
vector<lower=0,upper=1>[((likelihood==2 && inflation != 0) || likelihood==3) ? M_is:0] p1;

// likelihood=3
vector[likelihood==3 ? 1:0] min_lambda_EB;
vector[likelihood==3 ? 1:0] lambda_EB;
vector[likelihood==3 ? 2:0] which_min_EB;

// Random effects
vector[length_v] v; // Unstructured random effects
vector[spatial_err==1 ? D:0]  s; // Spatial random effects
matrix[temporal_err==1 ? D:0, temporal_err==1 ? TP:0]  t; // Temporal random effects
  // Cumulative random part
vector[M_is] reffs; // sampled areas
 // vector[(spatial_err==1 || temporal_err==1) ? M_oos:0]  reffs_oos; // unsampled ares
vector[M_oos]  reffs_oos; // unsampled ares


