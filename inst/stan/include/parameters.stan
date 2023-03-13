// Fixed part
vector[intercept == 1 ? 1:0] beta0;//intercept
vector[P] z_beta;// std regression coefficients
  // HS prior
  vector<lower=0>[prior_coeff == 1 ? P:0] lambda_HS;
  vector<lower = 0>[prior_coeff == 1 ? 1:0] z_c2;
  vector<lower = 0>[prior_coeff == 1 ? 1:0] z_tau;

// Unstructured reff
vector[length_v] v_raw; //raw area-specific random effect
    // prior_reff=0 and prior_reff=1
    vector<lower=0>[((prior_reff == 0 || prior_reff==1) && spatio_temporal == 0) ? 1:0] sigma_v;
    // prior_reff=1
    vector<lower=0>[(prior_reff == 1 && spatio_temporal == 0) ? 1:0] nu;
    // prior_reff=2 (VG)
    vector<lower=0>[(prior_reff == 2 && spatio_temporal == 0) ? length_v:0] psi_d;
    vector<lower=0>[(prior_reff == 2 && spatio_temporal == 0) ? 1:0] lambda;

// Spatial reff
vector[spatial_err==1 ? D:0] s_raw; // raw spatial reff
vector<lower=0>[spatial_err==1 ? 1:0] sigma_s; // scale hyperparemter
// Temporal reff
matrix[temporal_err==1 ? D:0, temporal_err==1 ? TP:0]  t_raw; // raw temporal reff
vector<lower=0>[temporal_err==1 ? 1:0] sigma_t; // scale hyperparemter

// likelihood-specific parameters
//likelihood=1
vector<lower=0.001,upper=0.999>[likelihood==1 ? 1:0] w;
vector<lower=0.001,upper=0.999>[likelihood==1 ? 1:0] p;
//likelihood=2
vector[(likelihood==2 && inflation != 1) ? P:0] gamma_p0;// 0-infl coeff
vector[(likelihood==2 && inflation != 0) ? P:0] gamma_p1;// 1-infl coeff
vector[(likelihood==2 && inflation != 1 && intercept==1) ? 1:0] gamma0_p0;// 0-infl intercept
vector[(likelihood==2 && inflation != 0 && intercept==1) ? 1:0] gamma0_p1;// 1-infl intercept
