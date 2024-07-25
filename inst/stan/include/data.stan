// Dummy variables
int<lower=0, upper=1> intercept; // 0: no; 1: yes
int<lower=0, upper=1> deff; // 0: variance; 1: deff
int<lower=0, upper=2> prior_reff; // 0: half-normal; 1: half-t; 2: VG
int<lower=0, upper=1> prior_coeff; // 0: normal; 1: HS
int<lower=0, upper=3> likelihood; // 0: beta; 1: FB; 2: Inflated, 3: FFT
int<lower=0, upper=2> inflation; // 0; 1; 2 (both); only used when likelihood=2
int<lower=0, upper=1> temporal_err;// 0: no, 1: yes
int<lower=0, upper=1> spatial_err;// 0: no, 1: yes
int<lower=0, upper=1> spatio_temporal;// 0: no, 1: yes


// Dimensions
int<lower=0> M_is; // number of available observations of the indicator
int<lower=0> M_oos; // number of unobserved values of the indicator
int<lower=0> D; // number of areas. Without temporal structure = M_is+M_oos
int<lower=0> P; // number of auxiliary variables
int<lower=0> TP; // number of time periods

// Observed data
vector<lower=0, upper=1>[M_is] y; // outcomes
matrix[M_is, P] X; // covariates for is data
matrix[M_oos,P] X_oos; // covariates for oos data
vector<lower=0>[M_is] disp; // dispersion parameter
vector<lower=0>[M_is] m_d; // area households

// Position indices
array[M_is] int<lower=0> indices_is; //indices units in sample
array[M_oos] int<lower=0> indices_oos; //indices units out of sample
array[D] int<lower=0> indices_spat; //indices ordering the spatial structure
array[M_is+M_oos,2] int<lower=0> indices_temp; //indexing time matrix with original obs vector

// Spatial structure
int<lower=0> N_edges; // number edges
int<lower=0> N_comp; // number disconnected components
array[N_comp] int<lower=0> dim_c; // components sizes
vector<lower=0>[D] scales_ICAR; // scaling factor graph
array[N_edges] int<lower=1, upper=D> node1;  // node1[i] adjacent to node2[i]
array[N_edges] int<lower=1, upper=D> node2;  // and node1[i] < node2[i]

// Temporal structure
array[TP-1] int<lower=1, upper=TP> node1_t; //temporal connections
array[TP-1] int<lower=1, upper=TP> node2_t; //temporal connections
real<lower=0> scale_factor_RW1; // scaling factor temporal graph
array[M_oos] int<lower=0> cat_ios; //Missingness kind for the temporal case

///// data for HS prior
real<lower=0> sigma_HS; // estimate sigma HS prior
real<lower=0> p0_HS;  // Expected number of large slopes
real<lower=0> slab_scale; // Scale for large slopes
real<lower=0> slab_df;// Effective degrees of freedom for large slopes

// Prior hyperparameters
real<lower=0> sigma_unstr;
real<lower=0> sigma_spatial;
real<lower=0> sigma_temporal;
real<lower=0> sigma_coeff;
