  if(temporal_err == 0){
    if(prior_reff == 0) {// std normal
      v_oos[1] = normal_rng(0, sigma_v[1]);
    }else if(prior_reff == 1) {// half t (3 dof)
      v_oos[1] = student_t_rng(nu[1], 0, sigma_v[1]);
    }else if(prior_reff == 2) {//VG
      psi_OOS[1] = gamma_rng(0.5, 1);
      v_oos[1] = normal_rng(0, psi_OOS[1]/lambda[1]);
    }
  }else{
      v_oos[1] = 0;
    }
