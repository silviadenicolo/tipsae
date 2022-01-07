for (i in 1:M_is) {
  log_lik[i] = log_mix(p[1], beta_lpdf(y[i] | a2[i], b2[i]),
      beta_lpdf(y[i] | a1[i], b1[i]));
  label_mixt = bernoulli_rng(p[1]);
  if(label_mixt == 1){
    y_rep[i] = beta_rng(a2[i], b2[i]);
  }else{
    y_rep[i] = beta_rng(a1[i], b1[i]);
  }
}
