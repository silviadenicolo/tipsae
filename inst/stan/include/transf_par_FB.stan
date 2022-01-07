for(i in 1:M_is) {
  if(intercept == 0){
    lambda2[i] = inv_logit(X[i, ] * beta + reffs[i]);
  }else{
    lambda2[i] = inv_logit(beta0[1] + X[i, ] * beta + reffs[i]);
  }
  lambda1[i] = lambda2[i] + w[1] * fmin((1 - lambda2[i])/p[1], sqrt(disp[i] / (p[1] * (1 - p[1]))));
  mu[i] = p[1] * lambda1[i] + (1 - p[1]) * lambda2[i];
  theta[i] = mu[i];
  phi[i] = (mu[i] * (1 - mu[i]) - disp[i]) /
    (disp[i] - p[1] * (1 - p[1]) * w[1] ^ 2 *
    fmin((1 - lambda2[i])/p[1], sqrt(disp[i] / (p[1] * (1 - p[1])))) ^ 2);
  b1[i] = (1 - lambda2[i]) * (phi[i]);
  a1[i] = lambda2[i] * (phi[i]);
  b2[i] = (1 - lambda1[i]) * (phi[i]);
  a2[i] = lambda1[i] * (phi[i]);
}

