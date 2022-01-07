if (deff==0) {
      b1[i] = (1 - mu[i]) * (mu[i] * (1 - mu[i]) / disp[i] - 1);
      a1[i] = mu[i] * (mu[i] * (1 - mu[i]) / disp[i] - 1);
    } else {
      b1[i] = (1 - mu[i]) * disp[i];
      a1[i] = mu[i] * disp[i];
  }


