data {
  int<lower=0> NT;
  int<lower=0> NG;
  int<lower=0> Z[NT];
  int<lower=0> N[NT];
  int<lower=0> G[NT];
}

parameters {
  real<lower=0, upper=1> omega_o;
  real<lower=2> kappa_o;
  
  real<lower=0, upper=1> omega[NG];
  real<lower=0> kappa_minus2[NG];
  
  real<lower=0, upper=1> theta[NT];
}

transformed parameters {
  real kappa[NG];
  for (j in 1:NG) {
    kappa[j] = kappa_minus2[j] + 2; 
  }
}

model {
  omega_o ~ beta(1, 1);
  kappa_o ~ gamma(0.01, 0.01);
  
  omega ~ beta(omega_o * (kappa_o - 2) + 1, (1 - omega_o) * (kappa_o - 2) + 1);
  kappa_minus2 ~ gamma(0.01, 0.01);
  for (i in 1:NT) {
    real o = omega[G[i]];
    real k = kappa[G[i]];
    theta[i] ~ beta(o * (k- 2) + 1, (1 - o) * (k- 2) + 1);
    Z[i] ~ binomial(N[i], theta[i]);
  }
}
