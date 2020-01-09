
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

model {
  for (j in 1:NG) {
    omega[j] ~ beta(omega_o * (kappa_o - 2) + 1, (1 - omega_o) * (kappa_o - 2) + 1);
    kappa_minus2[j] ~ 
  }
  for (i in 1:NT) {
    real o = omega[G[i]];
    real k = kappa[G[i]];
    theta[i] ~ beta(o * (k- 2) + 1, (1 - o) * (k- 2) + 1);
    Z[i] ~ binomial(N[i], theta[i]);
  }
}