data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> y[N];
  int<lower=0> s[N];
}

parameters {
  real<lower=0> kappa_minus2;
  real<lower=0, upper=1> omega;
  real<lower=0, upper=1> theta[K];
}

transformed parameters {
  real kappa = kappa_minus2 + 2;
}

model {
  kappa_minus2 ~ gamma(0.01, 0.01);
  theta ~ beta(omega * (kappa - 2) + 1, (1 - omega) * (kappa - 2) + 1);
  y ~ bernoulli(theta[s]);
}
