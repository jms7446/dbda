data {
  int<lower=0> N;
  real Y[N];
  real Y_MEAN;
  real<lower=0> Y_SD;
}

transformed data {
  real UNIF_LO = Y_SD / 1000;
  real UNIF_HI = Y_SD * 1000;
  real NORMAL_SIGMA = Y_SD * 100;
  real EXP_LAMBDA = 1.0 / 29.0;
}

parameters {
  real mu;
  real<lower=0> sigma;
  real<lower=0> nu_minus_one;
}

transformed parameters {
  real<lower=1> nu = nu_minus_one + 1;
}

model {
  nu_minus_one ~ exponential(EXP_LAMBDA);
  mu ~ normal(Y_MEAN, NORMAL_SIGMA);
  sigma ~ uniform(UNIF_LO, UNIF_HI);
  Y ~ student_t(nu, mu, sigma);
}

generated quantities {
  real log_nu = log10(nu);
  real effect_size = (mu - 100) / sigma;
}
