data {
  int<lower=0> N;
  int x[N];
  real y[N];
  real y_mean;
  real<lower=0> y_sd;
}

transformed data {
  real unif_lo = y_sd / 1000;
  real unif_hi = y_sd * 1000;
  real normal_sigma = y_sd * 100;
  real exp_lambda = 1.0 / 29.0;
}

parameters {
  real mu[2];
  real<lower=0> sigma[2];
  real<lower=0> nu_minus_one;
}

transformed parameters {
  real<lower=1> nu = nu_minus_one + 1;
}

model {
  mu ~ normal(y_mean, normal_sigma);
  sigma ~ uniform(unif_lo, unif_hi);
  nu_minus_one ~ exponential(exp_lambda);
  for (i in 1:N) {
    y[i] ~ student_t(nu, mu[x[i]], sigma[x[i]]);
  }
}
