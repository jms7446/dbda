data {
  int<lower=0> N;
  real<lower=0> x[N];
  real<lower=0> y[N];
  real mean_x;
  real sd_x;
  real mean_y;
  real sd_y;
}

transformed data {
  real HALF_nu = 30;
  real L_sigma = sd_y / 1000;
  real H_sigma = sd_y * 1000;
  real S_a1 = 10 * fabs(sd_y / sd_x);
  real S_a2 = 10 * fabs(mean_x * sd_y / sd_x);
}

parameters {
  real<lower=0> nu_minus_one;
  real a[2];
  real<lower=0> sigma;
}

transformed parameters {
  real nu = nu_minus_one + 1;
}

model {
  nu_minus_one ~ exponential(1.0 / (HALF_nu - 1));
  a[1] ~ normal(0, S_a1);
  a[2] ~ normal(0, S_a2);
  sigma ~ uniform(L_sigma, H_sigma);
  for (i in 1:N) {
    real mu = a[1] + x[i] * a[2];
    y[i] ~ student_t(nu, mu, sigma);
  }
}

generated quantities {
  real log_nu = log10(nu);
}
