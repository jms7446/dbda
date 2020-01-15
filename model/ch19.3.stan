data {
  int<lower=0> N;
  int<lower=0> K;
  int x[N];
  vector[N] y;
  real S_y;
  real M_y;
}

parameters {
  real alpha_0;
  vector[K] alpha;
  vector<lower=0>[K] sigma;
  real<lower=0> alpha_sigma;
  real sigma_shape;
  real sigma_rate;
  real<lower=0> nu_minus1;
}

transformed parameters {
  real beta_0 = alpha_0 + mean(alpha);
  vector[K] beta = alpha - mean(alpha);
  real nu = nu_minus1 + 1;
}

model {
  sigma_shape ~ uniform(0.01, 100);
  sigma_rate ~ uniform(0.01, 100);
  alpha ~ normal(0, alpha_sigma);
  alpha_0 ~ normal(M_y, S_y * 10);
  sigma ~ gamma(sigma_shape, sigma_rate);
  nu_minus1 ~ exponential(1.0/29);
  y ~ student_t(nu, alpha_0 + alpha[x], sigma[x]);
}
