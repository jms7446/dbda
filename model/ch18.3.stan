data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] x;
  vector[N] y;
}

transformed data {
}

parameters {
  real sigma_beta;
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=0> nu_minus1;
}

transformed parameters {
  real nu = nu_minus1 + 1;
}

model {
  sigma_beta ~ gamma(2.618,1.618);   // mode 1.0, sd 1.0;
  beta ~ student_t(2, 0, sigma_beta);
  nu_minus1 ~ exponential(1 / 29.0);
  y ~ student_t(nu, x * beta + alpha, sigma);
}
