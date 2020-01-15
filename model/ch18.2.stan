data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] x;
  vector[N] y;
}

transformed data {
}

parameters {
  real alpha;
  vector[K] beta;
  real<lower=0> sigma;
  real<lower=0> nu_minus1;
}

transformed parameters {
  real nu = nu_minus1 + 1;
}

model {
  nu_minus1 ~ exponential(1 / 29.0);
  y ~ student_t(nu, x * beta + alpha, sigma);
}
