data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> P;
  int x[N];
  matrix[N, P] cx;
  vector[N] y;
  real S_y;
  real M_y;
  vector[2] SIGMA_ALPHA_PARAMS;
}

parameters {
  real alpha_0;
  vector[K] alpha;
  vector[P] betac;
  real<lower=0> sigma;
  real<lower=0> sigma_alpha;
}

transformed parameters {
  real beta_0 = alpha_0 + mean(alpha);
  vector[K] beta = alpha - mean(alpha);
}

model {
  betac ~ normal(0, S_y * 2);
  sigma_alpha ~ gamma(SIGMA_ALPHA_PARAMS[1], SIGMA_ALPHA_PARAMS[2]);
  alpha ~ normal(0, sigma_alpha);
  alpha_0 ~ normal(M_y, S_y * 10);
  sigma ~ uniform(S_y / 100, S_y * 10);
  y ~ normal(alpha_0 + alpha[x] + cx * betac, sigma);
}
