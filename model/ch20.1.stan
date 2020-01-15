data {
  int<lower=0> N;
  int<lower=0> K1;
  int<lower=0> K2;
  int x1[N];
  int x2[N];
  vector[N] y;
  real y_SD;
  real y_M;
  vector[2] a_gamma_SH_RA;
}

parameters {
  real<lower=0> alpha1_sigma;
  real<lower=0> alpha2_sigma;
  real<lower=0> alpha12_sigma;
  real alpha_0;
  vector[K1] alpha1;
  vector[K2] alpha2;
  matrix[K1, K2] alpha12;
  real<lower=0> y_sigma;
}

transformed parameters {
}

model {
  alpha12_sigma ~ gamma(a_gamma_SH_RA[1], a_gamma_SH_RA[2]);
  for (j1 in 1:K1) {
    for (j2 in 1:K2) {
      alpha12[j1, j2] ~ normal(0, alpha12_sigma);
    }
  }
  alpha2_sigma ~ gamma(a_gamma_SH_RA[1], a_gamma_SH_RA[2]);
  alpha2 ~ normal(0, alpha2_sigma);
  alpha1_sigma ~ gamma(a_gamma_SH_RA[1], a_gamma_SH_RA[2]);
  alpha1 ~ normal(0, alpha1_sigma);
  alpha_0 ~ normal(y_M, y_SD * 5);
  y_sigma ~ uniform(0, y_SD * 10);
  for (i in 1:N) {
    y[i] ~ normal(alpha_0 + alpha1[x1[i]] + alpha2[x2[i]] + alpha12[x1[i], x2[i]], y_sigma);
  }
}

generated quantities {
  real beta_0;
  matrix[K1, K2] beta12;
  matrix[K1, K2] m;
  vector[K1] beta1;
  vector[K2] beta2;
  for (j1 in 1:K1) { for (j2 in 1:K2) { m[j1, j2] = alpha_0 + alpha1[j1] + alpha2[j2] + alpha12[j1, j2]; } }
  beta_0 = mean(m);
  for (j1 in 1:K1) { beta1[j1] = mean(m[j1, 1:K2]) - beta_0; }
  for (j2 in 1:K2) { beta2[j2] = mean(m[1:K1, j2]) - beta_0; }
  for (j1 in 1:K1) { for (j2 in 1:K2) { beta12[j1, j2] = m[j1, j2] - (beta_0 + beta1[j1] + beta2[j2]); } }
}
