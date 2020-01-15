data {
  int<lower=0> N;
  int<lower=0> N_subj;
  real<lower=0> x[N];
  real<lower=0> y[N];
  int s[N];
  real mean_x;
  real sd_x;
  real mean_y;
  real sd_y;
}

transformed data {
  real HALF_nu = 30;
  real L_sigma = sd_y / 1000;
  real H_sigma = sd_y * 1000;
  real S_a = 10 * fabs(sd_y / sd_x);
  real S_b = 10 * fabs(mean_x * sd_y / sd_x);
}

parameters {
  real<lower=0> nu_minus_one;
  real m_a;
  real m_b;
  real s_a;
  real s_b;
  real a[N_subj];
  real b[N_subj];
  real<lower=0> sigma;
}

transformed parameters {
  real nu = nu_minus_one + 1;
}

model {
  m_a ~ normal(0, S_a);
  m_b ~ normal(0, S_b);
  s_a ~ uniform(L_sigma, H_sigma);
  s_b ~ uniform(L_sigma, H_sigma);
  a ~ normal(m_a, s_a);
  b ~ normal(m_b, s_b);
  sigma ~ uniform(L_sigma, H_sigma);
  nu_minus_one ~ exponential(1.0 / (HALF_nu - 1));
  for (i in 1:N) {
    real mu = b[s[i]] + x[i] * a[s[i]];
    y[i] ~ student_t(nu, mu, sigma);
  }
}

generated quantities {
  real log_nu = log10(nu);
}
