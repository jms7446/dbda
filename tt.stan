  data {
    int<lower=0> N;
    real y[N];
    real y_mean;
    real<lower=0> y_sd;
  }
  
  parameters {
    real mu;
    real<lower=0> sigma;
    real<lower=0> nu_minus_one;
  }
  
  transformed parameters {
    real<lower=1> log_nu = log10(nu_minus_one + 1);
  }
  
  model {
    nu_minus_one ~ exponential(1.0 / 29.0);
    mu ~ normal(y_mean, 100 * y_sd);
    sigma ~ uniform(y_sd / 1000, y_sd * 1000);
    y ~ student_t(nu, mu, sigma);
  }
  