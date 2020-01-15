  data {
    int<lower=0> N;
    real Y[N];
    real Y_MEAN;
    real<lower=0> Y_SD;
  }
  
  parameters {
    real mu;
    real<lower=0> sigma;
  }
  
  model {
    mu ~ normal(Y_MEAN, 100 * Y_SD);
    sigma ~ uniform(Y_SD / 1000, Y_SD * 1000);
    Y ~ normal(mu, sigma);
  }
