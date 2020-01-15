data {
  int<lower=0> N_total;
  int<lower=0> N_subj;
  int y[N_total];
  int s[N_total];
}

parameters {
  real<lower=0,upper=1> theta[N_subj];
}

model {
  for (i in 1:N_total) {
    y[i] ~ bernoulli(theta[s[i]]);
  }
  for (j in 1:N_subj) {
    theta[j] ~ beta(2, 2);
  }
}
