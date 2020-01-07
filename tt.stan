  data {
    int N;
    real Y[N];
    real X[N];
    real W[N];
    int G[N]; 
    int NG;
  }
  
  parameters {
    real m_a0;
    real m_a1;
    real m_a2;
    real <lower=0> s_a0;
    real <lower=0> s_a1;
    real <lower=0> s_a2;
    real a0[NG];
    real a1[NG];
    real a2[NG];
    real<lower=0> sigma;
    real<lower=0> nu_minus_one;
  }
  
  transformed parameters {
    real nu = nu_minus_one + 1;
  }
  
  model {
    a0 ~ normal(m_a0, s_a0);
    a1 ~ normal(m_a1, s_a1);
    a2 ~ normal(m_a2, s_a2);
    nu_minus_one ~ exponential(1.0 / 29.0);
    for (i in 1:N) {
      int j = G[i];
      real mu = a0[j] + a1[j] * X[i] + a2[j] * (X[i] ^ 2);
      Y[i] ~ normal(mu, W[i] * sigma);
    }
  }