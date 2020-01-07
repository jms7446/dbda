library(tidyverse)
source("tools/stan_tools.R")
file_prefix = "ch22.3"
#load(paste0(file_prefix, ".RData"))

RopeMuDiff=c(-0.5,0.5) ; RopeSdDiff=c(-0.5,0.5) ; RopeEff=c(-0.1,0.1)

raw_data <- read_csv("data/TwoGroupIQ.csv") %>% 
  mutate(x = as.integer(factor(Group)))

model_code = "
  data {
    int<lower=0> N;
    int x[N];
    real y[N];
    real y_mean;
    real<lower=0> y_sd;
  }
  
  transformed data {
    real unif_lo = y_sd / 1000;
    real unif_hi = y_sd * 1000;
    real normal_sigma = y_sd * 100;
    real exp_lambda = 1.0 / 29.0;
  }
  
  parameters {
    real mu[2];
    real<lower=0> sigma[2];
    real<lower=0> nu_minus_one;
  }
  
  transformed parameters {
    real<lower=1> nu = nu_minus_one + 1;
  }
  
  model {
    mu ~ normal(y_mean, normal_sigma);
    sigma ~ uniform(unif_lo, unif_hi);
    nu_minus_one ~ exponential(exp_lambda);
    for (i in 1:N) {
      y[i] ~ student_t(nu, mu[x[i]], sigma[x[i]]);
    }
  }
"
model <- stan_model(model_code = model_code)
values <- raw_data$Score
data <- list(
  N = length(values), 
  x = raw_data$x, 
  y = values, 
  y_mean = mean(values), 
  y_sd = sd(values)
)
init <- function() {
  list(
    mu = rnorm(2, mean(values), sd(values) * 2), 
    sigma = runif(n = 2, sd(values) / 1000, sd(values) * 1000), 
    nu_minus_one = runif(1, 0, 60)
  )
}
fit <- sampling(model, data = data, init = init, seed = 1234, 
                chains = 4, warmup = 200, iter = 5400, thin = 1)

# diagnosis
fit
ggs_traceplot(ggs(fit, inc_warmup = T) %>% filter(Iteration < 500))
ggmcmc(ggs(fit), file = paste0(file_prefix, "_diag.pdf"))

# check posteria
S <- ggs(fit)
res <- as_tibble(as.data.frame(fit)) %>% 
  mutate(
    diff = `mu[2]` - `mu[1]`, 
    sigma_diff = `sigma[2]` - `sigma[1]`, 
    effect_size = (`mu[2]` - `mu[1]`) / sqrt((`sigma[1]` ^ 2 + `sigma[2]` ^ 2) / 2)
  )

hdi(res, ci = 0.95)
ggs_pairs(S %>% filter(Parameter %in% c("mu", "sigma", "log_nu")))

make_density <- function(values, idx, mu, sigma, nu) {
  normed_mu = (values - mu[idx]) / sigma[idx]
  dt(normed_mu, df = nu[idx]) / sigma[idx]
}
make_density1 <- function(values, idx) { make_density(values, idx, res$`mu[1]`, res$`sigma[1]`, res$nu) }
make_density2 <- function(values, idx) { make_density(values, idx, res$`mu[2]`, res$`sigma[2]`, res$nu) }
grid.arrange(
  #plot_post(res, "mu[1]", title = "Placebo Mean"), 
  #plot_post(res, "mu[2]", title = "Smart Drug Mean"), 
  #plot_post(res, "sigma", rope = c(14, 16), title = "Scale"), 
  #plot_post(res, "log_nu", title = "Normality"), 
  plot_post_predictive_with_histogram(
    filter(raw_data, x == 1)$Score, make_density1, sample_len = dim(res)[1], title = "Predictive1"), 
  plot_post_predictive_with_histogram(
    filter(raw_data, x == 2)$Score, make_density2, sample_len = dim(res)[1], title = "Predictive2"), 
  plot_post(res, "sigma_diff", rope = RopeSdDiff, title = "Difference of Sd."), 
  plot_post(res, "effect_size", rope = RopeEff, title = "Effect Size"),
  plot_post(res, "diff", rope = RopeMuDiff, title = "Difference of Means")
)

# t.test
t.test(Score ~ Group, data = raw_data)
