library(tidyverse)
source("tools/stan_tools.R")
file_prefix = "ch22.2"
#load(paste0(file_prefix, ".RData"))

raw_data <- read_csv("data/TwoGroupIQ.csv") %>% 
  filter(Group == "Smart Drug")

model_code = "
  data {
    int<lower=0> N;
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
    real mu;
    real<lower=0> sigma;
    real<lower=0> nu_minus_one;
  }
  
  transformed parameters {
    real<lower=1> nu = nu_minus_one + 1;
  }
  
  model {
    nu_minus_one ~ exponential(exp_lambda);
    mu ~ normal(y_mean, normal_sigma);
    sigma ~ uniform(unif_lo, unif_hi);
    y ~ student_t(nu, mu, sigma);
  }
  
  generated quantities {
    real log_nu = log10(nu);
    real effect_size = (mu - 100) / sigma;
  }
"
model <- stan_model(model_code = model_code)
values <- raw_data$Score
data <- list(
  N = length(values), 
  y = values, 
  y_mean = mean(values), 
  y_sd = sd(values)
)
init <- function() {
  list(
    mu = rnorm(1, mean(values), sd(values) * 2), 
    sigma = runif(n = 1, sd(values) / 1000, sd(values) * 1000), 
    nu_minus_one = runif(1, 0, 60)
  )
}
fit <- sampling(model, data = data, init = init, seed = 1234, 
                chains = 4, warmup = 200, iter = 5400, thin = 1)
#save.image(paste0(file_prefix, ".RData"))

# diagnosis
fit
ggs_traceplot(ggs(fit, inc_warmup = T) %>% filter(Iteration < 500))
ggmcmc(ggs(fit), file = paste0(file_prefix, "_diag.pdf"))

# check posteria
S <- ggs(fit)
res <- as_tibble(as.data.frame(fit))

hdi(res, ci = 0.95)
ggs_pairs(S %>% filter(Parameter %in% c("mu", "sigma", "log_nu")))
make_density <- function(values, idx) {
  normed_mu = (values - res$mu[idx]) / res$sigma[idx]
  dt(normed_mu, df = res$nu[idx]) / res$sigma[idx]
}
grid.arrange(
  plot_post(res, "mu", rope = c(99, 101), title = "Mean"), 
  plot_post(res, "sigma", rope = c(14, 16), title = "Scale"), 
  plot_post(res, "log_nu", title = "Normality"), 
  plot_post_predictive_with_histogram(raw_data$Score, make_density, title = "Predictive"), 
  plot_post(res, "effect_size", rope = c(-0.1, 0.1), title = "Effect Size")
)
