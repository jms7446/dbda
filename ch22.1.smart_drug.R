
library(tidyverse)
source("tools/stan_tools.R")
file_prefix = "ch22.1"

# cached data
image_file = paste0(file_prefix, ".RData")
load(image_file)

# data load
raw_data <- read_csv("data/TwoGroupIQ.csv")
raw_data
raw_data %>% count(Group)
raw_data <- raw_data %>% filter(Group == "Smart Drug")
raw_data %>% count(Group)

# make model
model_code <- "
  data {
    int<lower=0> N;
    real y[N];
    real y_mean;
    real<lower=0> y_sd;
  }
  
  parameters {
    real mu;
    real<lower=0> sigma;
  }
  
  model {
    mu ~ normal(y_mean, 100 * y_sd);
    sigma ~ uniform(y_sd / 1000, y_sd * 1000);
    y ~ normal(mu, sigma);
  }
"
model <- stan_model(model_code = model_code)
y <- raw_data$Score
data <- list(
  N = length(y), 
  y = y, 
  y_mean = mean(y), 
  y_sd = sd(y)
)
init <- function() {
  list(
    mu = rnorm(1, mean(y), sd(y) * 2), 
    sigma = runif(n = 1, sd(y) / 1000, sd(y) * 1000)
  )
}
fit <- sampling(model, data = data, init = init, seed = 1234, 
                chains = 4, warmup = 200, iter = 6000, thin = 1)
res <- as_tibble(as.data.frame(fit))
S <- ggs(fit)


# Caching results
#save.image(image_file)

# dignosis
ggs_traceplot(ggs(fit, burnin = T) %>% filter(Iteration < 200))
diag_file = paste0(file_prefix, "_diag.pdf")
ggmcmc(ggs(fit), file = diag_file)
fit

# posteria
res <- res %>% mutate(effect_size = (mu - 100) / sigma)
hdi(res, ci = 0.95)
norm_density <- function(values, idx) {
  dnorm(values, res$mu[idx], res$sigma[idx])
}

grid.arrange(
  plot_post(res, "mu", rope = c(99, 101), title = "Mean"), 
  plot_post(res, "sigma", title = "Std."), 
  plot_post(res, "effect_size", rope = c(-0.1, 0.1), title = "Effect Size"), 
  plot_post_predictive_with_histogram(raw_data$Score, norm_density, num_lines = 15, 
                                      title = "Data w. Post. Pred.")
)



