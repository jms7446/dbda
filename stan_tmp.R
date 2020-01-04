library(rstan)
library(ggmcmc)
library(tidyverse)
library(bayestestR)

source("tools/stan_tools.R")

raw_data <- read_csv("data/z15N50.csv")
data <- list(
  y = raw_data$y, 
  N = length(raw_data$y)
)

inits = function() {
  resampled_y <- sample(data$y, replace = T)
  theta_init <- sum(resampled_y) / length(resampled_y)
  theta_init <- 0.001 + 0.998 * theta_init
  list(theta = theta_init)
}

stan_model <- stan_model(file = "tt.stan")
stan_fit <- sampling(
  stan_model, data = data, seed = 123, 
  chains = 3, iter = 1000, warmup = 200, thin = 1
)
stan_fit

S <- ggs(stan_fit)
ggmcmc(S, file = "plot/tt.stan.pdf")
ggs_histogram(S, greek = T)
ggs_separation(S, data$y)

ms <- rstan::extract(stan_fit)
quantile(ms$theta, probs = c(0.025, 0.975))
hdi(ms$theta, ci = 0.95)
hdi(stan_fit, ci = 0.95, )
ms

p <- ggs_histogram(S, greek = T)

df <- data.frame(replicate(4, rnorm(100)))
hdi(df)
hdi(df, ci = c(.80, .90, .95))

hdi(stan_fit)
quantile(ms$theta, probs = c(0.025, 0.5, 0.975))
