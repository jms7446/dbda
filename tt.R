
library(rstan)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stan_dso <- stan_model(file = "tt.stan")

N <- 50
z <- 10
y <- c(rep(1, z), rep(0, N - z))
data <- list(
  y = y,
  N = N
)

stan_fit <- sampling(object = stan_dso, data = data, 
                     chain = 3, iter = 1000, warmup = 200, thin = 1)

stan_fit
          