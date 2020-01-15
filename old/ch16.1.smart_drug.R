library(tidyverse)
source("tools/stan_tools.R")
source("tools/common.R")

plot_file_name <- function(name) make_plot_file_name("ch16.1", name)

# make model
model <- stan_model(model_code = "
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
")
raw_data <- read_csv("data/TwoGroupIQ.csv")
data <- list(
  N = nrow(raw_data), 
  Y = raw_data$Score, 
  Y_MEAN = mean(raw_data$Score), 
  Y_SD = sd(raw_data$Score)
)
init <- function() {
  list(
    mu = rnorm(1, data$Y_MEAN, data$Y_SD * 2), 
    sigma = runif(n = 1, data$Y_SD / 1000, data$Y_SD * 1000)
  )
}
fit <- sampling(model, data = data, init = init, seed = 1234, 
                chains = 4, warmup = 200, iter = 6000, thin = 1)
fit
ggmcmc(ggs(fit), file = plot_file_name("diag"))

# check result
res <- fit_to_tibble(fit)

# posteria
res <- res %>% 
  mutate(effect_size = (mu - 100) / sigma)

p <- grid.arrange(
  plot_post(res, "mu", rope = c(99, 101), title = "Mean"), 
  plot_post(res, "sigma", title = "Std."), 
  plot_post(res, "effect_size", rope = c(-0.1, 0.1), title = "Effect Size"), 
  res %>% sample_n(20) %>% 
    make_line_grid(xs = seq(40, 250, length = 200), y_func = dnorm(Score, mu, sigma), x_name = "Score") %>% 
    ggplot(aes(Score)) + 
      geom_histogram(data = raw_data, bins = 30, aes(y = ..density..), color = "white", alpha = 0.5) + 
      geom_line(aes(y = Y, group = line_id), color = "skyblue", alpha = 0.5) + 
      labs(title = "Post.Predictive") + 
      theme_post()
); p
ggsave(plot_file_name("posteria_check"), p)
