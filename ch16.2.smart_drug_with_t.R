library(tidyverse)
source("tools/stan_tools.R")
source("tools/common.R")

plot_file_name <- function(name) make_plot_file_name("ch16.2", name)

# make model
model <- stan_model(model_code = "
  data {
    int<lower=0> N;
    real Y[N];
    real Y_MEAN;
    real<lower=0> Y_SD;
  }
  
  transformed data {
    real UNIF_LO = Y_SD / 1000;
    real UNIF_HI = Y_SD * 1000;
    real NORMAL_SIGMA = Y_SD * 100;
    real EXP_LAMBDA = 1.0 / 29.0;
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
    nu_minus_one ~ exponential(EXP_LAMBDA);
    mu ~ normal(Y_MEAN, NORMAL_SIGMA);
    sigma ~ uniform(UNIF_LO, UNIF_HI);
    Y ~ student_t(nu, mu, sigma);
  }
  
  generated quantities {
    real log_nu = log10(nu);
    real effect_size = (mu - 100) / sigma;
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
    sigma = runif(n = 1, data$Y_SD / 1000, data$Y_SD * 1000), 
    nu_minus_one = runif(1, 0, 60)
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
  plot_post(res, "log_nu", title = "normality"), 
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


