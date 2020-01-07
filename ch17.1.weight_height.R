library(tidyverse)
source("tools/stan_tools.R")
source("tools/common.R")

plot_file_name <- function(name) make_plot_file_name("ch17.1", name)

model <- stan_model(model_code = "
  data {
    int<lower=0> N;
    real<lower=0> x[N];
    real<lower=0> y[N];
    real mean_x;
    real sd_x;
    real mean_y;
    real sd_y;
  }
  
  transformed data {
    real HALF_nu = 30;
    real L_sigma = sd_y / 1000;
    real H_sigma = sd_y * 1000;
    real S_a1 = 10 * fabs(sd_y / sd_x);
    real S_a2 = 10 * fabs(mean_x * sd_y / sd_x);
  }
  
  parameters {
    real<lower=0> nu_minus_one;
    real a[2];
    real<lower=0> sigma;
  }
  
  transformed parameters {
    real nu = nu_minus_one + 1;
  }
  
  model {
    nu_minus_one ~ exponential(1.0 / (HALF_nu - 1));
    a[1] ~ normal(0, S_a1);
    a[2] ~ normal(0, S_a2);
    sigma ~ uniform(L_sigma, H_sigma);
    for (i in 1:N) {
      real mu = a[1] + x[i] * a[2];
      y[i] ~ student_t(nu, mu, sigma);
    }
  }
  
  generated quantities {
    real log_nu = log10(nu);
  }
")

raw_data <- read_csv("data/HtWtData300.csv")
data <- list(
  N = nrow(raw_data), 
  x = raw_data$height, 
  y = raw_data$weight, 
  mean_x = mean(raw_data$height), 
  sd_x = sd(raw_data$height), 
  mean_y = mean(raw_data$weight), 
  sd_y = sd(raw_data$weight)
)

fit <- sampling(model, data = data, seed = 123, 
                pars = (c("nu_minus_one")), include = F, 
                chains = 4, warmup = 300, iter = 5000, thin = 1)
fit
ggmcmc(ggs(fit), file = plot_file_name("diag"))

# check result
res <- fit_to_tibble(fit)

# plot posteria predictive
line_grid <- res %>% sample_n(20) %>% 
  make_line_grid(xs = seq(40, 80, by = 5), y_func = a_1 + a_2 * height, x_name = "height", y_name = "weight")
dist_grid <- line_grid %>% 
  make_tdist_grid(height, weight, sigma, nu = nu)
ggplot(raw_data, aes(height, weight)) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = line_grid, aes(group = line_id), color = "skyblue", alpha = 0.7) + 
  geom_path(data = dist_grid, aes(XX, YY, group = point_id), color = "skyblue", alpha = 0.5)
ggsave(file = plot_file_name("post_predictive"))


# plot posteria
grid.arrange(
  plot_post(res, "a_1"), 
  plot_post(res, "a_2", rope = c(-0.5, 0.5)), 
  plot_post(res, "sigma"), 
  plot_post(res, "log_nu")
)
ggsave(file = plot_file_name("posteria"))

