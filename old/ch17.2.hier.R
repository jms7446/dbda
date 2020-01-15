library(tidyverse)
source("tools/stan_tools.R")
source("tools/common.R")

plot_file_name <- function(name) make_plot_file_name("ch17.2", name)

model <- stan_model(model_code = "
  data {
    int<lower=0> N;
    int<lower=0> N_subj;
    real<lower=0> x[N];
    real<lower=0> y[N];
    int s[N];
    real mean_x;
    real sd_x;
    real mean_y;
    real sd_y;
  }
  
  transformed data {
    real HALF_nu = 30;
    real L_sigma = sd_y / 1000;
    real H_sigma = sd_y * 1000;
    real S_a = 10 * fabs(sd_y / sd_x);
    real S_b = 10 * fabs(mean_x * sd_y / sd_x);
  }
  
  parameters {
    real<lower=0> nu_minus_one;
    real m_a;
    real m_b;
    real s_a;
    real s_b;
    real a[N_subj];
    real b[N_subj];
    real<lower=0> sigma;
  }
  
  transformed parameters {
    real nu = nu_minus_one + 1;
  }
  
  model {
    m_a ~ normal(0, S_a);
    m_b ~ normal(0, S_b);
    s_a ~ uniform(L_sigma, H_sigma);
    s_b ~ uniform(L_sigma, H_sigma);
    a ~ normal(m_a, s_a);
    b ~ normal(m_b, s_b);
    sigma ~ uniform(L_sigma, H_sigma);
    nu_minus_one ~ exponential(1.0 / (HALF_nu - 1));
    for (i in 1:N) {
      real mu = b[s[i]] + x[i] * a[s[i]];
      y[i] ~ student_t(nu, mu, sigma);
    }
  }
  
  generated quantities {
    real log_nu = log10(nu);
  }
")

raw_data <- read_csv("data/HierLinRegressData.csv") %>% 
  mutate(s = as.integer(Subj))
data <- list(
  N = nrow(raw_data), 
  N_subj = n_distinct(raw_data$s), 
  x = raw_data$X, 
  y = raw_data$Y, 
  s = raw_data$s, 
  mean_x = mean(raw_data$X), 
  sd_x = sd(raw_data$X), 
  mean_y = mean(raw_data$Y), 
  sd_y = sd(raw_data$Y)
)
init <- function() list(m_a = 3, m_b = -100)
fit <- sampling(model, data = data, init = init, seed = 123, 
                chains = 4, warmup = 1000, iter = 50000, thin = 4, 
                control = list(max_treedepth = 15))
ggs_traceplot(ggs(fit, family = "a\\[[134]\\]", inc_warmup = T))
fit  
res <- fit_to_tibble(fit)
head(res)

# total regression by lm
xs <- seq(40, 100, length = 10)

line_grid <- res %>% 
  sample_n(20) %>% 
  make_line_grid(xs, y_func = m_a * X + m_b)
ggplot(raw_data, aes(X, Y)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  geom_line(data = line_grid, aes(group = line_id), color = "skyblue", alpha = 0.5)
ggsave(file = plot_file_name("total_regression"))

# regression by group
grid <- res %>% sample_n(20) %>% 
  multi_gather(matches("(a|b)_.*"), col_name = "s") %>% 
  make_line_grid(xs, y_func = a * X + b) 
ggplot(raw_data, aes(X, Y)) + 
  geom_point() + 
  geom_line(data = grid, aes(group = line_id), color = "skyblue", alpha=0.5) + 
  coord_cartesian(xlim = c(40, 100), ylim = c(50, 250)) + 
  facet_wrap(~ s, ncol = 5)
ggsave(file = plot_file_name("group_wise_regression"))
