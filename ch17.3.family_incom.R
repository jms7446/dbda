library(tidyverse)
source("tools/stan_tools.R")
source("tools/common.R")

set_stan_parallel()
plot_file_name <- function(name) make_plot_file_name("ch17.3", name)


################################################################################
# build model
################################################################################

model <- stan_model(model_code = "
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
")


################################################################################
# load data and model fit
################################################################################

raw_data <- read_csv("data/IncomeFamszState3yr.csv", comment = "#") %>% 
  mutate(State = factor(State)) %>% 
  mutate(state_id = as.integer(State)) 
  #filter(state_id <= 10)

# for inverse mapping
state_map <- raw_data %>% select(State, state_id) %>% distinct()

data <- list(
  N = nrow(raw_data), 
  Y = raw_data$MedianIncome, 
  X = raw_data$FamilySize, 
  W = raw_data$SampErr, 
  G = raw_data$state_id, 
  NG = n_distinct(raw_data$State) 
)

init <- function() {
  list(
    m_a0 = 40000, 
    m_a1 = 20000, 
    m_a2 = -2000
  )
}

fit <- sampling(model, data = data, seed = 123, init = init, 
                chains = 4, warmup = 1000, iter = 11000, thin = 2)
fit


################################################################################
# diagnosis
################################################################################

ggs_traceplot(ggs(fit, family = "m_a", inc_warmup = F))
# ggmcmc(ggs(fit), file = plot_file_name("diag")) # skip, because of file size.


################################################################################
# check result
################################################################################

res <- fit_to_tibble(fit)

# posteria
plot_post(res, "m_a2", title = "Is quadratic adequate?")
ggsave(make_plot_file_name("m_a2_posteria"))

plot_post(res, "nu")
quantile(res$nu)

# total plot with m_a0, m_a1, m_a2
res %>% 
  make_sample_grid(c(1, 7), x_name = "FamilySize", num_line = 50) %>% 
  mutate(MedianIncome = m_a0 + m_a1 * FamilySize + m_a2 * FamilySize ^ 2) %>% 
  ggplot(aes(FamilySize, MedianIncome)) +
    geom_line(aes(group = id), color = "skyblue", alpha = 0.5) + 
    geom_point(data = raw_data, aes(size = 1.0 / SampErr), alpha = 0.3) + 
    geom_line(data = raw_data, aes(group = state_id), color = "grey50", alpha = 0.3) + 
    geom_smooth(data = raw_data, method = "lm", formula = y ~ poly(x, 2), color = "red") + 
    labs(size = "1/SampErr")
ggsave(plot_file_name("total_predictive"))

# state-wise plot with a0[.], a1[.], a2[.]
plot_state_wise_predictive <- function(state_ids) {
  res %>% 
    make_sample_grid(c(1, 7), x_name = "FamilySize") %>% 
    multi_gather(starts_with("a"), col_name = "state_id") %>% 
    mutate(MedianIncome = a0 + a1 * FamilySize + a2 * FamilySize ^ 2) %>% 
    filter(state_id %in% state_ids) %>% 
    left_join(state_map, by = "state_id") %>% 
    ggplot(aes(FamilySize, MedianIncome)) + 
      geom_line(aes(group = id), color = "skyblue", alpha = 0.5) +
      geom_point(data = filter(raw_data, state_id %in% state_ids), aes(size = 1.0/SampErr)) +
      facet_wrap(~State, ncol = 5) + 
      labs(size = "1/SampErr")
}

plot_state_wise_predictive(seq(1, 30, by = 1))
ggsave(plot_file_name("state_wise_predictive_1"))

plot_state_wise_predictive(seq(31, 60, by = 1))
ggsave(plot_file_name("state_wise_predictive_2"))
