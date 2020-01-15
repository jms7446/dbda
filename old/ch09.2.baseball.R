library(rlist)
source("tools/stan_tools.R")
source("tools/common.R")
source("tools/my_plots.R")

plot_file_name <- function(name) make_plot_file_name("ch09.2", name)


################################################################################
# modeling
################################################################################

model <- stan_model(model_code = "
data {
  int<lower=0> NT;
  int<lower=0> NG;
  int<lower=0> Z[NT];
  int<lower=0> N[NT];
  int<lower=0> G[NT];
}

parameters {
  real<lower=0, upper=1> omega_o;
  real<lower=2> kappa_o;
  
  real<lower=0, upper=1> omega[NG];
  real<lower=0> kappa_minus2[NG];
  
  real<lower=0, upper=1> theta[NT];
}

transformed parameters {
  real kappa[NG];
  for (j in 1:NG) {
    kappa[j] = kappa_minus2[j] + 2; 
  }
}

model {
  omega_o ~ beta(1, 1);
  kappa_o ~ gamma(0.01, 0.01);
  
  omega ~ beta(omega_o * (kappa_o - 2) + 1, (1 - omega_o) * (kappa_o - 2) + 1);
  kappa_minus2 ~ gamma(0.01, 0.01);
  for (i in 1:NT) {
    real o = omega[G[i]];
    real k = kappa[G[i]];
    theta[i] ~ beta(o * (k- 2) + 1, (1 - o) * (k- 2) + 1);
    Z[i] ~ binomial(N[i], theta[i]);
  }
}
")

data <- read_csv("data/BattingAverage.csv") %>% 
  mutate(pos = as.integer(factor(PriPos)))
stan_data <- list(
  NT = nrow(data), 
  NG = n_distinct(data$pos), 
  Z = data$Hits, 
  N = data$AtBats, 
  G = data$pos
)
fit <- sampling(model, data = stan_data, seed = 123, 
                chains = 4, warmup = 500, iter = 10000, thin = 2)
fit
ggs_traceplot(ggs(fit, inc_warmup = T), family = "theta\\[[123]\\]")
#ggmcmc(ggs(fit), file = plot_file_name("diag"))  ## too big


################################################################################
# result
################################################################################

# data
NUM_POS <- max(data$pos)
res <- fit_to_tibble(fit)
id_to_mle <- function(idx) data$Hits[[idx]] / data$AtBats[[idx]]
pos_map <- data %>% select(PriPos, pos) %>% distinct()
id_to_pos <- function(id) filter(pos_map, pos == id)$PriPos

data %>% select(PriPos, pos) 


players <- data %>% 
  transmute(
    id = 1:n(), 
    ba_mle = Hits / AtBats, 
    Player
  )

positions <- data %>% 
  group_by(pos) %>% 
  summarise(pos_name = first(PriPos), ba_mle = sum(Hits) / sum(AtBats))


# picher vs catcher
# figure9.14
grid.arrange(
  plot_post_pair_diff(res, c(7, 4), "omega"), 
  plot_post_pair_diff(res, c(4, 1), "omega"), 
  ncol = 2, 
  left = "Pitcher vs. Catcher", right = "Catcher vs. 1st Base"
) %>% ggsave(plot_file_name("position_compare_example"))

# skip displaying player name, position and N, z (tiresome)
# figure9.15
grid.arrange(
  plot_post_pair_diff(res, c(75, 156), "theta",  mle_func = id_to_mle), 
  plot_post_pair_diff(res, c(159, 844), "theta", mle_func = id_to_mle), 
  ncol = 2
) %>% ggsave_plot(plot_file_name("plar_compare1"))

# figure9.16
grid.arrange(
  plot_post_pair_diff(res, c(494, 754), "theta", mle_func = id_to_mle), 
  plot_post_pair_diff(res, c(573, 428), "theta", mle_func = id_to_mle), 
  ncol = 2
) %>% ggsave_plot(plot_file_name("plar_compare2"))

# posteria of position and total
par_and_title <- 1:NUM_POS %>% 
  map(~list(par = "omega_" %+% .x, title = id_to_pos(.x))) %>% 
  list.append(list(par = "omega_o", title = "Total"))
grid.arrange(
  grobs = map(par_and_title, ~plot_post(res, .x$par, title = .x$title, xlim = c(0.11, 0.27))), 
  ncol = 3, 
) %>% ggsave_plot(plot_file_name("position_wise_ba"))
