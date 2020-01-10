source("tools/stan_tools.R")
source("tools/common.R")
source("tools/my_plots.R")

plot_file_name <- function(name) make_plot_file_name("ch09.1", name)

model <- stan_model(model_code = "
data {
  int<lower=0> N;
  int<lower=0> NG;
  int<lower=0> Y[N];
  int<lower=0> G[N];
}

parameters {
  real<lower=0> kappa_minus2;
  real<lower=0, upper=1> omega;
  real<lower=0, upper=1> theta[NG];
}

transformed parameters {
  real kappa = kappa_minus2 + 2;
}

model {
  kappa_minus2 ~ gamma(0.01, 0.01);
  theta ~ beta(omega * (kappa - 2) + 1, (1 - omega) * (kappa - 2) + 1);
  for (i in 1:N) {
    Y[i] ~ bernoulli(theta[G[i]]);
  }
}
")

# fit
raw_data <- read_csv("data/TherapeuticTouchData.csv") %>% 
  mutate(s_factor = factor(s), s = as.integer(s_factor))
data <- list(
  N = nrow(raw_data), 
  NG = n_distinct(raw_data$s), 
  Y = raw_data$y, 
  G = raw_data$s
)
fit <- sampling(model, data = data, seed = 123, pars = c("kappa_minus2"), include = F, 
                chains = 4, warmup = 1000, iter = 5000, thin = 1, 
                control = list(adapt_delta = 0.9, max_treedepth = 15))
fit
interests_pars <- "kappa|omega|theta\\[[12]\\]"
ggs_traceplot(ggs(fit, family = interests_pars, inc_warmup = T))
ggmcmc(ggs(fit, family = interests_pars), file = plot_file_name("diag"))


# result
res <- fit_to_tibble(fit)
mle <- raw_data %>% group_by(s) %>% summarise(m = mean(y))
get_mle <- function(i) (mle %>% filter(s == i))$m

plot_result <- function(comp) {
  grid.arrange(
    grid.arrange(
      plot_post(res, "kappa", title = "Posteria of Kappa"), 
      plot_post(res, "omega", comp_val = 0.5, title = "Posteria of Omega"), 
      ncol = 2
    ), 
    plot_post_pair_diff(res, comp, par_prefix = "theta", comp_val = 0.5, mle_func = get_mle), 
    heights = c(1.5, length(comp))
  )
}
p <- plot_result(c(1, 14, 28))
ggsave(plot_file_name("results1"), p)

