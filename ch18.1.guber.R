################################################################################
# env
################################################################################

file_prefix <- "ch18.1"
reuse_fit <- T
source("preset.R")

library(GGally)

input_file <- "data/Guber1999data.csv"
x_names <- c("Spend", "PrcntTake")
y_name <- "SATT"
coeff_names <- map_chr(1:length(x_names), ~"beta_" %+% .x)

################################################################################
# prepare
################################################################################

model_code <- "
  data {
    int<lower=0> N;
    int<lower=0> K;
    matrix[N, K] x;
    vector[N] y;
  }
  
  transformed data {
  }
  
  parameters {
    real alpha;
    vector[K] beta;
    real<lower=0> sigma;
    real<lower=0> nu_minus1;
  }
  
  transformed parameters {
    real nu = nu_minus1 + 1;
  }
  
  model {
    nu_minus1 ~ exponential(1 / 29.0);
    y ~ student_t(nu, x * beta + alpha, sigma);
  }
"
data <- read_csv(input_file)
stan_data <- list(
  N = nrow(data), 
  K = length(x_names), 
  x = data %>% select(x_names) %>% as.matrix(),
  y = data[[y_name]]
)

################################################################################
# fit model
################################################################################

if (reuse_fit) {
  load(save_file_name("fit"))
} else {
  model <- stan_model(model_code = model_code)
  fit <- sampling(model, data = stan_data, seed = 123, 
                  pars = c("nu_minus1"), include = F, 
                  chains = 4, warmup = 300, iter = 10000, thin = 2) 
  save(fit, file = save_file_name("fit"))
}

fit
#ggs_traceplot(ggs(fit, inc_warmup = F))


################################################################################
# result
################################################################################

res <- fit_to_tibble(fit) %>% 
  mutate(log_nu = log10(nu)) %>% 
  add_linear_regression_Rsq(data, coeff_names, x_names, y_name)

# Figure18.5
grid.arrange(
  grobs = map2(
    c(coeff_names, "alpha", "sigma", "log_nu", "Rsq"), 
    c(x_names, "Intercept", "Scale", "Normaliy", "R-square"), 
    ~plot_post(res, param_name = .x, title = .y)), 
  ncol = 2
) %>% ggsave_plot(plot_file_name("Figure5_1_posteria"), width = 6, height = 7)

res %>% select(coeff_names, "alpha", "sigma", "log_nu") %>% 
  ggpairs(lower = list(continuous = "density")) 
ggsave(plot_file_name("Figure5_2_parameter_pair_plot"))
