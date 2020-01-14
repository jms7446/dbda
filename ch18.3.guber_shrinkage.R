################################################################################
# env
################################################################################

file_prefix <- "ch18.3"
reuse_fit <- T
source("preset.R")

input_file <- "data/Guber1999data.csv"
N_RAND = 12
x_org_names <- c("Spend", "PrcntTake")
y_name <- "SATT"
x_rnd_names <- map_chr(1:N_RAND, ~c("xrand_" %+% .x))
x_names <- c(x_org_names, x_rnd_names)
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
    real sigma_beta;
    real alpha;
    vector[K] beta;
    real<lower=0> sigma;
    real<lower=0> nu_minus1;
  }
  
  transformed parameters {
    real nu = nu_minus1 + 1;
  }
  
  model {
    sigma_beta ~ gamma(2.618,1.618);   // mode 1.0, sd 1.0;
    beta ~ student_t(2, 0, sigma_beta);
    nu_minus1 ~ exponential(1 / 29.0);
    y ~ student_t(nu, x * beta + alpha, sigma);
  }
"

# make and add random fields
rnd_tbl <- matrix(rnorm(nrow(data) * N_RAND), ncol = N_RAND); colnames(rnd_tbl) <- x_rnd_names
data <- read_csv(input_file) %>% 
  bind_cols(as_tibble(rnd_tbl))

stan_data <- list(
  N = nrow(data), 
  K = length(x_names), 
  x = data %>% select(x_names) %>% mutate_if(is.numeric, standardize) %>% as.matrix(),
  y = data[[y_name]] %>% standardize()
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

# note: coeff of normalized input data
grid.arrange(
  grobs = map2(
    c(coeff_names, "alpha", "sigma", "log_nu", "Rsq"), 
    c(x_names, "Intercept", "Scale", "Normaliy", "R-square"), 
    ~plot_post(res, param_name = .x, title = .y, comp_val = 0)), 
  ncol = 5
) %>% ggsave_plot(plot_file_name("Figure12_posteria"), width=9, height = 9)
