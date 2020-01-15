source("tools/stan_tools.R")

# make model
model_code = "
  data {
    int<lower=0> N_total;
    int<lower=0> N_subj;
    int y[N_total];
    int s[N_total];
  }
  
  parameters {
    real<lower=0,upper=1> theta[N_subj];
  }
  
  model {
    for (i in 1:N_total) {
      y[i] ~ bernoulli(theta[s[i]]);
    }
    for (j in 1:N_subj) {
      theta[j] ~ beta(2, 2);
    }
  }
"
mod <- stan_model(model_code = model_code)

# make data
d <- read.csv("data/z6N8z2N7.csv")
data <- list(
  y = d$y, 
  s = as.numeric(d$s), 
  N_total = length(d$y), 
  N_subj = length(unique(d$s))
)
data

# sampling
fit <- sampling(
  mod, data = data, seed = 1234, 
  chains = 3, iter = 5000, warmup = 300, thin = 1
)
fit
res <- as_tibble(as.data.frame(fit))

# diagnosis
ggmcmc(ggs(fit, inc_warmup = T), file = "plot/ch08.1.traceplot.pdf", plot = "traceplot")
ggmcmc(ggs(fit), file = "plot/ch08.1.pdf")

# check posteria
plot_post(res, "theta[1]")
res %>% 
  mutate(diff = `theta[1]` - `theta[2]`) %>% 
  plot_post("diff", rope = c(-0.05, 0.05))

