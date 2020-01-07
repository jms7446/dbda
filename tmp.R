source("tools/stan_tools.R")
source("tools/common.R")

res <- tibble(
  a = c(1, 1.2, 0.8), 
  b = c(1, 0, 2), 
  m = c(1, 2, 3), 
  s = c(1, 0.5, 2)
)

line_grid <- res %>% sample_n(3) %>% 
  make_line_grid(xs = seq(0, 100, length = 5), y_func = X * a + b)
dist_grid <- line_grid %>% 
  make_dist_grid(X, Y, s, pdf = dnorm)

ggplot() + 
  geom_line(data = line_grid, aes(X, Y, group = line_id), color = "skyblue", alpha = 0.9) + 
  geom_path(data = dist_grid, aes(XX, YY, group = point_id), color = "skyblue", alpha = 0.6) 


















