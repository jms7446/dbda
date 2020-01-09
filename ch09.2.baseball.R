source("tools/stan_tools.R")
source("tools/common.R")
source("tools/my_plots.R")

plot_file_name <- function(name) make_plot_file_name("ch09.2", name)

data <- read_csv("data/BattingAverage.csv")
data <- data %>% 
  mutate(
    pos_ft = factor(PriPos), 
    pos = as.integer(pos_ft)
  )
data


model <- stan_model(model_code = "
")
