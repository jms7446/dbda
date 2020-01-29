library(glue)
library(tidyverse)
library(rlang)

# for bayes
library(rstan)
library(ggmcmc)
library(bayestestR)
library(hBayesDM)

# for extra plot
library(gridExtra)
library(cowplot) # https://wilkelab.org/cowplot/articles/introduction.html
library(ggthemes) # https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/

source("r_tools/common.R")
source("r_tools/my_plots.R")
source("r_tools/stan_tools.R")

plot_file_name <- function(name) make_plot_file_name(file_prefix, name)
save_file_name <- function(name) make_save_file_name(file_prefix, name)

# env
options(warnPartialMatchDollar = TRUE)
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
options(warnPartialMatchArgs = TRUE)

