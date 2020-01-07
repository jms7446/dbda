library(tidyverse)

d <- read_csv("data/HtWtData30.csv")
d %>% count(male)
d %>% 
  group_by(male) %>% 
  summarise(height = mean(height), weight = mean(weight))

d <- read_csv("data/HierLinRegressData.csv")
d %>% count(Subj)

d <- read_csv("data/IncomeFamszState3yr.csv")
d2 <- read_csv("data/IncomeFamszState.csv")
dim(d)
dim(d2)
d
d2
