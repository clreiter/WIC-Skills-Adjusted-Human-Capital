rm(list = ls())

library(tidyverse)

# DHS adjustment factor2
# Adjustment factor 1 is share of people who can read a full sentence divided by population-weighted OECD mean literacy rate;
# adjustment factor 2 is share of people who can read a full or part of a sentence divided by population-weighted OECD mean literacy rate.



d1 <- read.csv("./data/DHS_adjustment-factor_2000-15.csv") %>% 
  select(iso, year, adj_factor1, adj_factor2) %>% 
  rename(dhs_factor_full = adj_factor1, 
         dhs_factor_part = adj_factor2)

ggplot(d1, aes(x = dhs_factor_full, y = dhs_factor_part)) +
  geom_point() +
  geom_abline(intercept = 0) +
  ylim(0,1) +
  xlim(0,1)

d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age)

d3 <- d2 %>% 
  left_join(d1) %>% 
  mutate(qamys_dhs_full = wic_mys * dhs_factor_full,
         qamys_dhs_part = wic_mys * dhs_factor_part) %>% 
  select(iso, year, qamys_dhs_full, qamys_dhs_part)

write.csv(d3, "./data/qamys_dhs_full_part_2000_2015.csv", row.names = F)

