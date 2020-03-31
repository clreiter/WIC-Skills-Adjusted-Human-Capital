rm(list = ls())

library(tidyverse)
library(countrycode)
d1 <- read.csv("./data/hlo.csv")

names(d1) <-c("cc", "year", "hlo_mean", "hlo_min", "hlo_intermediate", "hlo_advanced") 

d2 <- d1 %>% 
  mutate(iso = countrycode(cc, "country.name", "iso3n")) %>% 
  select(-cc)

write.csv(d2, "./data/df_hlo.csv", row.names = F)
