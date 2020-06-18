rm(list = ls())

library(tidyverse)

#illiteracy data downloaded from UIS and then missing values replaced
# in STATA using old UNESCO estimates and regional averages */
d1 <- read.csv("./data/literacy_1970_2015_gender.csv")

d2 <- d1 %>% 
  rename(illiterate_prop = tot_ill_) %>% 
  select(iso, year, illiterate_prop)

write.csv(d2, "./data/df_illiterate_prop_cc_1970_2015.csv", row.names = F)
