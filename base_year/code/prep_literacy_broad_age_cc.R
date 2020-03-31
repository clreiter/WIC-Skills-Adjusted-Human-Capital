rm(list = ls())

library(tidyverse)
library(countrycode)


# caner
d1 <- read.csv("./data/new_unesco_lit_est_240220.csv")

d2 <- d1 %>% 
  rename(iso = iso3n, 
         illiterate_prop = illit_) %>% 
  mutate(year = substr(year, 1,4), 
         cc = countrycode(iso, "iso3n", "country.name")) %>% 
  filter(age == "15+", 
         year == 2015) 
  
  

write.csv(d2, "./data/df_illiteracy_cc_broad_age.csv", row.names = F)
