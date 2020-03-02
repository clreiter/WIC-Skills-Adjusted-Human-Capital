rm(list = ls())

library(tidyverse)
library(countrycode)
library(readxl)

# caner
d1 <- read.csv("./data/new_unesco_lit_est_240220.csv")

d2 <- d1 %>% 
  rename(iso = iso3n, 
         illiterate_prop = illit_) %>% 
  mutate(year = substr(year, 1,4), 
         cc = countrycode(iso, "iso3n", "country.name")) %>% 
  filter(age == "15+", 
         year == 2015) 

cc_unesco_as <- d2 %>% 
  select(iso, illiterate_prop) %>% 
  rename(unesco_caner = illiterate_prop)

d3 <- read.csv("./data/lit_europe_estimated_othersuis_wb.csv")

d4 <- d3 %>% 
  filter(year == 2015)

cc_unesco_wb_eu_est_female <- d4 %>% 
    filter(sex == "Female", 
           !is.na(est3)) %>% 
  select(iso, est3) %>% 
  rename(est_female = est3)


cc_unesco_wb_eu_est_male <- d4 %>% 
  filter(sex == "Male", 
         !is.na(est3)) %>% 
  select(iso, est3) %>% 
  rename(est_male = est3)

wic_cc <- read.csv("./data/df_pop_cc_broad_age.csv") %>% 
  filter(age == "15+") %>% 
  select(iso, cc) %>% 
  left_join(cc_unesco_as) %>% 
  left_join(cc_unesco_wb_eu_est_female) %>% 
  left_join(cc_unesco_wb_eu_est_male)


write.csv(wic_cc, "./data/wic_countries.csv", row.names = F)
