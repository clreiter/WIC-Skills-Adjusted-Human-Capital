# Merge literacy data from UNESCO files and WB data 

rm(list = ls())

library(readxl)
library(tidyverse)
library(countrycode)

# iso codes for the WIC country list
ccs <- read.csv("./data/df_mys_sex.csv")
ccs_iso <- unique(ccs$iso)

# UNESCO data merged in step 1
d1 <- read.csv("./data/unesco_lit_est.csv") %>% 
  filter(age_st == "15+", 
         !is.na(iso3c)) %>% 
  select(year, per_male, per_female, iso) %>% 
  gather(sex, unesco_il_prop, -c(year, iso)) %>% 
  mutate(sex = ifelse(sex == "per_male", "Male", "Female"))
  
# World Bank data prepared in step 2  
d2 <- read.csv("./data/WB_literate_prop.csv") %>% 
  select(-literate_prop) %>% 
  filter(year >= 1970)

d3 <- expand.grid(iso = ccs_iso, year = seq(1970, 2025, 1), sex = c("Male", "Female")) %>% 
  full_join(d1) %>% 
  full_join(d2) %>% 
  mutate(cc = countrycode(iso, "iso3n", "country.name"), 
         continent = countrycode(iso, "iso3n", "continent")) %>% 
  filter(!is.na(continent))

write.csv(d3, "./data/lit_unesco_wb.csv", row.names = F)
