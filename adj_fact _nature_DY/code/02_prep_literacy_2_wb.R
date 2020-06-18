# World Bank illiteracy rate 
# 11 Feb 20 

rm(list = ls())
library(tidyverse)
library(countrycode)
library(readxl)

d1 <- read_excel("./data/WB_literacy_rate_female.xlsx")

d2 <- d1 %>% 
  mutate(iso = countrycode(`Country Code`, "iso3c", "iso3n")) %>% 
  filter(!is.na(iso)) %>% 
  select(-c(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Code`)) %>% 
  gather(year, literate_prop, -iso) %>% 
  mutate(wb_il_prop = 100 - literate_prop, 
         sex = "Female")

d3 <- read_excel("./data/WB_literacy_rate_male.xlsx")

d4 <- d3 %>% 
  mutate(iso = countrycode(`Country Code`, "iso3c", "iso3n")) %>% 
  filter(!is.na(iso)) %>% 
  select(-c(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Code`)) %>% 
  gather(year, literate_prop, -iso) %>% 
  mutate(wb_il_prop = 100 - literate_prop, 
         sex = "Male")

d5 <- bind_rows(d4, d2)

write.csv(d5, "./data/WB_literate_prop.csv", row.names = F)
