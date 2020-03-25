rm(list = ls())

library(tidyverse)

d1 <- read.csv("./data/wicdf_mys_1970-2015.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         iso = ISOCode, 
         wic_mys = Years) %>% 
  filter(iso != 900) 

write.csv(d2, "./data/df_mys_cc_broad_age_1970_2015.csv", row.names = F)
