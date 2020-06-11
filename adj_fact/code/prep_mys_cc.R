rm(list = ls())

library(tidyverse)

d1 <- read.csv("./data/wicdf_mys_broad_age.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         iso = ISOCode, 
         wic_mys = Years)

write.csv(d2, "./data/df_mys_cc_broad_age.csv", row.names = F)
