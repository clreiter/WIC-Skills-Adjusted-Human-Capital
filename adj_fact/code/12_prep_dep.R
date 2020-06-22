rm(list = ls())

library(tidyverse)


d1 <- read.csv("./data/wicdf_old_age_dep.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         iso = ISOCode, 
         old_dep = Ratio)


write.csv(d2, "./data/df_old_age_dep_1970_2015.csv", row.names = F)
