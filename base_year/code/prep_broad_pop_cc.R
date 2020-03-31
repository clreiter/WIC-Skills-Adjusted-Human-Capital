rm(list = ls())

library(tidyverse)

d1 <- read.csv("./data/wicdf_broad_age_prop.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         iso = ISOCode, 
         wic_pop = Population) %>% 
  spread(age, wic_pop) %>% 
  mutate(tot_pop = (`0--19` + `20--64` + `65+`), 
         prop_19 = `0--19`/tot_pop, 
         prop_20_64 = `20--64`/tot_pop, 
         prop_65 = `65+`/tot_pop)

write.csv(d2, "./data/df_cc_broad_age_prop.csv", row.names = F)
