rm(list = ls())

library(tidyverse)

d1 <- read.csv("./data/wicdf_enrolment_1970_2015.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         edu = Education,
         iso = ISOCode, 
         LS = Distribution) %>% 
  spread(edu, LS) %>% 
  mutate(LS = `Under 15` + `No Education` + `Incomplete Primary` + `Primary` + `Lower Secondary`, 
         highLS = 100 - LS) %>% 
  select(cc, year, iso, age, highLS)

write.csv(d2, "./data/df_LS_cc_broad_age_1970_2015.csv", row.names = F)
