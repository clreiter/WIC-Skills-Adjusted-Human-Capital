rm(list = ls())

library(tidyverse)
library(readxl)

d1 <- read_excel("./data/wic_countries_unesco_lit_est.xlsx")

d2 <- d1 %>% 
  mutate(est_female = as.numeric(est_female),
         est_male = as.numeric(est_male),
         illiterate_prop = uis_tot_2015_19, 
         illiterate_prop = ifelse(is.na(illiterate_prop), uis_tot_2010_19, illiterate_prop), 
         illiterate_prop = ifelse(is.na(illiterate_prop), (est_female + est_male)/2, illiterate_prop), 
         illiterate_prop = ifelse(is.na(illiterate_prop), (manual_est_female + manual_est_male)/2, illiterate_prop)) %>% 
  rename(iso = `_ISO3N_`) %>% 
  select(iso, illiterate_prop)

write.csv(d2, "./data/df_illiterate_prop_cc.csv", row.names = F)
