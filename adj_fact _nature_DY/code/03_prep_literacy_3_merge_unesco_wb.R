#  read literacy data from old UNESCO files, WB data and assumptions for developed countries

rm(list = ls())

library(readxl)
library(tidyverse)
library(countrycode)

# 131 countries 
# by age and sex and single years
# aggregate 

# years <- seq(1970, 2025, by = 5)

ccs <- read.csv("./data/df_mys_sex.csv")
ccs_iso <- unique(ccs$iso)

d1 <- read.csv("./data/unesco_lit_est.csv") %>% 
  filter(age_st == "15+", 
         !is.na(iso3c)) %>% 
  select(year, per_male, per_female, iso) %>% 
  gather(sex, unesco_il_prop, -c(year, iso)) %>% 
  mutate(sex = ifelse(sex == "per_male", "Male", "Female"))
  
  
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

# Europe countries in WIC 
   d4 <- d3 %>% 
    filter(continent == "Europe", 
           iso %in% ccs_iso)
  
pdf("./figures/europe_ill_wic_countries.pdf")
  for(i in seq(1, length(unique(d4$cc)), 1)){
   p <- ggplot(d4 %>% filter(cc %in% unique(d4$cc)[i]), aes(x = year, y = unesco_il_prop)) +
    geom_point() +
    geom_point(aes(x = year, y = wb_il_prop), col = "red") +
     ggtitle(paste0(unique(d4$cc)[i])) +
    facet_grid(~ sex) 
   print(p)
  }
dev.off()
