rm(list = ls())
library(tidyverse)
library(gridExtra)
library(jtools)
library(countrycode)
library(foreign)

lit_data <- read.csv("./data/literacy_1970_2015_gender.csv") %>%
  rename(illiterate_prop=tot_ill_)

qamys_data <- read.csv("./data/qamys_1970-2015.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys, cc=country) %>% 
  select(iso, adj_factor, qamys_piaac)

age1 <- read.csv("S:/Qualmat/adj_fct_reg/data/wic_broad_age_groups_pop_1970_2015.csv")
age2 <- age1 %>%
  rename(cc = Area, year = Year, 
         age = Age,
         wic_pop = Population) %>% 
  spread(age, wic_pop) %>% 
  mutate(iso = countrycode(cc, "country.name", "iso3n"),
         tot_pop = (`0--19` + `20--64` + `65+`), 
         prop_19 = `0--19`/tot_pop, 
         prop_20_64 = `20--64`/tot_pop, 
         prop_65 = `65+`/tot_pop)
#write.csv(age2, "./data/broad_age_prop_1970_2015.csv", row.names = F)

ls1 <- read.csv("./data/wic_enrolment_1970_2015.csv")
ls2 <- ls1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         edu = Education,
         iso = ISOCode, 
         LS = Distribution) %>% 
  spread(edu, LS) %>% 
  mutate(LS = `Under 15` + `No Education` + `Incomplete Primary` + `Primary` + `Lower Secondary`, 
         highLS = 100 - LS) %>% 
  select(cc, year, iso, highLS)
#mutate(iso = countrycode(Area, "country.name", "iso3n")) 
#write.csv(ls2, ""./data/LS_prop_1970_2015.csv"", row.names = F)

mysdata <- read.csv("./data/wicdf_mys_1970-2015.csv") %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         iso = ISOCode, wic_mys = Years)
select(-age)

r1 <- qamys_data %>% 
  left_join(age2) %>% 
  left_join(lit_data) %>% 
  left_join(ls2) %>% 
  left_join(mysdata)

library(MASS)
# Fit the full model 
full.model <- lm(qamys_piaac ~  wic_mys + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + year, data = r1)
summary(full.model)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
lm
lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(plot = case_when(wic_mys > 10.85 ~ 1,
                          wic_mys <= 10.85 & wic_mys >= 8.5 ~ 2, 
                          wic_mys < 8.5 ~ 3))
