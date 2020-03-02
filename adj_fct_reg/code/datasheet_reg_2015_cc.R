rm(list = ls())

library(tidyverse)
library(gridExtra)
library(jtools)
library(mctest)
library(countrycode)
library(VGAM)#for tobit censor
library(readxl)

##### HLO countries #####
# HLO 2015 used, if not available most recent up to 2000 used

d1 <- read.csv("./data/qamys_final_piaac_step.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  select(iso, country, adj_factor, qamys)

d2 <- read.csv("./data/df_mys_cc_broad_age.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age)

d3 <- read.csv("./data/df_LS_cc_broad_age.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age)

d4 <- read.csv("./data/df_cc_broad_age_prop.csv") %>% 
  select(iso, prop_19, prop_20_64, prop_65)

d5 <- read.csv("./data/df_illiterate_prop_cc.csv") 

d6 <- read.csv("./data/df_hlo.csv") %>%
  select(iso, hlo_mean, year) %>% 
  # filter(year >= 2000) %>% 
  complete(year, nesting(iso)) %>% 
  spread(year, hlo_mean) %>% 
  mutate(hlo_mean = `2015`, 
         hlo_mean = ifelse(is.na(hlo_mean), `2010`, hlo_mean),
         hlo_mean = ifelse(is.na(hlo_mean), `2005`, hlo_mean),
         hlo_mean = ifelse(is.na(hlo_mean), `2000`, hlo_mean)) %>% 
  select(iso, hlo_mean)

d7 <- read_excel("./data/EDS2020_COUNTRY_CODES.xlsx")

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  filter(iso %in% d7$`ISO 3166-1 numeric`)

r1_temp <- r1 %>% 
  filter(!is.na(adj_factor))

# stepwise regression to find the correct model 

library(MASS)
# Fit the full model 
full.model <- lm(qamys ~  wic_mys +highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + hlo_mean , data = r1)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(qamys ~  wic_mys + highLS + prop_20_64 + hlo_mean, data = r1)

summ(lm1)

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  arrange(-fit1) %>% 
  mutate(country = countrycode(iso, "iso3n", "iso3c"))

pdf("./figures/datasheet_regression_fit.pdf")
ggplot(df1, aes(x = qamys, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank())
dev.off()

p <- ggplot(df1, aes(x = reorder(country, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = country, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()

pdf("./figures/datasheet.pdf")
print(p)
dev.off()

df_datasheet <- df1 %>% 
  rename(estimate = fit1, 
         lower = lwr1, 
         upper = upr1) %>% 
  dplyr::select(iso, cc, wic_mys,estimate, lower, upper, qamys, adj_factor, highLS, prop_20_64, hlo_mean)

write.csv(df_datasheet, "./data/datasheet_results.csv", row.names = F)
