rm(list = ls())

library(tidyverse)
library(jtools)
library(countrycode)

##### DHS countries #####


d1 <- read.csv("./data/qamys_final_piaac_step.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys)
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

d7 <- read.csv("./data/qamys_dhs_full_part.csv") 

# Adjustment factor 1 is share of people who can read a full sentence divided by population-weighted OECD mean literacy rate;
# adjustment factor 2 is share of people who can read a full or part of a sentence divided by population-weighted OECD mean literacy rate.

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  left_join(d7) %>% 
  filter(!is.na(qamys_dhs_full))

 

p1 <- ggplot(r1_temp, aes(x = wic_mys, y = qamys_dhs_full)) +
  geom_point() +
  geom_smooth() +
  ggtitle("DHS QAMYS vs WIC MYS") +
  xlab("WIC MYS") +
  ylab("DHS QAMYS") +
  geom_abline() +
  xlim(0, 15) +
  ylim(0, 15)


png("./figures/dhs_only_dhs_qamys_wicmys.png", width = 10, height = 10, res = 300, units = "in")
print(p1)
dev.off()

r3 <- r1 %>% 
  filter(!is.na(hlo_mean))

library(MASS)
# Fit the full model 
full.model <- lm(qamys_dhs_full ~  wic_mys + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + hlo_mean, data = r3)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

sink("./results/dhs_only_3March20.txt")
print(summary(step.model))
sink()

lm1 <- lm(qamys_dhs_full ~  wic_mys + highLS + prop_65 + illiterate_prop, data = r1)

summ(lm1)


sink("./results/dhsonly3March20.txt")
print(summary(lm1))
sink()

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

df1 <- r1 %>% 
  bind_cols(lm1predict)


write.csv(df1, "./results/results_dhs_only_3March20.csv", row.names = F)


p2 <- ggplot(df1 %>% filter(!is.na(fit1)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()

png("./figures/dhs_only_predicted.png", res = 300, unit = "in", height = 10, width = 10)
print(p2)
dev.off()

