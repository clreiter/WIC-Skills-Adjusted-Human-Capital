rm(list = ls())

library(tidyverse)
library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
 # library(VGAM)#for tobit censor
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

d7 <- read.csv("./data/new_adj-factor_for-below-0-countries.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  select(iso, adjustment_factor)

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  left_join(d7) %>% 
  mutate(adj_factor = ifelse(is.na(adj_factor), adjustment_factor, adj_factor), 
         qamys = wic_mys * adj_factor)

r1_temp <- r1 %>% 
  filter(!is.na(adj_factor))

p1 <- ggplot(r1_temp, aes(x = wic_mys, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, wic_mys < 11 | adj_factor > 1.1 |adj_factor <= 0.95), 
            aes(wic_mys, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs WIC MYS") +
  xlab("WIC MYS") +
  ylab("Adjustment factor")

p2 <- ggplot(r1_temp, aes(x = illiterate_prop, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, illiterate_prop > 5), 
           aes(illiterate_prop-0.2, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs Illiterate proportion") +
  xlab("Illiterate proportion") +
  ylab("Adjustment factor")

p3 <- ggplot(r1_temp, aes(x = highLS, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, highLS == 91.4 | adj_factor < 0.90 | adj_factor > 1.1), 
            aes(highLS, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion with higher than Lower secondary education") +
  xlab("Lower secondary +") +
  ylab("Adjustment factor")

p4 <- ggplot(r1_temp, aes(x = prop_19, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, prop_19 > 0.275 | adj_factor > 1.1 |adj_factor <= 0.95), 
            aes(prop_19, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion of 0-19 population") +
  xlab("Proportion 0-19 population") +
  ylab("Adjustment factor")

p5 <- ggplot(r1_temp %>% filter(iso != 702, iso != 410), aes(x = hlo_mean, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, hlo_mean > 600 | adj_factor > 1.1 |adj_factor <= 0.95), 
            aes(hlo_mean, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs HLO (without Singapore & Rep. Korea)") +
  xlab("HLO") +
  ylab("Adjustment factor")


p6 <- ggplot(r1_temp, aes(x = hlo_mean, y = adj_factor)) +
  geom_point() +
  geom_text(data = subset(r1_temp, hlo_mean > 600 | adj_factor > 1.1 |adj_factor <= 0.95), 
            aes(hlo_mean, adj_factor, label = cc)) +
  geom_smooth() +
  ggtitle("Adjustment factor vs HLO") +
  xlab("HLO") +
  ylab("Adjustment factor")

pdf("./figures/Scatterplot_adj_factor_piaac_step_dhs.pdf")
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

# check collinearity

pairs(r1_temp[,6:13])

p7 <- ggplot(r1_temp, aes(x = wic_mys, y = highLS)) +
  geom_point() +
  geom_text(data = subset(r1_temp, wic_mys < 11.5 | highLS < 70), 
           aes(wic_mys, highLS, label = cc)) +
  geom_smooth() +
  ggtitle("Lower secondary+ vs WIC MYS") +
  xlab("WIC MYS") +
  ylab("High LS")


library(MASS)
# Fit the full model 
full.model <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + hlo_mean, data = r1)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

# 134 countries 
lm1 <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + hlo_mean, data = r1)

summ(lm1)

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(qamys_pred = qamys, 
         qamys_pred = ifelse(is.na(qamys_pred), fit1, qamys_pred))

write.csv(df1, "./results/results_piaac_step_dhs(inc_part).csv", row.names = F)

pdf("./figures/piaac_step_dhs_regression_fit.pdf")
ggplot(df1, aes(x = qamys, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank())
dev.off()



p1 <- ggplot(df1 %>% filter(!is.na(fit1)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()

pdf("./figures/piaac_step_dhs_predicted_mys.pdf")
print(p1)
dev.off()





# tru tobit censor at 0

tm1 <- vglm(qamys ~ wic_mys +highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + hlo_mean, tobit(Lower = 0), data = r1_temp)
summary(tm1)
tm1predict <- as.data.frame(predict(tm1, newdata = r1, interval = "confidence")) 
colnames(tm1predict) <- paste0(colnames(tm1predict),1)

dd <- VGAM::predict(r1,tm1,type = c("response"))

predict.vglm(tm1, newdata = r1, type = "response")


df2 <- r1 %>% 
  bind_cols(tm1@fitted.values %>% as.data.frame())
