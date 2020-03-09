rm(list = ls())

library(tidyverse)
# library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
 # library(VGAM)#for tobit censor
##### HLO countries #####
# HLO 2015 used, if not available most recent up to 2000 used
# Adjustment factor for all dhs countries

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

png("./figures/qamys_dhs_full_partial.png", width = 5, height = 5, res = 300, units = "in")
ggplot(d7, aes(x = qamys_dhs_full, y = qamys_dhs_part)) +
  geom_point() +
  geom_abline() +
  xlab("QAMYS DHS - Can read full sentence") +
  ylab("QAMYS DHS - Can read full or partial sentence") +
  xlim(0,13) +
  ylim(0,13) +
  scale_x_continuous(breaks= seq(0, 12, 2), labels = seq(0, 12, 2)) +
  scale_y_continuous(breaks= seq(0, 12, 2), labels = seq(0, 12, 2)) +
  theme_bw()
dev.off()

# Adjustment factor 1 is share of people who can read a full sentence divided by population-weighted OECD mean literacy rate;
# adjustment factor 2 is share of people who can read a full or part of a sentence divided by population-weighted OECD mean literacy rate.

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  left_join(d7) %>% 
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys))

 
# Check DHS correction used above 

r2 <- r1 %>%
  filter(iso %in% c(68, 288, 404, 604)) %>%
  mutate(dhs_corr_full = qamys_piaac/qamys_dhs_full, dhs_corr_part = qamys_piaac/qamys_dhs_part)

# mean of peru, kenya, bolivia 0.7973025
# mean of peru, kenya, bolivia and ghana 0.8736634


# write.csv(r1, "./data/piaac_step_dhs_corrected_qamys.csv", row.names = F)
 

r1_temp <- r1 %>% 
  filter(!is.na(qamys))

p1 <- ggplot(r1_temp, aes(x = log(wic_mys), y = log(qamys))) +
  geom_point() +
  geom_smooth() +
  ggtitle("QAMYS vs WIC MYS") +
  xlab("WIC MYS") +
  ylab("QAMYS") +
  geom_abline() +
  theme_bw()

png("./figures/log_qamys_log_mys.png", width = 5, height = 5, res = 300, units = "in")
print(p1)
dev.off()


ggplot(r1_temp, aes(x = (wic_mys), y = (qamys))) +
  geom_point() +
  # geom_smooth() +
  ggtitle("QAMYS vs WIC MYS") +
  xlab("WIC MYS") +
  ylab("QAMYS") +
  geom_abline() +
  theme_bw()



r3 <- r1 %>% 
  filter(!is.na(qamys), 
         !is.na(hlo_mean))

library(MASS)
# Fit the full model 
full.model <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + hlo_mean, data = r1)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

sink("./results/piaac_Step_dhs_corrected_full_stepmodel2March20.txt")
print(summary(step.model))
sink()

lm1 <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + hlo_mean, data = r1)

summ(lm1)

sink("./results/piaac_Step_dhs_corrected_full_lm1_hlo_3MArch20.txt")
print(summary(lm1))
sink()

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)



loglm1 <- lm(log(qamys) ~  log(wic_mys) + highLS + prop_19 + prop_20_64 + hlo_mean, data = r1)
summ(loglm1)

loglm1predict <- as.data.frame(predict(loglm1, newdata = r1, interval = "confidence")) 
colnames(loglm1predict) <- paste0(colnames(loglm1predict),"log",1)

# OLS assumptions 



df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(plot = case_when(wic_mys > 10.85 ~ 1,
                          wic_mys <= 10.85 & wic_mys >= 8.5 ~ 2, 
                          wic_mys < 8.5 ~ 3)) %>%
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit1, qamys_pred))

logdf1 <- df1 %>%
  bind_cols(loglm1predict) %>% 
  mutate(logfit1exp = exp(fitlog1))
  


  

# 134 countries
df2 <- df1 %>%
  filter(!is.na(fit1))



write.csv(df1, "./results/results_piaac_step_dhs_full_corrected_plus_pred_and_emprical_hlo.csv", row.names = F)

png("./figures/piaac_step_dhs_full_corrected_fit_hlo_cc.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = qamys_piaac, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank()) +
  ylab("Predicted QAMYS (lm1 model incl. HLO)") +
  xlab("Empirical QAMYS") + 
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()



p1 <- ggplot(df1 %>% filter(!is.na(fit1), plot == 1), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p2 <- ggplot(df1 %>% filter(!is.na(fit1), plot == 2), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS &  QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p3 <- ggplot(df1 %>% filter(!is.na(fit1), plot == 3), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()



p4 <- ggplot(df1 %>% filter(!is.na(fit1)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit1)) +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()


png("./figures/piaac_step_dhs_full_predicted_hlo_my1.png", res = 300, unit = "in", height = 10, width = 10)
print(p1)
dev.off()


png("./figures/piaac_step_dhs_full_predicted_hlo_my2.png", res = 300, unit = "in", height = 10, width = 10)
print(p2)
dev.off()

png("./figures/piaac_step_dhs_full_predicted_hlo_mys3.png", res = 300, unit = "in", height = 10, width = 10)
print(p3)
dev.off()


png("./figures/piaac_step_dhs_full_predicted_hlo.png", res = 300, unit = "in", height = 20, width = 10)
print(p4)
dev.off()
##### Use illiteracy instead of HLO #####

# Fit the full model 
full.model <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop, data = r1)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

sink("./results/piaac_Step_dhs_corrected_full_stepmodel2March20_illiteracy.txt")
print(summary(step.model))
sink()

lm2 <- lm(qamys ~  wic_mys + highLS + prop_19 + prop_20_64 + illiterate_prop, data = r1)

summ(lm2)


sink("./results/piaac_Step_dhs_corrected_full_lm2_illiteracy_3MArch20.txt")
print(summary(lm2))
sink()

lm2predict <- as.data.frame(predict(lm2, newdata = r1, interval = "confidence")) 
colnames(lm2predict) <- paste0(colnames(lm2predict),2)

df3 <- df1 %>% 
  bind_cols(lm2predict) %>%
  mutate(qamys_pred2 = qamys,
         qamys_pred2 = ifelse(is.na(qamys_pred2), fit2, qamys_pred2), 
         qamys_pred2_lwr = ifelse(is.na(qamys), lwr2, NA), 
         qamys_pred2_upr = ifelse(is.na(qamys), upr2, NA))



# 134 countries
df4 <- df3 %>%
  filter(!is.na(fit2))



write.csv(df4, "./results/results_piaac_step_dhs_full_corrected_plus_pred_and_emprical_hlo_illiteracy.csv", row.names = F)

png("./figures/piaac_step_dhs_full_corrected_fit_literacy_cc.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df4, aes(x = qamys_piaac, y = fit2)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank()) +
  ylab("Predicted QAMYS (lm2 model incl. illiteracy)") +
  xlab("Empirical QAMYS") + 
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()



p5 <- ggplot(df4 %>% filter(!is.na(fit2), plot == 1), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p6 <- ggplot(df4 %>% filter(!is.na(fit2), plot == 2), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS &  QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p7 <- ggplot(df4 %>% filter(!is.na(fit2), plot == 3), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()



p8 <- ggplot(df4 %>% filter(!is.na(fit2)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()


png("./figures/piaac_step_dhs_full_predicted_illiteracy_mys1.png", res = 300, unit = "in", height = 10, width = 10)
print(p5)
dev.off()


png("./figures/piaac_step_dhs_full_predicted_illiteracy_mys2.png", res = 300, unit = "in", height = 10, width = 10)
print(p6)
dev.off()

png("./figures/piaac_step_dhs_full_predicted_illiteracy_mys3.png", res = 300, unit = "in", height = 10, width = 10)
print(p7)
dev.off()

png("./figures/piaac_step_dhs_full_predicted_illiteracy.png", res = 300, unit = "in", height = 20, width = 10)
print(p8)
dev.off()

### Empirical plus predicted for literacy model

p9 <- ggplot(df4 %>% filter(plot == 1), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = qamys_pred2)) +
  geom_errorbar(aes(ymin = qamys_pred2_lwr, ymax = qamys_pred2_upr)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p10 <- ggplot(df4 %>% filter(!is.na(fit2), plot == 2), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = qamys_pred2)) +
  geom_errorbar(aes(ymin = qamys_pred2_lwr, ymax = qamys_pred2_upr)) +
  xlab("Country") +
  ylab("WIC MYS &  QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()


p11 <- ggplot(df4 %>% filter(!is.na(fit2), plot == 3), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = qamys_pred2)) +
  geom_errorbar(aes(ymin = qamys_pred2_lwr, ymax = qamys_pred2_upr)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()



p12 <- ggplot(df4 %>% filter(!is.na(fit2)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = qamys_pred2)) +
  geom_errorbar(aes(ymin = qamys_pred2_lwr, ymax = qamys_pred2_upr)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()



png("./figures/piaac_step_dhs_full_empirical_predicted_illiteracy_mys1.png", res = 300, unit = "in", height = 10, width = 10)
print(p9)
dev.off()


png("./figures/piaac_step_dhs_full_empirical_predicted_illiteracy_mys2.png", res = 300, unit = "in", height = 10, width = 10)
print(p10)
dev.off()

png("./figures/piaac_step_dhs_full_empirical_predicted_illiteracy_mys3.png", res = 300, unit = "in", height = 10, width = 10)
print(p11)
dev.off()

png("./figures/piaac_step_dhs_full_empirical_predicted_illiteracy.png", res = 300, unit = "in", height = 20, width = 10)
print(p12)
dev.off()
