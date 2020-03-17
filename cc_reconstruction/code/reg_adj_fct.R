rm(list = ls())

library(tidyverse)
# library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
 # library(VGAM)#for tobit censor


d1 <- read.csv("./data/qamys_1970-2015.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)

d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

d4 <- read.csv("./data/df_cc_broad_age_prop_1970_2015.csv") %>% 
  select(cc, iso, year, prop_19, prop_20_64, prop_65)

d5 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")

d6 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")


r1 <- d4 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys))

 
# Check DHS correction used above 

r2 <- r1 %>%
   filter(iso %in% c(68, 288, 404, 604), year == 2015) %>%
  mutate(dhs_corr_full = qamys_piaac/qamys_dhs_full, dhs_corr_part = qamys_piaac/qamys_dhs_part)

mean(r2$dhs_corr_full, na.rm = T)

r1_temp <- r1 %>% 
  filter(!is.na(qamys))

p1 <- ggplot(r1_temp, aes(x = log(wic_mys), y = log(qamys))) +
  geom_point() +
  geom_smooth() +
  ggtitle("QAMYS vs WIC MYS") +
  xlab("log(WIC MYS)") +
  ylab("log(QAMYS)") +
  # geom_abline() +
  theme_bw()


png("./figures/log_qamys_log_mys_1970_2015.png", width = 5, height = 5, res = 300, units = "in")
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
  filter(!is.na(qamys))

library(MASS)
# Fit the full model 
full.model <- lm(log(qamys) ~  log(wic_mys) + highLS + prop_19 + prop_20_64 + prop_65 + illiterate_prop + year , data = r1)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

sink("./results/piaac_Step_dhs_corrected_full_stepmodel13March20_log.txt")
print(summary(step.model))
sink()

lm1 <- lm(log(qamys) ~  log(wic_mys) + prop_19 + illiterate_prop + year, data = r1)

summ(lm1)

sink("./results/piaac_Step_dhs_corrected_full_lm1_hlo_13March20_log.txt")
print(summary(lm1))
sink()

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

# OLS assumptions 

par(mfrow = c(2,2))
plot(lm1)
# there is heteroscedasticity
# # try boxcox transformation 
#DID NOT WORK
# 
# library(caret)
# 
# r3_BC <- caret::BoxCoxTrans(r3$qamys)
# print(r3_BC)
# 
# r3 <- cbind(r3, qamys_BC=predict(r3_BC, r3$qamys)) # append the transformed variable to cars
# head(r3)
# 
# lm1_BC <- lm(qamys_BC ~ (wic_mys) + highLS + prop_19 + prop_20_64 + hlo_mean, data = r3)
# 
# 
# plot(lm1_BC)

par(mfrow = c(1,1))
acf(lm1$residuals)


df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(fit1 = exp(fit1), 
         lwr1 = exp(lwr1), 
         upr1 = exp(upr1)) %>%
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit1, qamys_pred))
  

detach("package:MASS", unload=TRUE)

cc_grp <- df1 %>% 
  filter(year == 2015) %>% 
  mutate(plot = case_when((wic_mys > 10.85) ~ 1,
                          (wic_mys <= 10.85 & wic_mys >= 8.5) ~ 2, 
                          (wic_mys < 8.5) ~ 3)) %>% 
  select(iso, plot)
  
df2 <- df1 %>%  
  left_join(cc_grp)

write.csv(df2, "./results/results_piaac_step_dhs_full_corrected_log_1970_2015.csv", row.names = F)

png("./figures/piaac_step_dhs_full_corrected_fit_cc_log_1970_2015.png", width = 5, height = 5, res = 300, units = "in")
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




pdf("./figures/mys_qamys_1970_2015_plot1.pdf")
for(i in seq(unique(df2$year))){
  
pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 1, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
print(pp)
}
dev.off()


pdf("./figures/mys_qamys_1970_2015_plot2.pdf")
for(i in seq(unique(df2$year))){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 2, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()



pdf("./figures/mys_qamys_1970_2015_plot3.pdf")
for(i in seq(unique(df2$year))){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 3, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()




for(i in seq(unique(df2$year))){
plot_name <- paste0("./figures/mys_qamys_",unique(df2$year)[i], "N.png")
png(plot_name, width = 10, height = 20, res = 300, units = "in")
pp <- ggplot(df2 %>% filter(!is.na(fit1), year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
print(pp)
dev.off() 
}



pdf("./figures/mys_qamys_1970_2015_countries.pdf")
for(i in seq(1, length(unique(df2$cc)), 4)){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), cc %in% unique(df2$cc)[i: (i+ 3)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Year") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()
