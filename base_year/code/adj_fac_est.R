rm(list = ls())

library(tidyverse)
library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
 # library(VGAM)#for tobit censor
##### HLO countries #####
# HLO 2015 used, if not available most recent up to 2000 used
# Adjustment factor for all dhs countries

d1 <- read.csv("./data/qamys_final_piaac_step.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys,
         cc=country)%>%
  select(iso, cc, adj_factor, qamys_piaac)

d2 <- read.csv("./data/df_mys_cc_broad_age.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age)

d3 <- read.csv("./data/df_LS_cc_broad_age.csv")%>%
  filter(age == "20--64") %>%
  select(-age)%>%
  mutate (highLS=highLS/100)
#write.csv(d3, "./data/df_LS_cc_broad_age.csv", row.names = F)

d4 <- read.csv("./data/df_cc_broad_age_prop.csv") %>% 
  select(iso, prop_19, prop_20_64, prop_65)

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

d8 <- read.csv("./data/wicdf_dep.csv", sep=";")%>%
  rename(cc=country)

d9 <- read.csv("./data/air.csv")
  #mutate(illiterate_prop=illiterate_prop/100)
#write.csv(d9, "./data/air.csv", row.names = F)

d10 <- read.csv("./wic_educ.csv")%>% 
  select(iso, cc, above_sec, bach_n_higher, master_n_above)%>% 
  mutate(above_sec=above_sec/100,
         bach_n_higher=bach_n_higher/100,
         master_n_above=master_n_above/100)
#write.csv(d10, "./data/wic_educ.csv", row.names = F)

d11 <- read.csv("./data/wic_sex_ratio.csv", sep=";")

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d9) %>% 
  left_join(d6) %>% 
  left_join(d7) %>%
  left_join(d8) %>%
  left_join(d10) %>%
  left_join(d11)%>%
  mutate(illiterate_prop=ifelse(is.na(illiterate_prop), 0.013, illiterate_prop),#manually enter illiterate proportion for channel islands as 1.13 source: uis
          qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys))
r1 <- r1[-c(199),] #removing world

write.csv(r1, "./data/piaac_step_dhs_adj_qamys.csv", row.names = F)
  
r1_temp <- r1 %>% 
  filter(!is.na(qamys))

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
  geom_smooth() +
  ggtitle("Adjustment factor vs Illiterate proportion") +
  xlab("Illiterate proportion") +
  ylab("Adjustment factor")

p3 <- ggplot(r1_temp, aes(x = illiterate_prop, y = log(adj_factor))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs Illiterate proportion") +
  xlab("Illiterate proportion") +
  ylab("Log Adjustment factor")

p4 <- ggplot(r1_temp, aes(x = highLS, y = adj_factor)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion with higher than Lower secondary education") +
  xlab("Lower secondary +") +
  ylab("Adjustment factor")

p5 <- ggplot(r1_temp, aes(x = highLS, y = log(adj_factor))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion with higher than Lower secondary education") +
  xlab("Lower secondary +") +
  ylab("Log Adjustment factor")

p6 <- ggplot(r1_temp, aes(x = old_dep, y = adj_factor)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion of 0-19 population") +
  xlab("Old dependency ratio") +
  ylab("Adjustment factor")

p7 <- ggplot(r1_temp, aes(x = old_dep, y = log(adj_factor))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs Proportion of 0-19 population") +
  xlab("Old dependency ratio") +
  ylab("Log Adjustment factor")

p8 <- ggplot(r1_temp, aes(x = hlo_mean, y = adj_factor)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs HLO") +
  xlab("HLO") +
  ylab("Adjustment factor")

p9 <- ggplot(r1_temp, aes(x = hlo_mean, y = log(adj_factor))) +
  geom_point() +
  geom_smooth() +
  ggtitle("Adjustment factor vs HLO") +
  xlab("HLO") +
  ylab("Log Adjustment factor")

pdf("./figures/Scatterplot_adj_factor_log2.pdf")
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9)
dev.off()

r2 <- r1 %>% 
  filter(!is.na(qamys), 
         !is.na(hlo_mean))

library(MASS)
library(olsrr)
#model1 with age groups, 0.98 fit bu multicollinearity
model1<-lm(log(qamys) ~  log(wic_mys) + highLS + prop_19 + prop_20_64 + illiterate_prop, data = r1)
summary(model1)
ols_vif_tol(model1)

#model2 with old dependency similar 0.98 fit bu multicollinearity
model2<-lm(log(qamys) ~  log(wic_mys) + highLS + old_dep + illiterate_prop, data = r1)
summary(model2)
ols_vif_tol(model2)

#model3 adj_factor as y  0.89 fit and still multicollinearity 
model3<-lm(log(adj_factor) ~  log(wic_mys) + highLS + old_dep + illiterate_prop, data = r1)
summary(model3)
ols_vif_tol(model3)

#model4 adj factor as y 0.89 fit multicollinearity vanishes
model4<-lm(log(adj_factor) ~  highLS + old_dep + youth_dep + illiterate_prop, data = r1)
summary(model4)
ols_vif_tol(model4)

# Stepwise regression model
step.model <- stepAIC(model4, direction = "both", 
                      trace = FALSE)
summary(step.model)
#highLS is dropped. other educational attainment level variables also give similar results
sink("./results/adj_est.txt")
print(summary(step.model))
sink()

lm2 <- lm(log(adj_factor) ~ old_dep + illiterate_prop, data = r1)
lm2predict <- as.data.frame(predict(lm2, newdata = r1, interval = "confidence")) 
colnames(lm2predict) <- paste0(colnames(lm2predict),2)

df1 <- r1 %>% 
  bind_cols(lm2predict) %>%
  mutate(fit2=wic_mys*exp(fit2),
         upr2=wic_mys*exp(upr2),
         lwr2=wic_mys*exp(lwr2),
         plot = case_when(wic_mys > 10.85 ~ 1,
                          wic_mys <= 10.85 & wic_mys >= 8.5 ~ 2, 
                          wic_mys < 8.5 ~ 3))%>%
        mutate(qamys_pred = qamys, 
        qamys_pred = ifelse(is.na(qamys_pred), fit2, qamys_pred))
summary(df1$qamys_pred) #min 0.3 max 15.59
write.csv(df1, "./results/results_adj_est.csv", row.names = F)

png("./figures/piaac_step_dhs_full_corrected_fit_adj_fac.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = qamys, y = fit2)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  xlab("Empirical QAMYS") +
  ylab("Estimated QAMYS") +
  theme(legend.title = element_blank())+
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

res<-cor.test(df1$qamys, df1$fit2, method="pearson")
res

p1 <- ggplot(df1 %>% filter(!is.na(fit2), plot == 1), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()

p2 <- ggplot(df1 %>% filter(!is.na(fit2), plot == 2), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()

p3 <- ggplot(df1 %>% filter(!is.na(fit2), plot == 3), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  ylim(-2.5, 17.5) +
  coord_flip() +
  theme_bw()

p4 <- ggplot(df1 %>% filter(!is.na(fit2)), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
  geom_bar(stat = "identity", alpha = 0.25) +
  geom_point(aes(x = cc, y = fit2)) +
  geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
  xlab("Country") +
  ylab("WIC MYS & QAMYS (95% CI)") +
  coord_flip() +
  theme_bw()


png("./figures/adj_est_predicted_mys1.png", res = 300, unit = "in", height = 10, width = 10)
print(p1)
dev.off()

png("./figures/adj_est_predicted_mys2.png", res = 300, unit = "in", height = 10, width = 10)
print(p2)
dev.off()

png("./figures/adj_est_predicted_mys3.png", res = 300, unit = "in", height = 10, width = 10)
print(p3)
dev.off()