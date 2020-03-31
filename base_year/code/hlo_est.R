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

r1 <- d2 %>% 
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d9) %>% 
  left_join(d6) %>% 
  left_join(d7) %>%
  left_join(d8) %>%
  left_join(d10) %>%
  mutate(illiterate_prop=ifelse(is.na(illiterate_prop), 0.013, illiterate_prop),#manually enter illiterate proportion for channel islands as 1.13 source: uis
         qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys))
r1 <- r1[-c(199),] #removing world

r2 <- r1 %>% 
  filter(!is.na(qamys), 
         !is.na(hlo_mean))

lm3 <- lm(log(adj_factor) ~  hlo_mean + old_dep + illiterate_prop, data = r2)
summ(lm3)
sink("./results/adj_est_hlo.txt")
print(summary(lm3))
sink()

lm3predict <- as.data.frame(predict(lm3, newdata = r2, interval = "confidence")) 
colnames(lm3predict) <- paste0(colnames(lm3predict),3)

df3 <- r2 %>% 
  bind_cols(lm3predict) %>%
  mutate(fit3=wic_mys*exp(fit3),
         upr3=wic_mys*exp(upr3),
         lwr3=wic_mys*exp(lwr3),
         plot = case_when(wic_mys > 10.85 ~ 1,
                          wic_mys <= 10.85 & wic_mys >= 8.5 ~ 2, 
                          wic_mys < 8.5 ~ 3))
df4 <- df3 %>% 
  mutate(qamys_pred = qamys, 
         qamys_pred = ifelse(is.na(qamys_pred), fit3, qamys_pred))
summary(df4$qamys_pred) #min 0.3 max 15.59

png("./figures/piaac_step_dhs_full_corrected_fit_adj_fac_hlo.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df4, aes(x = qamys, y = fit3)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  xlab("Empirical QAMYS") +
  ylab("Estimated QAMYS") +
  theme(legend.title = element_blank())+
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

res<-cor.test(df4$qamys, df4$fit3, method="pearson")
res