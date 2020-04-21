rm(list = ls())

library(tidyverse)
# library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
# library(VGAM)#for tobit censor
library(fastDummies)

d1 <- read.csv("./data/qamys_1970-2015.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)

d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)%>%
  mutate (highLS=highLS/100)

d4 <- read.csv("./data/df_cc_broad_age_prop_1970_2015.csv") %>% 
  select(cc, iso, year, prop_19, prop_20_64, prop_65)

d5 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")%>%
  mutate (illiterate_prop=illiterate_prop/100)

d6 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")

d7 <- read.csv("./data/wic_dep_1970_2015.csv")%>%
  mutate(cc=country)

d8 <- read.csv("./data/uis_edu_exp.csv")

d9 <- read.csv("./data/uis_tp.csv")

d10 <- read.csv("./data/hlo_1970_2015.csv")

d11 <- read.csv("./data/uis_c_exp.csv")

d12 <- read.csv("./data/tp_s.csv")

d13 <- read.csv("./data/piaac.csv")

d14 <- read.csv("./data/lm1_lm2_lm3_lm4.csv")%>% 
  rename(hlo1 = fit1)%>% 
  rename(hlo2 = fit2)%>% 
  rename(hlo3 = fit3)%>%
  rename(hlo4 = fit4)

d15 <- read.csv("./data/hlo_empirical_1970_2015.csv")

r1 <- d7 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>%
  left_join(d6) %>%
  left_join(d8) %>%
  left_join(d9) %>%
  left_join(d10) %>%
  left_join(d11) %>%
  left_join(d12) %>%
  left_join(d13) %>%
  left_join(d14) %>%
  left_join(d15) %>%
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys),
         adj_fact_dhs = qamys/wic_mys,
         adj_factor = adj_fact_dhs) %>% 
  dummy_cols(select_columns = "year")

r1<-r1[!(r1$iso=="900"),] #removing world
res<-cor.test(r1$hlo_mean, r1$piaac_est, method="pearson")
res

library(MASS)
library(olsrr)
model1 <- model1 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
                        year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
                        year_2000 + year_2005 + year_2010 + old_dep, data = r1)
summ(model1)

model2 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
                        year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
                        year_2000 + year_2005 + year_2010 + hlo, data = r1)
summ(model2)

model3 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + hlo1, data = r1)
summ(model3)

model4 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + hlo2, data = r1)
summ(model4)

model5 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + hlo3, data = r1)
summ(model5)

model6 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + hlo4, data = r1)
summ(model6)

model7 <-lm(log(adj_factor) ~  highLS + illiterate_prop +  
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + old_dep + hlo, data = r1)
summ(model7)

detach("package:MASS", unload = TRUE)

library(sjPlot)
tab_model(model1, model2, model3, model4, model5, model6, model7,
          show.se = TRUE, show.ci = FALSE,
          collapse.se = TRUE)
library(olsrr)
ols_vif_tol(model7)

r1$samys1 <- predict.lm(model1, newdata=r1) 
z1<-r1 %>% 
    mutate(samys1 = exp(samys1)*wic_mys) %>% 
    select(iso, cc, year, samys1)

r1$samys2 <- predict.lm(model2, newdata=r1)
z2<-r1 %>% 
  mutate(samys2 = exp(samys2)*wic_mys)%>% 
  select(iso, cc, year, samys2)

r1$samys3 <- predict.lm(model3, newdata=r1)
z3<-r1 %>% 
  mutate(samys3 = exp(samys3)*wic_mys)%>% 
  select(iso, cc, year, samys3)

r1$samys4 <- predict.lm(model4, newdata=r1)
z4<-r1 %>% 
  mutate(samys4 = exp(samys4)*wic_mys)%>% 
  select(iso, cc, year, samys4)

r1$samys5 <- predict.lm(model5, newdata=r1)
z5<-r1 %>% 
  mutate(samys5 = exp(samys5)*wic_mys)%>% 
  select(iso, cc, year, samys5)

r1$samys6 <- predict.lm(model6, newdata=r1)
z6<-r1 %>% 
  mutate(samys6 = exp(samys6)*wic_mys)%>% 
  select(iso, cc, year, samys6)

r1$samys7 <- predict.lm(model7, newdata=r1)
z7<-r1 %>% 
  mutate(samys7 = exp(samys7)*wic_mys)%>% 
  select(iso, cc, year, samys7)

r2<-z1%>% 
  left_join(z2)%>% 
  left_join(z3)%>% 
  left_join(z4)%>% 
  left_join(z5)%>% 
  left_join(z6)%>% 
  left_join(z7)

p<-ggplot(r2, aes(x = samys1, y = samys2)) +
  geom_point() +
  geom_abline()
p

library(gridExtra)
sc_plots = list()
sc_plots$sc1 = ggplot(r2, aes(x = samys2, y = samys1)) +
  geom_point() + geom_abline() +
  ylab("M w/ old_dep") +
  xlab("M w/ basic HLO imp.") +
  theme_bw()
sc_plots$sc2 = ggplot(r2, aes(x = samys3, y = samys1)) +
  geom_point() + geom_abline() +
  ylab("M w/ old_dep") +
  xlab("M w/ HLO imp. model 1") +
  theme_bw()
sc_plots$sc3 = ggplot(r2, aes(x = samys4, y = samys1)) +
  geom_point() + geom_abline() +
  ylab("M w/ old_dep") +
  xlab("M w/ HLO imp. model 2") +
  theme_bw()
sc_plots$sc4 = ggplot(r2, aes(x = samys4, y = samys1)) +
  geom_point() + geom_abline() +
  ylab("M w/ old_dep") +
  xlab("M w/ HLO imp. model 3") +
  theme_bw()
sc_plots$sc5 = ggplot(r2, aes(x = samys5, y = samys1)) +
  geom_point() + geom_abline() +
  ylab("M w/ old_dep") +
  xlab("M w/ HLO imp. model 4") +
  theme_bw()
sc_plots$sc6 = ggplot(r2, aes(x = samys6, y = samys1)) +
  geom_point() + geom_abline()+
  ylab("M w/ old_dep") +
  xlab("M w/ old_dep and basic HLO imp.") +
  theme_bw()
grid.arrange(sc_plots$sc1, sc_plots$sc2, sc_plots$sc3,
             sc_plots$sc4, sc_plots$sc5, sc_plots$sc6,
             ncol = 3)