rm(list = ls())

library(tidyverse)
library(jtools)
library(countrycode)
library(fastDummies)

#SAMYS scores based on PIAAC/STEP for 44 countries 
d1 <- read.csv("./data/samys_1970-2015_rev.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)
#WIC MYS data
d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)
#Proportion of population at least secondary level education
d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)%>%
  mutate (highLS=highLS/100)
#Proportion of illiterate population
d4 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")%>%
  mutate (illiterate_prop=illiterate_prop/100)
#SAMYS scores based on DHS
d5 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")
#WIC population dependency ratio data
d6 <- read.csv("./data/wic_dep_1970_2015.csv")%>%
  mutate(cc=country)
#QEI scores based on hlo estimation using region, edu. exp. teach./pup. ratio 
d7 <- read.csv("./data/qei_1945_1990.csv", sep = ";")%>%
              select(year, iso, hlo)%>%
              mutate(year=year+25)

r1 <- d6 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>%
  left_join(d7) %>%
  mutate(samys_piaac = qamys_piaac, 
         samys = samys_piaac, 
         samys = ifelse(is.na(samys), (qamys_dhs_full * 0.80), samys),
         adj_fact_dhs = samys/wic_mys,
         adj_factor = adj_fact_dhs) %>% 
  dummy_cols(select_columns = "year")

#removing world average and countries with missing MYS data
r1<-r1[!(r1$iso=="900"|r1$iso=="28"|r1$iso=="52"|r1$iso=="96"|r1$iso=="830"| +
         r1$iso=="262"|r1$iso=="232"|r1$iso=="308"|r1$iso=="316"|r1$iso=="434"| +
         r1$iso=="478"|r1$iso=="175"|r1$iso=="598"|r1$iso=="690"|r1$iso=="850"| +
         r1$iso=="860"|r1$iso=="732"),]

library(MASS)
library(olsrr)
#estimation model for skills adjustment factor
model1 <-lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + 
              year_1995 + year_2000 + year_2005 + year_2010, data = r1)
summ(model1)
ols_vif_tol(model1) #no multicollinearity

# Stepwise regression model
step.model <- stepAIC(model1, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

#sink("./results/samys_model_1970_2015.txt")
#print(summary(lm1))
#sink()

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

#adding SAMYS estimates to the dataset
df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(fit1 = exp(fit1)*wic_mys, 
         lwr1 = exp(lwr1)*wic_mys, 
         upr1 = exp(upr1)*wic_mys) %>%
  mutate(samys_pred = samys, 
         samys_pred = ifelse(is.na(samys_pred), fit1, samys_pred))

detach("package:MASS", unload=TRUE)
#country graphs for MYS and SAMYS
df1 <- df1[order(df1$cc, df1$year),]
pdf("./figures/countries_samys.pdf")
for(i in seq(1, length(unique(df1$cc)), 25)){
  pp <-  ggplot(df1 %>% filter(!is.na(samys_pred), cc %in% unique(df1$cc)[i: (i+ 24)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = samys_pred)) +
    xlab("Year") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.1, 17.5) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 5, ncol = 5) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()

#Saving SAMYS dataset
df2 <- df1%>%
  select(country, year, iso, wic_mys, samys_pred)
write.csv(df2, "./results/samys_1970_2015.csv", row.names = F)

#comparison of model without hlo
#model2 <-lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop +
#year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
#  year_2000 + year_2005 + year_2010, data = r1)
#summ(model2)
#ols_vif_tol(model2)

# Stepwise regression model
#step.model <- stepAIC(model2, direction = "both", 
#                      trace = FALSE)
#summary(step.model)

#lm2 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop +
#            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

#lm2predict <- as.data.frame(predict(lm2, newdata = r1, interval = "confidence")) 
#colnames(lm2predict) <- paste0(colnames(lm2predict),2)

#df1 <- r1 %>% 
#  bind_cols(lm1predict) %>% 
#  mutate(fit1 = exp(fit1)*wic_mys, 
#         lwr1 = exp(lwr1)*wic_mys, 
#         upr1 = exp(upr1)*wic_mys) %>%
#  bind_cols(lm2predict) %>% 
#  mutate(fit2 = exp(fit2)*wic_mys, 
#         lwr2 = exp(lwr2)*wic_mys, 
#         upr2 = exp(upr2)*wic_mys) %>%   
#  mutate(samys_pred1 = samys, 
#         samys_pred1 = ifelse(is.na(samys_pred1), fit1, samys_pred1))%>%   
#  mutate(samys_pred2 = samys, 
#         samys_pred2 = ifelse(is.na(samys_pred2), fit2, samys_pred2))
#
#detach("package:MASS", unload=TRUE)

#df1 <- df1[order(df1$cc, df1$year),]
#pdf("./figures/countries_samys_comparison.pdf")
#for(i in seq(1, length(unique(df1$cc)), 25)){
#  pp <-  ggplot(df1 %>% filter(!is.na(samys_pred2), cc %in% unique(df1$cc)[i: (i+ 24)]), 
#                aes(x = year, y = wic_mys)) +
#    geom_bar(stat = "identity", alpha = 0.25) +
#    geom_point(alpha=0.5, aes(x = year, y = samys_pred1)) +
#    geom_point(alpha=0.5, aes(x = year, y = samys_pred2), color="blue") +
#    xlab("Year") +
#    ylab("WIC MYS & SAMYS (95% CI)") +
#    ylim(-0.1, 17.5) +
#    theme_bw() +
#    facet_wrap(~ cc, nrow = 5, ncol = 5) +
#    theme(axis.text.x = element_text(angle = 90))
#  print(pp)
#}
# dev.off()