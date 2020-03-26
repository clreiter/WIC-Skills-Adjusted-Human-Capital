rm(list = ls())

library(tidyverse)
# library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
# library(VGAM)#for tobit censor
library(fastDummies)

#estimates with dependency ratios
d1 <- read.csv("./data/qamys_1970-2015.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)

d2 <- read.csv("./data/wic_dep_1970_2015.csv")

d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

d4 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

d5 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")

d6 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")


r1 <- d4 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys))%>% 
  dummy_cols(select_columns = "year")
# Check DHS correction used above 

r2 <- r1 %>%
  filter(iso %in% c(68, 288, 404, 604), year == 2015) %>%
  mutate(dhs_corr_full = qamys_piaac/qamys_dhs_full, dhs_corr_part = qamys_piaac/qamys_dhs_part)

mean(r2$dhs_corr_full, na.rm = T)

r1_temp <- r1 %>% 
  filter(!is.na(qamys))

r3 <- r1 %>% 
  filter(!is.na(qamys))

library(MASS)
library(olsrr)
#model without age groups
model1 <- lm (log(qamys) ~ log(wic_mys) + illiterate_prop + highLS+ year_1970 + year_1975 + year_1980 +
                year_1985 + year_1990 + year_1995  + year_2000 + year_2005 + 
                year_2010, data = r1)
summary(model1) #r2=0.97
ols_vif_tol(model1) #multicollinearity

#model with old dependency ratio
model2 <- lm(log(qamys) ~ log(wic_mys) + illiterate_prop + highLS+ old_dep + year_1970 + year_1975 + year_1980 +
               year_1985 + year_1990 + year_1995  + year_2000 + year_2005 + 
               year_2010, data = r1)
summary(model2) #r2=0.97, highLS insignificant
ols_vif_tol(model2) # multicollinearity

#model without wic_mys
model3 <- lm( log(qamys) ~ illiterate_prop + highLS+ old_dep + year_1970 + year_1975 + year_1980 +
                year_1985 + year_1990 + year_1995  + year_2000 + year_2005 + 
                year_2010, data = r1)
summary(model3) #r2=0.90
ols_vif_tol(model3) # multicollinearity


# Stepwise regression model
step.model <- stepAIC(model3, direction = "both", 
                      trace = FALSE)
summary(step.model) #r2=0.90 years after 1990 are dropped
ols_vif_tol(step.model) #no multicollinearity

lm4 <- lm(log(qamys) ~  log(wic_mys) + illiterate_prop + highLS+ old_dep +year_1970 + year_1975 + year_1980 +
            year_1985 + year_1990, data = r1)
summary(lm4)


lm4predict <- as.data.frame(predict(lm4, newdata = r1, interval = "confidence")) 
colnames(lm4predict) <- paste0(colnames(lm4predict), 4)

# OLS assumptions 

par(mfrow = c(2,2))
png("./results/lm4_with_old_dep_modelcheck.png")
dev.off()
# lmtest::bptest(lm4) 
# studentized Breusch-Pagan test
# 
# data:  lm4
# BP = 128.98, df = 9, p-value < 2.2e-16
# p-value < 0.5 heteroscedasticity exsist

par(mfrow = c(1,1))
acf(lm4$residuals)
ccf(r1$illiterate_prop, r1$old_dep)

df1 <- r1 %>% 
  bind_cols(lm4predict) %>% 
  mutate(fit4 = exp(fit4), 
         lwr4 = exp(lwr4), 
         upr4 = exp(upr4)) %>%
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit4, qamys_pred))


detach("package:MASS", unload=TRUE)

cc_grp <- df1 %>% 
  filter(year == 2015) %>% 
  mutate(plot = case_when((wic_mys > 10.85) ~ 1,
                          (wic_mys <= 10.85 & wic_mys >= 8.5) ~ 2, 
                          (wic_mys < 8.5) ~ 3)) %>% 
  select(iso, plot)

df2 <- df1 %>%  
  left_join(cc_grp)

write.csv(df2, "./results/results_with_old_dep_1970_2015.csv", row.names = F)


png("./figures/fit_lm4_old_dep.png", width = 5, height = 5, res = 300, units = "in")

ggplot(df1, aes(x = qamys, y = fit4)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank()) +
  ylab("Predicted QAMYS") +
  xlab("Empirical QAMYS") + 
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()




pdf("./figures/mys_qamys_1970_2015_plot1_old_dep.pdf")

for(i in seq(unique(df2$year))){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit4), plot == 1, year == unique(df2$year)[i]), 
                aes(x = reorder(country, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = country, y = fit4)) +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
    xlab("Country") +
    ylab("WIC MYS (bars) & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()


pdf("./figures/mys_qamys_1970_2015_plot2_old_dep.pdf")

for(i in seq(unique(df2$year))){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit4), plot == 2, year == unique(df2$year)[i]), aes(x = reorder(country, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = country, y = fit4)) +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()



pdf("./figures/mys_qamys_1970_2015_plot3_old_dep.pdf")

for(i in seq(unique(df2$year))){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit4), plot == 3, year == unique(df2$year)[i]), aes(x = reorder(country, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = country, y = fit4)) +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
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
  plot_name <- paste0("./figures/mys_qamys_old_dep",unique(df2$year)[i], ".png")
  
  png(plot_name, width = 10, height = 20, res = 300, units = "in")
  pp <- ggplot(df2 %>% filter(!is.na(fit4), year == unique(df2$year)[i]), aes(x = reorder(country, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = country, y = fit4)) +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
    xlab("Country") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
  dev.off() 
}

pdf("./figures/mys_qamys_1970_2015_old_dep.pdf")


for(i in seq(1, length(unique(df2$country)), 4)){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit4), country %in% unique(df2$country)[i: (i+3)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit4)) +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
    xlab("Year") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    theme_bw() +
    facet_wrap(~ country, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()
