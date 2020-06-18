rm(list = ls())

library(tidyverse)
# library(gridExtra)
library(jtools)
# library(mctest)
library(countrycode)
# library(VGAM)#for tobit censor
library(fastDummies)

# PIAAC
# See XYZ for calculation
d1 <- read.csv("./data/samys_1970-2015_rev.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)

# WIC Mean Years of Schooling
d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

# WIC Higher than Lower Secondary
d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)%>%
  mutate (highLS=highLS/100)

# Illiterate population 
# See XYZ for STATA imputation
d4 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")%>%
  mutate (illiterate_prop=illiterate_prop/100)

# DHS 
# See code/prep_qamys_dhs for data preperation 
# Factors calculated in data/DHS-STEP-adjustment_calculations.csv 
d5 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")

# WIC young, old and total dependency 
d6 <- read.csv("./data/wic_dep_1970_2015.csv")%>%
  mutate(cc=country)

# 
d7 <- read.csv("./data/r2_hlo_1970_2015.csv")%>%
  select(-cc)

r1 <- d7 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  left_join(d5) %>%
  left_join(d6) %>%
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

model <-lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model)
ols_vif_tol(model)

# Stepwise regression model
step.model <- stepAIC(model, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

sink("./results/adj_fac_est_hlo_1970_2015_185_countries.txt")
print(summary(lm1))
sink()

lm1predict <- as.data.frame(predict(lm1, newdata = r1, interval = "confidence")) 
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

lmtest::bptest(lm1) 
#studentized Breusch-Pagan test p-value < 0.05 heteroscedasticity exists
par(mfrow = c(2,2))
plot(lm1)

df1 <- r1 %>% 
  bind_cols(lm1predict) %>% 
  mutate(fit1 = exp(fit1)*wic_mys, 
         lwr1 = exp(lwr1)*wic_mys, 
         upr1 = exp(upr1)*wic_mys) %>%
  mutate(samys_pred = samys,
         samys_pred = ifelse(is.na(samys_pred), fit1, samys_pred))
write.csv(df1, "./results/fit1.csv", row.names = F)

detach("package:MASS", unload=TRUE)

png("./figures/estimated_empirical_samys.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = samys, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  xlab("Empirical SAMYS") +
  ylab("Estimated SAMYS") +
  theme(legend.title = element_blank())+
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

res<-cor.test(df1$samys, df1$fit1, method="pearson")
res

png("./figures/mys_samys.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = wic_mys, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  xlab("Empirical SAMYS") +
  ylab("Estimated SAMYS") +
  theme(legend.title = element_blank())+
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

cc_grp <- df1 %>% 
  filter(year == 2015) %>% 
  mutate(plot = case_when((wic_mys > 10.85) ~ 1,
                          (wic_mys <= 10.85 & wic_mys >= 8.5) ~ 2, 
                          (wic_mys < 8.5) ~ 3)) %>% 
  select(iso, plot)

df2 <- df1 %>%  
  left_join(cc_grp)

write.csv(df2, "./results/results_adj_fac_hlo_1970_2015.csv", row.names = F)

png("./figures/fit_adj_fac_1970_2015.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = samys_piaac, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank()) +
  ylab("Predicted SAMYS") +
  xlab("Empirical SAMYS") + 
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

pdf("./figures/mys_samys_1970_2015_plot1.pdf")
for(i in seq(unique(df2$year))){
    pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 1, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()


pdf("./figures/mys_samys_1970_2015_plot2.pdf")
for(i in seq(unique(df2$year))){
    pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 2, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()

pdf("./figures/mys_samys_1970_2015_plot3.pdf")
for(i in seq(unique(df2$year))){
    pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 3, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()


for(i in seq(unique(df2$year))){
  plot_name <- paste0("./figures/mys_samys_",unique(df2$year)[i], "N.png")
  png(plot_name, width = 10, height = 20, res = 300, units = "in")
  pp <- ggplot(df2 %>% filter(!is.na(fit1), year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.1, 17.6) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
  dev.off() 
}

df2 <- df2[order(df2$cc, df2$year),]
pdf("./figures/mys_samys_1970_2015_countries.pdf")
for(i in seq(1, length(unique(df2$cc)), 25)){
    pp <-  ggplot(df2 %>% filter(!is.na(fit1), cc %in% unique(df2$cc)[i: (i+ 24)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Year") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.1, 17.5) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 5, ncol = 5) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()

#country graphs with fit rank
pdf("./figures/mys_samys_1970_2015_plot1_f.pdf")
for(i in seq(unique(df2$year))){
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 1, year == unique(df2$year)[i]), aes(x = reorder(cc, fit1), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 18.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()

pdf("./figures/mys_samys_1970_2015_plot2_f.pdf")
for(i in seq(unique(df2$year))){
    pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 2, year == unique(df2$year)[i]), aes(x = reorder(cc, fit1), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()

pdf("./figures/mys_samys_1970_2015_plot3_f.pdf")
for(i in seq(unique(df2$year))){
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), plot == 3, year == unique(df2$year)[i]), aes(x = reorder(cc, fit1), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Country") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
}
dev.off()
