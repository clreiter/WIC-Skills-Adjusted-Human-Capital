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
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys),
         adj_fact_dhs = qamys/wic_mys,
         adj_factor = adj_fact_dhs) %>% 
  dummy_cols(select_columns = "year")

r1<-r1[!(r1$iso=="900"),] #removing world
 
library(MASS)
library(olsrr)
model1 <-lm(log(adj_factor) ~  highLS + illiterate_prop + old_dep + 
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model1)
ols_vif_tol(model1)

model2 <-lm(log(adj_factor) ~  highLS + illiterate_prop + hlo +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model2)
ols_vif_tol(model2)

model3 <-lm(log(adj_factor) ~  highLS + illiterate_prop + tp_ +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model3)
ols_vif_tol(model3)

model4 <-lm(log(adj_factor) ~  highLS + illiterate_prop + edu_exp_ +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model4)
ols_vif_tol(model4)

model5 <-lm(log(adj_factor) ~  highLS + illiterate_prop + old_dep + hlo +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010, data = r1)
summ(model5)
ols_vif_tol(model5)

#install.packages("sjPlot")
library(sjPlot)
tab_model(model1, model2, model3, model4, model5,
          show.se = TRUE, show.ci = FALSE,
          collapse.se = TRUE)


# Stepwise regression model
step.model <- stepAIC(model5, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(log(adj_factor) ~  highLS + illiterate_prop + hlo + old_dep +
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

sink("./results/adj_fac_est_hlo_1970_2015.txt")
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
  mutate(adj_fit = exp(fit1), 
         adj_lwr1 = exp(lwr1), 
         adj_upr1 = exp(upr1),
         fit1 = exp(fit1)*wic_mys, 
         lwr1 = exp(lwr1)*wic_mys, 
         upr1 = exp(upr1)*wic_mys) %>%
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit1, qamys_pred))
summary(df1$qamys_pred) #min=0.003 max=15.605
write.csv(df1, "./results/fit1.csv", row.names = F)

detach("package:MASS", unload=TRUE)

png("./figures/estimated_empirical_samys.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = qamys, y = fit1)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  xlab("Empirical SAMYS") +
  ylab("Estimated SAMYS") +
  theme(legend.title = element_blank())+
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

res <- cor.test(df1$qamys, df1$fit1, method="pearson")
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
  dplyr::select(iso, plot)

df2 <- df1 %>%  
  left_join(cc_grp)

write.csv(df2, "./results/results_adj_fac_hlo_1970_2015_with_adj_fct.csv", row.names = F)

png("./figures/fit_adj_fac_1970_2015.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = qamys_piaac, y = fit1)) +
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
    ylim(-0.5, 17.5) +
    ggtitle(paste(unique(df2$year)[i])) +
    coord_flip() +
    theme_bw()
  print(pp)
  dev.off() 
}

pdf("./figures/mys_samys_1970_2015_countries.pdf")
for(i in seq(1, length(unique(df2$cc)), 4)){
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit1), cc %in% unique(df2$cc)[i: (i+ 3)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Year") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 2) +
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
    ylim(-0.5, 17.5) +
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
