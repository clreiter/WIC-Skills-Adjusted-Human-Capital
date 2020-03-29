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

r1 <- d7 %>% 
  left_join(d1) %>% 
  left_join(d2) %>% 
  left_join(d3) %>% 
  left_join(d5) %>% 
  left_join(d6) %>% 
  mutate(qamys = qamys_piaac, 
         qamys = ifelse(is.na(qamys), (qamys_dhs_full * 0.80), qamys)) %>% 
  dummy_cols(select_columns = "year")

r1<-r1[!(r1$iso=="900"),] #removing world
 
library(MASS)
library(olsrr)
model1 <-lm(log(adj_factor) ~  log(wic_mys) + highLS + illiterate_prop + old_dep + youth_dep + 
          year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
          year_2000 + year_2005 + year_2010 + year_2015, data = r1)
summ(model1)
ols_vif_tol(model1) #r2=0,78 but high multicollinearity

model2 <-lm(log(adj_factor) ~  highLS + illiterate_prop + old_dep + 
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
              year_2000 + year_2005 + year_2010 + year_2015, data = r1)
summ(model2)
ols_vif_tol(model2) #r2=0,73 multicollinearity reduced, youth_dep is also removed due to multicollinearity

# Stepwise regression model
step.model <- stepAIC(model2, direction = "both", 
                      trace = FALSE)
summary(step.model) #r2=0.73

lm2 <- lm(log(adj_factor) ~  highLS + illiterate_prop + old_dep + year_1970 + 
            year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
            year_2000, data = r1)

sink("./results/adj_fac_est_1970_2015.txt")
print(summary(lm2))
sink()

lm2predict <- as.data.frame(predict(lm2, newdata = r1, interval = "confidence")) 
colnames(lm2predict) <- paste0(colnames(lm2predict),2)

#lmtest::bptest(lm2) 
#studentized Breusch-Pagan test p-value < 0.5 heteroscedasticity exists

df1 <- r1 %>% 
  bind_cols(lm2predict) %>% 
  mutate(fit2 = exp(fit2)*wic_mys, 
         lwr2 = exp(lwr2)*wic_mys, 
         upr2 = exp(upr2)*wic_mys) %>%
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit2, qamys_pred))
summary(df1$qamys_pred) #min=0.003 max=15.605
detach("package:MASS", unload=TRUE)

cc_grp <- df1 %>% 
  filter(year == 2015) %>% 
  mutate(plot = case_when((wic_mys > 10.85) ~ 1,
                          (wic_mys <= 10.85 & wic_mys >= 8.5) ~ 2, 
                          (wic_mys < 8.5) ~ 3)) %>% 
  select(iso, plot)

df2 <- df1 %>%  
  left_join(cc_grp)

write.csv(df2, "./results/results_adj_fac_1970_2015.csv", row.names = F)

png("./figures/fit_adj_fac_1970_2015.png", width = 5, height = 5, res = 300, units = "in")
ggplot(df1, aes(x = qamys_piaac, y = fit2)) +
  geom_point(alpha = 0.6) +
  geom_abline() +
  theme(legend.title = element_blank()) +
  ylab("Predicted QAMYS") +
  xlab("Empirical QAMYS") + 
  xlim(-1, 16) +
  ylim(-1, 16) +
  theme_bw()
dev.off()

pdf("./figures/mys_qamys_1970_2015_plot11.pdf")
for(i in seq(unique(df2$year))){
    pp <-  ggplot(df2 %>% filter(!is.na(fit2), plot == 1, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit2)) +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
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
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit2), plot == 2, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit2)) +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
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
    pp <-  ggplot(df2 %>% filter(!is.na(fit2), plot == 3, year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit2)) +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
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
  pp <- ggplot(df2 %>% filter(!is.na(fit2), year == unique(df2$year)[i]), aes(x = reorder(cc, wic_mys), y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = cc, y = fit2)) +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
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
  
  pp <-  ggplot(df2 %>% filter(!is.na(fit2), cc %in% unique(df2$cc)[i: (i+ 3)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit2)) +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
    xlab("Year") +
    ylab("WIC MYS & QAMYS (95% CI)") +
    ylim(-0.5, 17.5) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 2) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()