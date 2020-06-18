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
# Data preparation: prep_mys_cc
d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

# WIC Higher than Lower Secondary
# Data preperation: prep_LS_cc
d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)%>%
  mutate (highLS=highLS/100)


d4 <- read.csv("./data/df_cc_broad_age_prop_1970_2015.csv") %>% 
  select(cc, iso, year, prop_19, prop_20_64, prop_65)

# Illiterate population 
# See XYZ for STATA imputation
d5 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv")%>%
  mutate (illiterate_prop=illiterate_prop/100)

# DHS literracy test can read full/partial sentence
d6 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")

# WIC young, old and total dependency
d7 <- read.csv("./data/wic_dep_1970_2015.csv")%>%
  mutate(cc=country)


d8 <- read.csv("./data/uis_edu_exp.csv")%>%
            select(-cc)

d9 <- read.csv("./data/uis_tp.csv")%>%
  select(-cc)

d10 <- read.csv("./data/r2_hlo_1970_2015.csv")%>%
  select(-cc)%>%
  mutate(year=year+25)

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

#removing world average and countries with missing MYS data
r1<-r1[!(r1$iso=="900"|r1$iso=="28"|r1$iso=="52"|r1$iso=="96"|r1$iso=="830"| +
           r1$iso=="262"|r1$iso=="232"|r1$iso=="308"|r1$iso=="316"|r1$iso=="434"| +
           r1$iso=="478"|r1$iso=="175"|r1$iso=="598"|r1$iso=="690"|r1$iso=="850"| +
           r1$iso=="860"|r1$iso=="732"),]

library(MASS)
library(olsrr)
model5 <-lm(log(adj_factor) ~  highLS + illiterate_prop + 
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 +
              year_1995 + year_2000 + year_2005 + year_2010 +
              old_dep + hlo, data = r1)
summ(model5)
ols_vif_tol(model5)

# Stepwise regression model
step.model <- stepAIC(model5, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(log(adj_factor) ~  highLS + illiterate_prop + hlo + old_dep, data = r1)

sink("./results/adj_fac_est_hlo_lagged_1970_2015.txt")
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
  mutate(qamys_pred = qamys,
         qamys_pred = ifelse(is.na(qamys_pred), fit1, qamys_pred))
summary(df1$qamys_pred) #min=0.003 max=15.605
write.csv(df1, "./results/fit1_lagged.csv", row.names = F)

detach("package:MASS", unload=TRUE)

png("./figures/estimated_empirical_samys_lagged.png", width = 5, height = 5, res = 300, units = "in")
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

res<-cor.test(df1$qamys, df1$fit1, method="pearson")
res

df1 <- df1[order(df1$cc, df1$year),]
pdf("./figures/mys_samys_1995_2015_countries_lagged.pdf")
for(i in seq(1, length(unique(df1$cc)), 25)){
  pp <-  ggplot(df1 %>% filter(!is.na(fit1), cc %in% unique(df1$cc)[i: (i+ 24)]), 
                aes(x = year, y = wic_mys)) +
    geom_bar(stat = "identity", alpha = 0.25) +
    geom_point(aes(x = year, y = fit1)) +
    geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
    xlab("Year") +
    ylab("WIC MYS & SAMYS (95% CI)") +
    ylim(-0.1, 17.6) +
    theme_bw() +
    facet_wrap(~ cc, nrow = 5, ncol = 5) +
    theme(axis.text.x = element_text(angle = 90))
  print(pp)
}
dev.off()

