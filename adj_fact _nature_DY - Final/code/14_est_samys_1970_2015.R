rm(list = ls())

library(tidyverse)
library(jtools)
library(countrycode)
library(fastDummies)

# SAMYS scores based on PIAAC/STEP for 44 countries 
d1 <- read.csv("./data/samys_1970-2015_rev.csv") %>% 
  filter(age == 0 & sex == 0 & educ == 0) %>% 
  rename(qamys_piaac = qamys) %>% 
  select(iso, year, adj_factor, qamys_piaac)

# WIC MYS data
# Data prepared in ./code/prep_mys_cc.R
d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc)

# Proportion of population at least secondary level education
# Data prepared in ./code/prep_LS_cc.R
d3 <- read.csv("./data/df_LS_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age, -cc) %>%
  mutate (highLS = highLS/100)

# Proportion of illiterate population
# Data prepared in several steps in prep_literacy_ files
d4 <- read.csv("./data/df_illiterate_prop_cc_1970_2015.csv") %>%
  mutate (illiterate_prop = illiterate_prop/100)

# SAMYS scores based on DHS
# Data prepared in prep_qamys_dhs.R
d5 <- read.csv("./data/qamys_dhs_full_part_2000_2015.csv")

# WIC population dependency ratio data
# Data prepared in prep_dep
d6 <- read.csv("./data/df_old_age_dep_1970_2015.csv")

# QEI scores based on hlo estimation using region, edu. exp. teach./pup. ratio 
# Estimates calculated in ./code/qei_estimation.R
d7 <- read.csv("./data/qei_1945_1990.csv", sep = ";") %>%
              select(year, iso, hlo) %>%
              mutate(year = year + 25)

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

# removing world average and countries with missing MYS data
r1 <- r1[!(r1$iso=="900"|r1$iso=="28"|r1$iso=="52"|r1$iso=="96"|r1$iso=="830"| +
         r1$iso=="262"|r1$iso=="232"|r1$iso=="308"|r1$iso=="316"|r1$iso=="434"| +
         r1$iso=="478"|r1$iso=="175"|r1$iso=="598"|r1$iso=="690"|r1$iso=="850"| +
         r1$iso=="860"|r1$iso=="732"),]

library(MASS)
library(olsrr)

# estimation model for skills adjustment factor
model1 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
              year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + 
              year_1995 + year_2000 + year_2005 + year_2010, data = r1)
summ(model1)
# no multicollinearity
ols_vif_tol(model1) 

# Stepwise regression model
step.model <- stepAIC(model1, direction = "both", 
                      trace = FALSE)
summary(step.model)

lm1 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

#REGRESSION DIAGNOSTICS

#Normality
library(car)
par(mfrow = c(3,2))
qqPlot(lm1, main="QQ Plot")

#histogram for distribution of residuals
sresid <-studres(lm1)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals") 
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
#residual distribution seems normal

#Plots bw each predictor and residuals
par(mfrow = c(2,2))
plot(x=lm1$model$highLS, y=resid(lm1), xlab = "aboveLS", ylab = "Residuals")
plot(x=lm1$model$old_dep, y=resid(lm1), xlab = "ODR", ylab = "Residuals")
plot(x=lm1$model$illiterate_prop, y=resid(lm1), xlab = "AIR", ylab = "Residuals")
plot(x=lm1$model$highLS, y=resid(lm1), xlab = "QEI", ylab = "Residuals")
#residuals and predictors are independent

#Homoscedasticity
lmtest::bptest(lm1) #studentized Breusch-Pagan test p-value < 0.05 heteroskedasticity exists
par(mfrow = c(2,2))
plot(lm1) #the left hand side graphs show that thre might be some degree of heteroskedasticity

#calculate robust standard errors
#install.packages('sandwich')
library(sandwich)
library(lmtest)
lm1 %>% 
  vcovHC() %>% 
  diag() %>% 
  sqrt()
coeftest(lm1, vcov = vcovHC(lm1,))
#standard errors are higher for some of the coefficients but significance is pretty much the same

#Multicollinearity
library(olsrr)
ols_vif_tol(lm1)
#no multicorrelation. All VIF scores are less than 5

#Linearity
crPlots(lm1, terms = ~.- year_1970 -year_1975 -year_1980 -year_1985 -year_1990)
#all predictors seems to be linear

#End of diagnostic tests


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
         samys_pred = ifelse(is.na(samys_pred), fit1, samys_pred)) %>% 
  arrange(cc, year)

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

# Save SAMYS dataset

df2 <- df1 %>%
  select(cc, year, iso, wic_mys, samys_pred)

write.csv(df2, "./results/samys_1970_2015.csv", row.names = F)

