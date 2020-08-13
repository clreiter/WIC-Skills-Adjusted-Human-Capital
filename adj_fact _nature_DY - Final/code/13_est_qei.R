rm(list = ls())

library(tidyverse)
library(countrycode)
library(fastDummies)

# Countries in the WIC dataset
d1 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>%
  select(year, iso)

# GDSEQ/HLO scores
d2 <- read.csv("./data/hlo_empirical_1970_2015.csv")

# Government expenditure on education as a percentage of GDP (%) from UIS
d3 <- read.csv("./data/uis_edu_exp.csv")

# Teacher-Pupil ratio from UIS
d4 <- read.csv("./data/uis_tp.csv")

d5 <- d2%>%
  left_join(d1) %>% 
  left_join(d3) %>% 
  left_join(d4) %>% 
  dummy_cols(select_columns = "year")

#estimation of HLO 
lm1 <- lm(hlo_mean ~ GEO + tp_ + edu_exp_, data = d5)
summary(lm1)

library(MASS)
step.model <- stepAIC(lm1, direction = "both", 
                      trace = FALSE)
summary(step.model)

#REGRESSION DIAGNOSTICS
#Normality
library(car)
qqPlot(lm1, main="QQ Plot")
sresid <-studres(lm1)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals") 
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
      
#independence
plot(x=lm1$model$tp_, y=resid(lm1), xlab = "Pupil-teacher ratio", ylab = "Residuals")
plot(x=lm1$model$edu_exp_, y=resid(lm1), xlab = "Educational expenditure", ylab = "Residuals")
#residuals and predictors are independent

#Homoscedasticity
lmtest::bptest(lm1) 
#studentized Breusch-Pagan test p-value < 0.05 heteroscedasticity exists
par(mfrow = c(2,2))
plot(lm1)

#Multicollinearity
library(olsrr)
ols_vif_tol(lm1) 

detach("package:MASS", unload = TRUE)

lm1predict <- as.data.frame(predict(step.model, newdata = d5, interval = "confidence"))
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

d6 <- bind_cols(d5, lm1predict) %>%
                mutate(hlo = fit1)

#recoding values of 1970 to older periods
d7<-d6[!(d6$year=="1995" | d6$year=="2000" |d6$year=="2005" |
           d6$year=="2010" |d6$year=="2015"),] %>%
  select(-year_1970, -year_1975, -year_1980,-year_1985, -year_1990, 
         -year_1995, -year_2000, -year_2005, -year_2010, -year_2015)

d8<-d7[(d7$year=="1970"),]

d1945 <- d8 %>%
  mutate(year = year-25)
 
d1950 <- d8 %>%
  mutate(year = year-20)

d1955 <- d8 %>%
  mutate(year = year-15)

d1960 <- d8 %>%
  mutate(year = year-10)

d1965 <- d8 %>%
  mutate(year = year-5)

d9 <- d7 %>%
  rbind(d1945) %>% 
  rbind(d1950) %>% 
  rbind(d1955) %>% 
  rbind(d1960) %>% 
  rbind(d1965) %>% 
  dummy_cols(select_columns = "year")

write.csv(d9, "./data/qei_1945_2015.csv", row.names = F)

