rm(list = ls())

library(tidyverse)
library(countrycode)
library(fastDummies)

#countries in the WIC dataset
d1 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv")%>%
  select(year, iso)
#GDSEQ/HLO scores
d2 <- read.csv("./data/hlo_empirical_1970_2015.csv")
#Government expenditure on education as a percentage of GDP (%) from UIS
d3 <- read.csv("./data/uis_edu_exp.csv")
#Teacher-Pupil ratio from UIS
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
detach("package:MASS", unload = TRUE)

lm1predict <- as.data.frame(predict(step.model, newdata = d5, interval = "confidence"))
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

d6 <- bind_cols(d5, lm1predict)%>%
                mutate(hlo = fit1)
d7<-d6[!(d6$year=="1995" | d6$year=="2000" |d6$year=="2005" |
           d6$year=="2010" |d6$year=="2015"),]

write.csv(d7, "./data/qei_1995_2015.csv", row.names = F)
#values for 1970 was manually copied to periods from 1945 to 1965 to be used in SAMYS estimates
