rm(list = ls())
library(tidyverse)
library(countrycode)
library(zoo)
library(readxl)

d1 <- read.csv("./data/uis_edu_exp.csv")

d2 <-  read.csv("./data/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_935918.csv") %>% 
  select(-`Indicator.Name`, -`Indicator.Code`) %>% 
  rename(cc = `ï..Country.Name`, cc_code = `Country.Code`) %>% 
  gather(year, gdp, -c(cc, cc_code)) %>% 
  mutate(year = as.numeric(gsub("X", "", year))) %>% 
  filter(year >= 1970) %>% 
  group_by(cc, cc_code) %>% 
  summarise(gdp = mean(gdp, na.rm = T)) %>% 
  filter(!is.nan(gdp))
  
d3 <- read.csv("./data/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_935918.csv") %>% 
  select(-`Indicator.Name`, -`Indicator.Code`) %>% 
  rename(cc = `ï..Country.Name`, cc_code = `Country.Code`) %>% 
  gather(year, gdp, -c(cc, cc_code)) %>% 
  mutate(year = as.numeric(gsub("X", "", year))) %>% 
  filter(year >= 1970, 
         cc_code %in% d2$cc_code) %>% 
  mutate(year5 = cut(year, breaks = c(seq(1969, 2029, 5)), 
                     labels = seq(1970, 2025, 5)), 
         year = as.numeric(as.character(year5))) %>% 
  group_by(cc, cc_code, year) %>% 
  summarise(gdp = mean(gdp, na.rm = T)) %>% 
  mutate(iso = countrycode(cc_code, 'iso3c', 'iso3n')) %>% 
  filter(!is.na(iso)) %>% 
  group_by(cc) %>% 
  mutate(gdp = ifelse(is.nan(gdp), NA, gdp), 
         gdp2 = gdp, 
         gdp3 = na.spline(gdp)) 



d4 <- d3 %>% 
  ungroup() %>% 
  filter(iso == 4) %>% 
  select(iso, year, gdp) %>% 
  mutate(gdp = ifelse(is.nan(gdp), NA, gdp), 
         gdp2 = gdp) %>% 
  mutate(gdp2 = na.approx(gdp2, na.rm = F))
           
   

ggplot(d4, aes(x = year, y = gdp)) +
  geom_point() +
  geom_smooth()
        
n = 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through", n, "points"))
lines(na.spline(y))
lines(spline(x, y, n = 201), col = 2)

d4 <- d3 %>%  filter(iso == 4)
x <- d4$year
y <- d4$gdp

  
  fill(gdp2, .direction = "up") %>% 
  group_by(cc, cc_code, iso) 

d3$gdp3 <- mutate(na.spline(d3$gdp, na.rm = F))


d5 <- read.csv("./data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_935987.csv") %>% 
  select(-`Indicator.Name`, -`Indicator.Code`) %>% 
  rename(cc = `ï..Country.Name`, cc_code = `Country.Code`) %>% 
  gather(year, gdp, -c(cc, cc_code)) %>% 
  mutate(year = as.numeric(gsub("X", "", year))) %>% 
  filter(year >= 1970, 
         cc_code %in% d2$cc_code) %>% 
  mutate(iso = countrycode(cc_code, 'iso3c', 'iso3n')) %>% 
  mutate(region = countrycode(iso, "iso3n", "region")) 

d6 <- read_excel("./data/CLASS.xls", sheet = 1, skip = 5)[1:218,] %>% 
  select(-c("x...1", "x...2", "x...5", "x...8", "x...9")) %>% 
  rename(cc_name = "x...3", cc_code = "x...4", region = "x...6", 
         income_group = "x...7")
