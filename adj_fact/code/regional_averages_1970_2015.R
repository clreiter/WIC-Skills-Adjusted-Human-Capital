rm(list = ls())

library(tidyverse)
library(jtools)
library(countrycode)
library(readxl)

d1 <- read.csv("./results/samys_1970_2015.csv") %>%
    mutate(region = countrycode(iso, "iso3n", "region"),
    region = ifelse(country == "Channel Islands", "Northern Europe", region))
d2 <- read.csv("./data/df_wic_population_1970_2015.csv", sep=";")
d3 <- d1 %>% 
  left_join(d2) 

d3<-d3[(d3$year=="1970"|d3$year=="2015"),]

library(plyr)
d3$region <- revalue(d3$region, c("Australia and New Zealand"="Oceania","Caribbean"="Latin America", "Central America"="Latin America",
                               "Central Asia"="Central and South Asia", "Eastern Africa"="Sub-Saharan Africa",
                               "Eastern Europe"="Europe", "Melanesia"="Oceania", "Micronesia"="Oceania",
                               "Middle Africa"="Sub-Saharan Africa", "Northern Africa"="MENA", "Northern Europe"="Europe",
                               "Polynesia"="Oceania", "South America"="Latin America", "South-Eastern Asia"="Central and South Asia",
                               "Southern Africa"="Sub-Saharan Africa", "Southern Asia"="Central and South Asia",
                               "Southern Europe"="Europe", "Western Africa"="Sub-Saharan Africa", "Western Asia"="MENA",
                               "Western Europe"="Europe"))
d3$region[which(d3$country == "CuraÃ§ao")] = "Latin America"
d3$region[which(d3$country == "Montenegro")] = "Europe"
d3$region[which(d3$country == "Serbia")] = "Europe"
d3$region[which(d3$country == "Taiwan Province of China")] = "Eastern Asia"
detach("package:plyr", unload = TRUE)

d4<-d3 %>%
  dplyr::group_by(region, year) %>% 
  dplyr::mutate(weighted_samys = weighted.mean(samys_pred, population))%>% 
  dplyr::mutate(weighted_mys = weighted.mean(wic_mys, population))
write.csv(d4, "./results/regional_averages_1970-2015.csv")

d5<-d3 %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(world_samys = weighted.mean(samys_pred, population))%>% 
  dplyr::mutate(world_mys = weighted.mean(wic_mys, population))

