rm(list = ls())

library(tidyverse)


d1 <- read.csv("C:/Users/acer/Dropbox/research/Qualmatt/R/1970-2015/adj_fact/data/raw_data/wic_tot_dep_1970_2015.csv")
d2 <- read.csv("C:/Users/acer/Dropbox/research/Qualmatt/R/1970-2015/adj_fact/data/raw_data/wic_youth_dep_1970_2015.csv")
d3 <- read.csv("C:/Users/acer/Dropbox/research/Qualmatt/R/1970-2015/adj_fact/data/raw_data/wic_old_dep_1970_2015.csv")

d4<- d3%>% 
  left_join(d2) %>%
  left_join(d1)

write.csv(d4, "./data/wic_dep_1970_2015.csv", row.names = F)
