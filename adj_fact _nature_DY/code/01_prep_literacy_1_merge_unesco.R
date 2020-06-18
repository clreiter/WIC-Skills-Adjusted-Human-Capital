#  read literacy data from Anne's files

rm(list = ls())

library(readxl)
library(tidyverse)
library(countrycode)


workdir <- "./data/UNESCO Country/"

files <- list.files(path = workdir,recursive = TRUE)

files2 <-  files %>% 
  as.data.frame() %>% 
  rename(name = ".") %>% 
  mutate(iso = countrycode(name, "country.name", "iso3n"), 
         iso3c = countrycode(iso, "iso3n", "iso3c"), 
         cc = row_number()) 

# 
# 
# 
# d1 <- read_excel(path = paste0(workdir,files)[2], sheet = 1, skip = 8, col_names = FALSE)[,1:12] %>% 
#   `colnames<-`(d1_col) 

d1_col <- c("year", "age_st", "age_end", "per_tot", "per_male", "per_female", "pop_il_tot", 
            "pop_il_male", "pop_il_female", "pop_tot", "pop_male", "pop_female")
            

d1 <- NULL

for(i in 1:132){
  tempdf <- read_excel(path = paste0(workdir,files)[i], sheet = 1, skip = 8, col_names = FALSE)[,1:12] %>% 
    `colnames<-`(c("year", "age_st", "age_end", "per_tot", "per_male", "per_female", "pop_il_tot", 
                  "pop_il_male", "pop_il_female", "pop_tot", "pop_male", "pop_female")) %>% 
    mutate(cc = i)
  d1 <- bind_rows(d1, tempdf)
}

d2 <- d1 %>% 
  left_join(files2) %>% 
  select(-name, -cc)

write.csv(d2, "./data/unesco_lit_est.csv", row.names = F)
