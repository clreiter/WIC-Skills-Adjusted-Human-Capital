# European countries with missing literacy are are estimated
rm(list = ls())

library(tidyverse)
library(countrycode)
library(jtools)
library(zoo)

# Countries with missing literacy data
# Austria, Belgium, Czechia, Denmark, Finland, 
# France,Germany, Iceland, Ireland, Luxembourg, 
# Netherlands, Norway, Slovakia, Sweden, 
# Switzerland, United Kingdom

# Partial info: 
# Bosnia, Serbia, Estonia, Montenegro, Macedonia

# Downloaded rom https://ourworldindata.org/literacy
d1 <- read.csv("./data/cross-country-literacy-rates.csv") %>% 
  mutate(iso = countrycode(Code, "iso3c", "iso3n"))

d2 <- read.csv("./data/lit_unesco_wb.csv")

# iso codes for WIC countries
wic_mys <- read.csv("./data/df_mys_sex.csv") %>% 
  group_by(year, iso, sex) %>% 
  summarise(mys = mean(mys))

wic_cc <- unique(wic_mys$iso)

# Filter the countries in Europe
d3 <- d2 %>% 
  filter(iso %in% wic_cc, 
         continent == "Europe") %>% 
  mutate(lit = unesco_il_prop, 
         lit = ifelse(is.na(lit), wb_il_prop, lit), 
         year5 = cut(year, breaks = c( seq(1969, 2029, 5)), 
                     labels = seq(1970, 2025, 5)), 
         year = as.numeric(as.character(year5))) %>% 
  group_by(iso, sex, cc, continent, year) %>% 
  summarise(lit = mean(lit, na.rm = T)) %>% 
  left_join(wic_mys)

# WIC educational attainment distribution 
# to group the countries with missing data
wic_noeduc <-  read.csv("./data/wic_edu.csv", sep = ";") %>% 
  rename(cc = Area, year = Year, 
         age = Age, sex = Sex,  
         edu = Education, iso = ISOCode, 
         prop = Distribution) %>% 
  mutate(age = substr(age, 1, 2)) %>% 
  filter(edu != "Under 15", age >= 15) %>% 
  spread(edu, prop)  %>% 
  mutate(e0 = `No Education`, 
         e1 = `Incomplete Primary`, 
         continent = countrycode(iso, "iso3n", "continent")) %>% 
  filter(sex != "Both", 
         continent == "Europe") %>% 
  select(year, age, sex, iso, e0, e1)

d4 <- read.csv("./data/unesco_lit_est.csv") %>% 
  filter(age_st != "15+", 
         !(age_st == 15 & age_end == 24),
         !is.na(iso3c)) %>% 
  select(year, age_st, per_male, per_female, iso) %>% 
  gather(sex, unesco_il_prop, -c(year, iso, age_st)) %>% 
  mutate(sex = ifelse(sex == "per_male", "Male", "Female"), 
         year5 = cut(year, breaks = c( seq(1969, 2029, 5)), 
                     labels = seq(1970, 2025, 5)), 
         year = as.numeric(as.character(year5))) %>% 
  rename(age = age_st) %>% 
  group_by(iso, age, sex, year) %>% 
  summarise(lit = mean(unesco_il_prop, na.rm = T))

### NO NEED TO RUN ###
### Grouping countries with missing data ###
# This part includes checking literacy proportion from other sources
# and WIC Data Explorer educational attainment to group the countries 
# 
# d5 <- wic_noeduc %>% 
#   left_join(d4) %>% 
#   mutate(cc = countrycode(iso, "iso3n", "country.name"))
# 
# 
# d6 <- d5 %>% 
#   group_by(iso, sex, year) %>% 
#   summarise(lit = mean(lit, na.rm = T), 
#            e0 = mean(e0, na.rm = T), 
#            e1 = mean(e1, na.rm = T))
# 
# d7 <- d3 %>% 
#   filter(year < 2020)
# 
# # missing countries
# missing_cc <- d7 %>% 
#   group_by(iso, sex, cc) %>% 
#   summarise(illiterate = mean(lit, na.rm = T)) %>% 
#   filter(is.nan(illiterate))
# 
# # countries with less than primary is low
# 
# # Check partially missing countries
# 
# par_missing_cc <- data.frame(cc = c("Bosnia & Herzegovina", "Estonia", "Montenegro",
#                     "Serbia", "Macedonia")) %>% 
#   mutate(iso = countrycode(cc, "country.name", "iso3n"))
# 
# highedu_cc <- wic_noeduc %>% 
#   mutate(lessPri = e0 + e1) %>% 
#   filter(year == 1970) %>% 
#   group_by(iso, sex) %>% 
#   summarise(lessPri = mean(lessPri)) %>% 
#   # filter(lessPri < 2) %>% 
#   mutate(cc = countrycode(iso, "iso3n", "country.name")) %>% 
#   filter(iso %in% c(missing_cc$iso, par_missing_cc$iso))


# Less than primary completed is less than 2%
# > unique(highedu_cc$cc) 1970
# [1] "Austria"        "Czechia"        "Denmark"        "Finland"        "Germany"        "Ireland"        "Luxembourg"     "Norway"        
# [9] "Sweden"         "United Kingdom"

# > unique(highedu_cc$cc) 2010
# [1] "Austria"        "Belgium"        "Czechia"        "Denmark"        "Finland"        "France"         "Germany"        "Ireland"       
# [9] "Luxembourg"     "Netherlands"    "Norway"         "Slovakia"       "Sweden"         "Switzerland"    "United Kingdom"


# 1970 start with 98% 2020 finish with 99.8%

# [1] "Austria"        "Czechia"        "Denmark"        "Finland"        "Germany"        "Luxembourg"     "Norway"         "Sweden"        
# [9] "United Kingdom"
# slovakia

#### Assumptions ####
# d3 lit = illiteracy unesco, if is.na(unesco), worldbank 

# 1970 start with 98% 2020finish with 99.8%
# apply Poland rates

# Countries with missing literacy data are grouped into cc1, cc2, cc3  depending on 
# their literacy rates in 1970 and 2020 as found in other data sources
# or educational attainment distribution from WIC Data Explorer
# or by looking at experiences of similar countries 
# as shown in d9 data frame below

# then illiteracy curve from Poland has been applied to these countries 
# we assumed countries in cc1 will follow the same literacy curve as Poland which is
# very low in 1970 and almost 0.02 in 2025
# countries in cc2 group have/had higher literacy rates than Poland and assumed
# to have twice as much illierate proportion as Poland
# men in cc3 countries assumed to have 5 times the illiterate proportion as Poland
# women in cc3 countries assumed to 10 times the illiterate proportion as Poland

cc1 <- c("Austria", "Czechia", "Denmark", "Finland", "Germany", "Luxembourg", "Norway","Sweden", "United Kingdom", 
         "Slovakia", "Switzerland", "Estonia")
# Belgium Ireland France Netherlands start with 96% finish with 99.5% 
cc2 <- c("Belgium", "France", "Ireland", "Netherlands", "Iceland")
# countries with partially missing data
cc3 <- c("Bosnia & Herzegovina", "Montenegro",
         "Serbia", "Macedonia")

d9 <- d3 %>% 
   mutate(est = lit, 
         est = ifelse(is.nan(est), NA, est), 
         est = ifelse((sex == "Female" & year == 1970 & iso == 499), 20, est), # montenegro
         est = ifelse((sex == "Male" & year == 1970 & iso == 499), 10, est), 
         est = ifelse((sex == "Female" & year == 2025 & iso == 499), 0.5, est), 
         est = ifelse((sex == "Male" & year == 2025 & iso == 499), 0.3, est), 
         est = ifelse((sex == "Male" & year == 2025 & iso == 499), 0.3, est),
         est = ifelse((sex == "Female" & year == 1970 & iso == 70), 20, est), # Bosnia
         est = ifelse((sex == "Male" & year == 1970 & iso == 70), 10, est),
         est = ifelse((sex == "Female" & year == 2025 & iso == 70), 2, est), 
         est = ifelse((sex == "Male" & year == 2025 & iso == 70), 0.3, est),
         est = ifelse((sex == "Female" & year == 1970 & iso == 688), 20, est), # Serbia
         est = ifelse((sex == "Male" & year == 1970 & iso == 688), 10, est),
         est = ifelse((sex == "Female" & year == 2025 & iso == 688), 2, est), 
         est = ifelse((sex == "Male" & year == 2025 & iso == 688), 0.3, est),
         est = ifelse((sex == "Female" & year == 1970 & iso == 807), 20, est), # Macedonia
         est = ifelse((sex == "Male" & year == 1970 & iso == 807), 10, est),
         est = ifelse((sex == "Female" & year == 2025 & iso == 807), 2, est), 
         est = ifelse((sex == "Male" & year == 2025 & iso == 807), 0.3, est),
         est = ifelse((sex == "Female" & year == 2025 & iso == 8), 1.3, est), # Albania https://en.wikipedia.org/wiki/Education_in_Albania
         est = ifelse((sex == "Male" & year == 2025 & iso == 8), 0.8, est),
         est = ifelse((sex == "Female" & year == 2025 & iso %in% c(804)), 0.13, est), # Ukraine, UNESCO country profiles
         est = ifelse((sex == "Male" & year == 2025 & iso %in% c(804)), 0.13, est),
         # est = ifelse((sex == "Female" & year == 2025 & iso %in% c(233, 804)), 0.02, est), #  Estonia,  
         # est = ifelse((sex == "Male" & year == 2025 & iso %in% c(233, 804)), 0.02, est),
         est = ifelse((year == 1970 & cc %in% cc1), 2, est), # industrialised countries 1
         est = ifelse((year == 2025 & cc %in% cc1), 0.2, est), 
         est = ifelse((year == 1970 & cc %in% cc2), 4, est), # industrialised countries 2
         est = ifelse((year == 2025 & cc %in% cc2), 0.5, est)) %>% 
  group_by(sex, cc) %>% 
  mutate(est2 = na.approx(est), 
         est3 = est2)

# The curve for Poland
d10 <- d9 %>% 
  ungroup() %>% 
  filter(cc == "Poland") %>% 
  select(sex, year, lit) %>% 
  rename(polandlit = lit)
  
# Applying the curve from Poland to three groups of countries
d11 <- d9 %>% 
  filter(cc %in% c(cc1, cc2, cc3)) %>% 
  left_join(d10) %>% 
  mutate(est3 = ifelse(cc %in% cc1, polandlit, est3), 
         est3 = ifelse(cc %in% cc2, 2 * polandlit, est3), 
         est3 = ifelse((cc %in% cc3 & sex == "Female"), 10 * polandlit, est3), 
         est3 = ifelse((cc %in% cc3 & sex == "Male"), 5 * polandlit, est3)) %>% 
  select(-polandlit)

# Other European countries merged with new estimates
d12 <- d9 %>% 
  filter(!cc %in% c(cc1, cc2, cc3)) %>% 
  bind_rows(d11) %>% 
  arrange(cc, sex, year)
  

pdf("./figures/europe_ill_estimated.pdf")
for(i in seq(1, length(unique(d12$cc)), 1)){
  p <- ggplot(d12 %>% filter(cc %in% unique(d12$cc)[i]), aes(x = year, y = est3)) +
    geom_line() +
    geom_point(aes(x = year, y = lit), col = "red") +
    ggtitle(paste0(unique(d12$cc)[i])) +
    facet_grid(~ sex) 
  print(p)
}
dev.off()

write.csv(d12, "./data/lit_europe_estimated.csv", row.names = F)

cc4 <- as.character(unique(d12$cc))

# All countries
# unesco illiterate proportion
# if missing >> world bank illiterate proportion
# if missing & European country >> estimates explained above

d13 <- d2 %>% 
  mutate(lit = unesco_il_prop, 
         lit = ifelse(is.na(lit), wb_il_prop, lit), 
         year5 = cut(year, breaks = c( seq(1969, 2029, 5)), 
                     labels = seq(1970, 2025, 5)), 
         year = as.numeric(as.character(year5))) %>% 
  group_by(iso, sex, cc, continent, year) %>% 
  summarise(lit = mean(lit, na.rm = T)) %>% 
  left_join(wic_mys) %>% 
  mutate(est3 = lit) %>% 
  filter(!cc %in% cc4) 

d14 <- d12 %>% 
  bind_rows(d13)
  
write.csv(d14, "./data/lit_europe_estimated_othersuis_wb.csv", row.names = F)

