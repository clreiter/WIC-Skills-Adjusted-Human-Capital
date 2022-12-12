#============================================================#
#=====> SLAMYS Time Series Estimation for 185 countries <=====#
#============================================================#

#=====> STEP 0: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#=> Installing necessary packages
#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("countrycode")
#install.packages("jtools")
#install.packages("zoo")
#install.packages("fastDummies")
#install.packages("MASS")
#install.packages("olsrr")
#install.packages("readxl")
#install.packages("dplyr")    # alternative installation of the %>%

#=> Loading packages
library(readxl)
library(tidyverse)
library(countrycode)
library(jtools)
library(zoo)
library(fastDummies)
library(MASS)
library(olsrr)
library(readxl)
library(dplyr)    # alternatively, this also loads %>%

#=====> STEP 1: PREPARE LITERACY DATA MERGE UNESCO
#-----------------------------------------------------------------------------------------------------------

#=> Changing the working directory
workdir <- "C:/Users/Raquel/GitHub/WiC-Human-Capital-Quality-Projections/adj_fact/"
setwd(workdir)
getwd()

data <- "./data/UNESCO Country/"

files <- list.files(path = data,recursive = TRUE)

files2 <-  files %>% 
  as.data.frame() %>% 
  rename(name = ".") %>% 
  mutate(iso = countrycode(name, "country.name", "iso3n"), 
         iso3c = countrycode(iso, "iso3n", "iso3c"), 
         cc = row_number()) 

# 
# 
# 
# d1 <- read_excel(path = paste0(data,files)[2], sheet = 1, skip = 8, col_names = FALSE)[,1:12] %>% 
#   `colnames<-`(d1_col) 

d1_col <- c("year", "age_st", "age_end", "per_tot", "per_male", "per_female", "pop_il_tot", 
            "pop_il_male", "pop_il_female", "pop_tot", "pop_male", "pop_female")
            

d1 <- NULL

for(i in 1:132){
  tempdf <- read_excel(path = paste0(data,files)[i], sheet = 1, skip = 8, col_names = FALSE)[,1:12] %>% 
    `colnames<-`(c("year", "age_st", "age_end", "per_tot", "per_male", "per_female", "pop_il_tot", 
                  "pop_il_male", "pop_il_female", "pop_tot", "pop_male", "pop_female")) %>% 
    mutate(cc = i)
  d1 <- bind_rows(d1, tempdf)
}

d2 <- d1 %>% 
  left_join(files2) %>% 
  select(-name, -cc)

write.csv(d2, "./data/unesco_lit_est.csv", row.names = F)

#=====> STEP 2: PREPARE WORLD BANK ILLITERACY RATES
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

# Reading WB data for females
d1 <- read_excel("./data/WB_literacy_rate_female.xlsx")

d2 <- d1 %>% 
  mutate(iso = countrycode(`Country Code`, "iso3c", "iso3n")) %>% 
  filter(!is.na(iso)) %>% 
  select(-c(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Code`)) %>% 
  gather(year, literate_prop, -iso) %>% 
  mutate(wb_il_prop = 100 - literate_prop, 
         sex = "Female")

# Reading WB data for males

d3 <- read_excel("./data/WB_literacy_rate_male.xlsx")

d4 <- d3 %>% 
  mutate(iso = countrycode(`Country Code`, "iso3c", "iso3n")) %>% 
  filter(!is.na(iso)) %>% 
  select(-c(`Country Code`, `Country Name`, `Indicator Name`, `Indicator Code`)) %>% 
  gather(year, literate_prop, -iso) %>% 
  mutate(wb_il_prop = 100 - literate_prop, 
         sex = "Male")

d5 <- bind_rows(d4, d2)

write.csv(d5, "./data/WB_literate_prop.csv", row.names = F)

#=====> STEP 3: READ LITERACY DATA FROM OLD UNESCO FILES, WB DATA AND ASSUMPTIONS FOR DEVELOPED COUNTRIES
#-----------------------------------------------------------------------------------------------------------

# 131 countries 
# by age and sex and single years
# aggregate 

# years <- seq(1970, 2025, by = 5)

ccs <- read.csv("./data/df_mys_sex.csv")
ccs_iso <- unique(ccs$iso)

d1 <- read.csv("./data/unesco_lit_est.csv") %>% 
  filter(age_st == "15+", 
         !is.na(iso3c)) %>% 
  select(year, per_male, per_female, iso) %>% 
  gather(sex, unesco_il_prop, -c(year, iso)) %>% 
  mutate(sex = ifelse(sex == "per_male", "Male", "Female"))
  
  
d2 <- read.csv("./data/WB_literate_prop.csv") %>% 
  select(-literate_prop) %>% 
  filter(year >= 1970)

d3 <- expand.grid(iso = ccs_iso, year = seq(1970, 2025, 1), sex = c("Male", "Female")) %>% 
  full_join(d1) %>% 
  full_join(d2) %>% 
  mutate(cc = countrycode(iso, "iso3n", "country.name"), 
         continent = countrycode(iso, "iso3n", "continent")) %>% 
  filter(!is.na(continent))

write.csv(d3, "./data/lit_unesco_wb.csv", row.names = F)

# Europe countries in WIC 
   d4 <- d3 %>% 
    filter(continent == "Europe", 
           iso %in% ccs_iso)
  
pdf("./figures/europe_ill_wic_countries.pdf")
  for(i in seq(1, length(unique(d4$cc)), 1)){
   p <- ggplot(d4 %>% filter(cc %in% unique(d4$cc)[i]), aes(x = year, y = unesco_il_prop)) +
    geom_point() +
    geom_point(aes(x = year, y = wb_il_prop), col = "red") +
     ggtitle(paste0(unique(d4$cc)[i])) +
    facet_grid(~ sex) 
   print(p)
  }
dev.off()

#=====> STEP 4: PREDICTION MODEL FOR THE LITERACY RATES FOR COUNTRIES WITH NO INFORMATION
#-----------------------------------------------------------------------------------------------------------

# Countries with missing data
# Austria, Belgium, Czechia, Denmark, Finland, 
# France,Germany, Iceland, Ireland, Luxembourg, 
# Netherlands, Norway, Slovakia, Sweden, 
# Switzerland, United Kingdom

# Partial info: 
# Bosnia, Serbia, Estonia, Montenegro, Macedonia

# Assumptions: 
# 0.02: Austria

# Downloaded rom https://ourworldindata.org/literacy
d1 <- read.csv("./data/cross-country-literacy-rates.csv") %>% 
  mutate(iso = countrycode(Code, "iso3c", "iso3n"))

d2 <- read.csv("./data/lit_unesco_wb.csv")

wic_mys <- read.csv("./data/df_mys_sex.csv") %>% 
  group_by(year, iso, sex) %>% 
  summarise(mys = mean(mys))

wic_cc <- unique(wic_mys$iso)

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

lm1 <- lm(lit ~ sex + cc + mys + year, data = d3)
summ(lm1)

# > summ(lm1)
# MODEL INFO:
#   Observations: 460 (476 missing obs. deleted)
# Dependent Variable: lit
# Type: OLS linear regression 
# 
# MODEL FIT:
#   F(25,434) = 41.75, p = 0.00
# R² = 0.71
# Adj. R² = 0.69 
# 
# Standard errors: OLS
# --------------------------------------------------------------
#   Est.    S.E.   t val.      p
# ---------------------------- --------- ------- -------- ------
#   (Intercept)                    -281.01   69.59    -4.04   0.00
# sexMale                          -0.56    0.40    -1.40   0.16
# ccBelarus                       -13.37    1.31   -10.18   0.00
# ccBosnia & Herzegovina          -14.56    1.71    -8.50   0.00
# ccBulgaria                      -10.33    1.39    -7.41   0.00
# ccCroatia                       -10.43    1.37    -7.59   0.00
# ccEstonia                        -9.20    2.11    -4.36   0.00
# ccGreece                        -10.83    1.25    -8.67   0.00
# ccHungary                       -10.79    1.48    -7.27   0.00
# ccItaly                         -13.43    1.23   -10.88   0.00
# ccLatvia                         -5.93    1.96    -3.02   0.00
# ccLithuania                      -9.86    1.57    -6.26   0.00
# ccMacedonia                     -17.72    1.72   -10.28   0.00
# ccMalta                          -5.82    1.23    -4.71   0.00
# ccMoldova                       -18.27    1.10   -16.66   0.00
# ccMontenegro                    -10.28    1.55    -6.62   0.00
# ccPoland                         -7.22    1.81    -3.98   0.00
# ccPortugal                      -14.58    1.19   -12.30   0.00
# ccRomania                       -11.88    1.29    -9.19   0.00
# ccRussia                        -16.37    1.16   -14.10   0.00
# ccSerbia                        -10.23    2.15    -4.75   0.00
# ccSlovenia                       -7.36    1.81    -4.06   0.00
# ccSpain                         -14.66    1.14   -12.90   0.00
# ccUkraine                       -16.71    1.19   -14.06   0.00
# mys                              -2.99    0.34    -8.72   0.00
# year                              0.16    0.04     4.48   0.00
# --------------------------------------------------------------

# Try with no education proportions from wic 
# and unesco age distribution 


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

d5 <- wic_noeduc %>% 
  left_join(d4) %>% 
  mutate(cc = countrycode(iso, "iso3n", "country.name"))

lm2 <- lm(lit ~  age + sex + iso + e0 + year, data = d5)
summ(lm2)

# > summ(lm2)
# MODEL INFO:
#   Observations: 4708 (11204 missing obs. deleted)
# Dependent Variable: lit
# Type: OLS linear regression 
# 
# MODEL FIT:
#   F(14,4693) = 409.36, p = 0.00
# R² = 0.55
# Adj. R² = 0.55 
# 
# Standard errors: OLS
# -------------------------------------------------
#   Est.    S.E.   t val.      p
# ----------------- ------- ------- -------- ------
#   (Intercept)         91.26   11.83     7.71   0.00
# age20                0.14    0.45     0.32   0.75
# age25                0.32    0.45     0.72   0.47
# age30                0.57    0.45     1.26   0.21
# age35                0.80    0.45     1.78   0.07
# age40                1.05    0.45     2.32   0.02
# age45                1.25    0.45     2.77   0.01
# age50                1.45    0.45     3.19   0.00
# age55                1.59    0.45     3.50   0.00
# age60                1.77    0.46     3.86   0.00
# age65                1.96    0.46     4.24   0.00
# sexMale             -0.86    0.19    -4.43   0.00
# iso                 -0.01    0.00   -19.98   0.00
# e0                   0.56    0.01    59.92   0.00
# year                -0.04    0.01    -7.34   0.00
# -------------------------------------------------

d6 <- d5 %>% 
  group_by(iso, sex, year) %>% 
  summarise(lit = mean(lit, na.rm = T), 
           e0 = mean(e0, na.rm = T), 
           e1 = mean(e1, na.rm = T))

cor.test(d5$e0, d5$lit)
# > cor.test(d5$e0, d5$lit)
# 
# Pearson's product-moment correlation
# 
# data:  d5$e0 and d5$lit
# t = 68.924, df = 4706, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.6942592 0.7227025
# sample estimates:
#       cor 
# 0.7087688 

lm3 <- lm(lit ~  sex + iso + e0 + e1 + year, data = d6)
# summ(lm3)
# > summ(lm3)
# MODEL INFO:
#   Observations: 428 (508 missing obs. deleted)
# Dependent Variable: lit
# Type: OLS linear regression 
# 
# MODEL FIT:
#   F(5,422) = 91.70, p = 0.00
# R² = 0.52
# Adj. R² = 0.52 
# 
# Standard errors: OLS
# -------------------------------------------------
#   Est.    S.E.   t val.      p
# ----------------- ------- ------- -------- ------
#   (Intercept)          6.21   36.92     0.17   0.87
# sexMale             -0.15    0.55    -0.27   0.79
# iso                 -0.01    0.00    -5.09   0.00
# e0                   0.38    0.02    16.23   0.00
# e1                  -0.03    0.03    -0.98   0.33
# year                -0.00    0.02    -0.10   0.92
# -------------------------------------------------

# Regression does not work!

# try expert opinion

d7 <- d3 %>% 
  filter(year < 2020)

# missing countries

missing_cc <- d7 %>% 
  group_by(iso, sex, cc) %>% 
  summarise(illiterate = mean(lit, na.rm = T)) %>% 
  filter(is.nan(illiterate))

# countries with less than primary is low

# Check partially missing countries

par_missing_cc <- data.frame(cc = c("Bosnia & Herzegovina", "Estonia", "Montenegro",
                    "Serbia", "Macedonia")) %>% 
  mutate(iso = countrycode(cc, "country.name", "iso3n"))

highedu_cc <- wic_noeduc %>% 
  mutate(lessPri = e0 + e1) %>% 
  filter(year == 1970) %>% 
  group_by(iso, sex) %>% 
  summarise(lessPri = mean(lessPri)) %>% 
  # filter(lessPri < 2) %>% 
  mutate(cc = countrycode(iso, "iso3n", "country.name")) %>% 
  filter(iso %in% c(missing_cc$iso, par_missing_cc$iso))


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

# Belgium Ireland France Netherlands start with 96% finish with 99.5% 

wic_par_miss <- wic_noeduc %>% 
  mutate(lessPri = e0 + e1) %>% 
  filter(year == 1970, iso %in% par_missing_cc$iso) %>% 
  mutate(cc = countrycode(iso, "iso3n", "country.name")) 

# World bank data and lesPri


wic_15plus <- read.csv("./data/wic_15plus_edu.csv", sep = ";") %>% 
  rename(cc = Area, year = Year, 
         age = Age, sex = Sex,  
         edu = Education, iso = ISOCode, 
         prop = Distribution) %>% 
  spread(edu, prop)  %>% 
  mutate(e0 = `No Education`, 
         e1 = `Incomplete Primary`, 
         continent = countrycode(iso, "iso3n", "continent")) %>% 
  filter(sex != "Both", 
         continent == "Europe") %>% 
  select(year, sex, iso, e0, e1)

d8 <- d2 %>% 
  mutate(year5 = cut(year, breaks = c(seq(1969, 2029, 5)), 
              labels = seq(1970, 2025, 5)), 
year = as.numeric(as.character(year5))) %>% 
  group_by(iso, year, sex, cc, continent) %>% 
  summarise(wb_il_prop= mean(wb_il_prop, na.rm = T)) %>% 
  filter(iso %in%par_missing_cc$iso) %>% 
  left_join(wic_15plus) %>% 
  mutate(ratio = wb_il_prop/e0)

hist(d8$ratio)

ggplot(d8, aes(x = e0, y = wb_il_prop, color = cc)) +
  geom_point() +
  geom_abline(slope = 1) 

cor.test(d8$wb_il_prop, d8$e0)


#### Assumptions ####
# d3 lit = illiteracy unesco, if is.na(unesco), worldbank 

# 1970 start with 98% 2020finish with 99.8%
# apply Poland rates

cc1 <- c("Austria", "Czechia", "Denmark", "Finland", "Germany", "Luxembourg", "Norway","Sweden", "United Kingdom", 
         "Slovakia", "Switzerland", "Estonia")
cc2 <- c("Belgium", "France", "Ireland", "Netherlands", "Iceland")

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

d10 <- d9 %>% 
  ungroup() %>% 
  filter(cc == "Poland") %>% 
  select(sex, year, lit) %>% 
  rename(polandlit = lit)
  
d11 <- d9 %>% 
  filter(cc %in% c(cc1, cc2, cc3)) %>% 
  left_join(d10) %>% 
  mutate(est3 = ifelse(cc %in% cc1, polandlit, est3), 
         est3 = ifelse(cc %in% cc2, 2 * polandlit, est3), 
         est3 = ifelse((cc %in% cc3 & sex == "Female"), 10 * polandlit, est3), 
         est3 = ifelse((cc %in% cc3 & sex == "Male"), 5 * polandlit, est3)) %>% 
  select(-polandlit)

d12 <- d9 %>% 
  filter(!cc %in% c(cc1, cc2, cc3)) %>% 
  bind_rows(d11) %>% 
  arrange(cc, sex, year)
  
pdf("./figures/europe_ill_estimated14feb.pdf")
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


#=====> STEP 5: PREPARING LITERACY DATA - IMPUTATION USING REGIONAL AVERAGES AND STATA DO-FILES
# Refer to do-file step5.do
#-----------------------------------------------------------------------------------------------------------


#=====> STEP 6: PREPARING LITERACY DATA - IMPUTATION USING REGIONAL AVERAGES
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

#illiteracy data downloaded from UIS and then missing values replaced
# in STATA using old UNESCO estimates and regional averages */
d1 <- read.csv("./data/literacy_1970_2015_gender.csv", sep = ";")

d2 <- d1 %>% 
  rename(illiterate_prop = tot_ill_) %>% 
  select(iso, year, illiterate_prop)

write.csv(d2, "./data/df_illiterate_prop_cc_1970_2015.csv", row.names = F)

#=====> STEP 7: PREPARING LITERACY DATA - LESS THAN SECONDARY EDUCATION
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

d1 <- read.csv("./data/wicdf_enrolment_1970_2015.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         edu = Education,
         iso = ISOCode, 
         LS = Distribution) %>% 
  spread(edu, LS) %>% 
  mutate(LS = `Under 15` + `No Education` + `Incomplete Primary` + `Primary` + `Lower Secondary`, 
         highLS = 100 - LS) %>% 
  select(cc, year, iso, age, highLS)

write.csv(d2, "./data/df_LS_cc_broad_age_1970_2015.csv", row.names = F)

#=====> STEP 8: PREPARING MEAN YEARS OF SCHOOL DATA - ?? COMPLETE HERE
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

d1 <- read.csv("./data/wicdf_enrolment_1970_2015.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         age = Age,
         edu = Education,
         iso = ISOCode, 
         LS = Distribution) %>% 
  spread(edu, LS) %>% 
  mutate(LS = `Under 15` + `No Education` + `Incomplete Primary` + `Primary` + `Lower Secondary`, 
         highLS = 100 - LS) %>% 
  select(cc, year, iso, age, highLS)

write.csv(d2, "./data/df_LS_cc_broad_age_1970_2015.csv", row.names = F)

#=====> STEP 9: CALCULATE DHS ADJUSTMENT FACTOR 
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

d1 <- read.csv("./data/DHS_adjustment-factor_2000-15.csv") %>% 
  select(iso, year, adj_factor1, adj_factor2) %>% 
  rename(dhs_factor_full = adj_factor1, 
         dhs_factor_part = adj_factor2)

ggplot(d1, aes(x = dhs_factor_full, y = dhs_factor_part)) +
  geom_point() +
  geom_abline(intercept = 0) +
  ylim(0,1) +
  xlim(0,1)

d2 <- read.csv("./data/df_mys_cc_broad_age_1970_2015.csv") %>% 
  filter(age == "20--64") %>% 
  select(-age)

d3 <- d2 %>% 
  left_join(d1) %>% 
  mutate(qamys_dhs_full = wic_mys * dhs_factor_full,
         qamys_dhs_part = wic_mys * dhs_factor_part) %>% 
  select(iso, year, qamys_dhs_full, qamys_dhs_part)

write.csv(d3, "./data/qamys_dhs_full_part_2000_2015.csv", row.names = F)


#=====> STEP 10: PREPARING UIS DATA - 
# Refer to do-file step10.do
#-----------------------------------------------------------------------------------------------------------

#=====> STEP 11: PREPARING DATA FOR GOVERNMENT EXPENDITURES - 
# Refer to do-file step11.do
#-----------------------------------------------------------------------------------------------------------

#=====> STEP 12: LOAD OLD DEPENDENCY RATIO DATA FROM WIC 
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

d1 <- read.csv("./data/wicdf_old_age_dep.csv")

d2 <- d1 %>% 
  rename(cc = Area, year = Year, 
         iso = ISOCode, 
         old_dep = Ratio)


write.csv(d2, "./data/df_old_age_dep_1970_2015.csv", row.names = F)

#=====> STEP 13: QEI regression model
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

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

step.model <- stepAIC(lm1, direction = "both", 
                      trace = FALSE)
summary(step.model)
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

#=====> STEP 14: SAMYS prediction model
#-----------------------------------------------------------------------------------------------------------

#=> Removing all objects from memory
rm(list = ls(all.names = TRUE))

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

#comparison of model without hlo
#model2 <-lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop +
#year_1970 + year_1975 + year_1980 + year_1985 + year_1990 + year_1995 + 
#  year_2000 + year_2005 + year_2010, data = r1)
#summ(model2)
#ols_vif_tol(model2)

# Stepwise regression model
#step.model <- stepAIC(model2, direction = "both", 
#                      trace = FALSE)
#summary(step.model)

#lm2 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop +
#            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)

#lm2predict <- as.data.frame(predict(lm2, newdata = r1, interval = "confidence")) 
#colnames(lm2predict) <- paste0(colnames(lm2predict),2)

#df1 <- r1 %>% 
#  bind_cols(lm1predict) %>% 
#  mutate(fit1 = exp(fit1)*wic_mys, 
#         lwr1 = exp(lwr1)*wic_mys, 
#         upr1 = exp(upr1)*wic_mys) %>%
#  bind_cols(lm2predict) %>% 
#  mutate(fit2 = exp(fit2)*wic_mys, 
#         lwr2 = exp(lwr2)*wic_mys, 
#         upr2 = exp(upr2)*wic_mys) %>%   
#  mutate(samys_pred1 = samys, 
#         samys_pred1 = ifelse(is.na(samys_pred1), fit1, samys_pred1))%>%   
#  mutate(samys_pred2 = samys, 
#         samys_pred2 = ifelse(is.na(samys_pred2), fit2, samys_pred2))
#
#detach("package:MASS", unload=TRUE)

#df1 <- df1[order(df1$cc, df1$year),]
#pdf("./figures/countries_samys_comparison.pdf")
#for(i in seq(1, length(unique(df1$cc)), 25)){
#  pp <-  ggplot(df1 %>% filter(!is.na(samys_pred2), cc %in% unique(df1$cc)[i: (i+ 24)]), 
#                aes(x = year, y = wic_mys)) +
#    geom_bar(stat = "identity", alpha = 0.25) +
#    geom_point(alpha=0.5, aes(x = year, y = samys_pred1)) +
#    geom_point(alpha=0.5, aes(x = year, y = samys_pred2), color="blue") +
#    xlab("Year") +
#    ylab("WIC MYS & SAMYS (95% CI)") +
#    ylim(-0.1, 17.5) +
#    theme_bw() +
#    facet_wrap(~ cc, nrow = 5, ncol = 5) +
#    theme(axis.text.x = element_text(angle = 90))
#  print(pp)
#}
# dev.off()

#=====> STEP 15: INITIAL CONFIGURATIONS
#-----------------------------------------------------------------------------------------------------------


d1 <- read.csv("./results/samys_1970_2015.csv") %>%
    mutate(region = countrycode(iso, "iso3n", "region"),
    region = ifelse(cc == "Channel Islands", "Northern Europe", region))

d2 <- read.csv("./data/df_wic_population_1970_2015.csv", sep=";")

d3 <- d1 %>% 
  left_join(d2) 

d3 <- d3[(d3$year=="1970"|d3$year=="2015"),]

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

d4 <- d3 %>%
  dplyr::group_by(region, year) %>% 
  dplyr::mutate(weighted_samys = weighted.mean(samys_pred, population)) %>% 
  dplyr::mutate(weighted_mys = weighted.mean(wic_mys, population))

write.csv(d4, "./results/regional_averages_1970-2015.csv")

d5 <- d3 %>%
  dplyr::group_by(year) %>% 
  dplyr::mutate(world_samys = weighted.mean(samys_pred, population)) %>% 
  dplyr::mutate(world_mys = weighted.mean(wic_mys, population))

