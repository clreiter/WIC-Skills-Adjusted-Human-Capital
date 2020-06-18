
rm(list = ls())

library(tidyverse)
library(countrycode)
library(jtools)
library(zoo)

# Missing countries
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

