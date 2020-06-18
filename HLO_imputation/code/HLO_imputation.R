rm(list = ls())
library(tidyverse)
library(countrycode)
library(fastDummies)
library(maps)

d1 <- read.csv("./data/hlo_empirical_1970_2015.csv")

ggplot(d1, aes(x = year, y = hlo_mean, group = 1, color = country)) +
  geom_line(aes(group = country)) +
  geom_smooth(aes(group = country)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none")

d2 <- read.csv("./data/uis_edu_exp.csv")

d3 <- read.csv("./data/uis_tp.csv")

d4 <- left_join(d1, d2) %>% 
  left_join(d3 %>% select(-cc), by = c("year", "iso")) %>% 
  dummy_cols(select_columns = "year") %>% 
  mutate(region = countrycode(iso, "iso3n", "region"),
         region = ifelse(country == "Channel Islands", "Northern Europe", region)) %>% 
  filter(!region %in% c("Melanesia", "Micronesia", "Polynesia"))

d5 <- d4 %>% 
  filter(!is.na(hlo_mean)) 

d6 <-  d4 %>% 
  filter(!country %in% unique(d5$country)) %>% 
  select(country, region) %>%
  unique()

cor(d5$hlo_mean, d5$edu_exp_)
cor(d5$hlo_mean, d5$tp_)
cor(d5$tp_, d5$edu_exp_)

# lm model with region does not work well

lm1 <- lm(hlo_mean ~ region + tp_ + edu_exp_ + year_1970 +
            year_1975 + year_1980 + year_1985 + year_1990 +
            year_1995 + year_2000 + year_2005 + year_2010 +
            year_2015, data = d4)
summary(lm1)

lm1predict <- as.data.frame(predict(lm1, newdata = d4, interval = "confidence"))
colnames(lm1predict) <- paste0(colnames(lm1predict),1)

d7 <- bind_cols(d4, lm1predict)

ggplot(d7, aes(x = hlo_mean, y = fit1)) +
  geom_point()

plot(density(d5$hlo_mean))
plot(density(d7$fit1[!is.na(d7$fit1)]))

png("./figures/HLO_year_m1_predictions_region_95CI.png", width = 7.5, height = 7.5,
    res = 300, units = "in")
ggplot(d7, aes(x = year, y = hlo_mean, group = 1, color = country)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  theme(legend.position = "none") +
  facet_wrap(~region, nrow = 5) +
  ggtitle("HLO predictions - region model")
dev.off()

pdf("./figures/HLO_year_predictions_regional_model_cc.pdf")
for(i in seq(1, length(unique(d7$country)),4)){
p <- ggplot(d7 %>% filter(country %in% unique(d7$country)[i: (i+3)]), 
       aes(x = year, y = hlo_mean, group = 1, color = country)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr1, ymax = upr1)) +
  theme(legend.position = "none") +
  facet_wrap(~country, ncol = 2) 
print(p)
}
dev.off()


# country model for 136 countries 

d8 <- d4 %>%  
  filter(country %in% unique(d5$country))
lm2 <- lm(hlo_mean ~  country + tp_ + edu_exp_ + year_1970 +
                         year_1975 + year_1980 + year_1985 + year_1990 +
                       year_1995 + year_2000 + year_2005 + year_2010 +
                        year_2015, data = d8)
summary(lm2)
     
d9 <- d8 %>% 
  filter(!is.na(tp_), 
         !is.na(edu_exp_))
d9$country <- factor(d9$country, levels = levels(d8$country))
lm2predict <- as.data.frame(predict(lm2, newdata = d9, interval = "confidence")) 
colnames(lm2predict) <- paste0(colnames(lm2predict),2)
             
d10 <- bind_cols(d9, lm2predict) %>% 
  mutate(fark = hlo_mean - fit2)
             
ggplot(d10, aes(x = hlo_mean, y = fit2)) +
  geom_point()


pdf("./figures/HLO_year_predictions_country_model_cc.pdf")
for(i in seq(1, length(unique(d10$country)),4)){
  p <- ggplot(d10 %>% filter(country %in% unique(d10$country)[i: (i+3)]), 
              aes(x = year, y = hlo_mean, group = 1)) +
    geom_point(colour = "red") +
    geom_errorbar(aes(ymin = lwr2, ymax = upr2)) +
    geom_point(aes(x = year, y = fit2), colour = "black") +
    theme(legend.position = "none") +
    facet_wrap(~country, ncol = 2) 
  print(p)
}
dev.off()

lm3 <- lm(hlo_mean ~ tp_ + edu_exp_ + year_1970 +
            year_1975 + year_1980 + year_1985 + year_1990 +
            year_1995 + year_2000 + year_2005 + year_2010 +
            year_2015, data = d4)
summary(lm3)
lm3predict <- as.data.frame(predict(lm3, newdata = d4, interval = "confidence")) 
colnames(lm3predict) <- paste0(colnames(lm3predict),3)

d11 <- bind_cols(d4, lm3predict) %>% 
  mutate(fark = hlo_mean - fit3)

ggplot(d11, aes(x = hlo_mean, y = fit3)) +
  geom_point()


c1 <- read.csv("./data/hlo_1970_2015.csv")

d12 <- left_join(d10, c1)

ggplot(d12, aes(x = fit2, y = hlo)) +
  geom_point()


# Without year

lm4 <- lm(hlo_mean ~ country + tp_ + edu_exp_, data = d4)
summary(lm4)

lm4predict <- as.data.frame(predict(lm4, newdata = d9, interval = "confidence")) 
colnames(lm4predict) <- paste0(colnames(lm4predict),4)

d13 <- bind_cols(d9, lm4predict) %>% 
  mutate(fark = hlo_mean - fit4)

ggplot(d13, aes(x = hlo_mean, y = fit4)) +
  geom_point()


pdf("./figures/HLO_year_predictions_tp_expenditure_model_cc.pdf")
for(i in seq(1, length(unique(d13$country)),4)){
  p <- ggplot(d13 %>% filter(country %in% unique(d13$country)[i: (i+3)]), 
              aes(x = year, y = hlo_mean, group = 1)) +
    geom_point(colour = "red") +
    geom_errorbar(aes(ymin = lwr4, ymax = upr4)) +
    geom_point(aes(x = year, y = fit4), colour = "black") +
    theme(legend.position = "none") +
    facet_wrap(~country, ncol = 2) 
  print(p)
}
dev.off()



# Without year

lm5 <- lm(hlo_mean ~ region + tp_ + edu_exp_, data = d4)
summary(lm5)

lm5predict <- as.data.frame(predict(lm5, newdata = d9, interval = "confidence")) 
colnames(lm5predict) <- paste0(colnames(lm5predict),5)

d14 <- bind_cols(d9, lm5predict) %>% 
  mutate(fark = hlo_mean - fit5)

ggplot(d14, aes(x = hlo_mean, y = fit5)) +
  geom_point()


d15 <- d4 %>% 
  left_join(d7) %>% 
  left_join(d10 %>% select(-fark)) %>% 
  left_join(d11 %>% select(-fark)) %>% 
  left_join(d13 %>% select(-fark)) %>% 
  left_join(d14 %>%select(-fark)) %>% 
  left_join(c1)

write.csv(d15, "./results/lm1_lm2_lm3_lm4_lm5.csv", row.names = F)


world <- map_data("world") %>% 
  mutate(iso = countrycode(region, "country.name", "iso3n"))

YEAR = 1985

d_temp <- d15 %>% 
  select(-region) %>% 
  filter(year == YEAR) %>% 
  full_join(world)

ggplot() +
  geom_map(data = world,
           map = world,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = d_temp,
           map = world,
           aes(fill = hlo, map_id = region),
           color="#ffffff", size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  ggtitle("HLO 1970")
