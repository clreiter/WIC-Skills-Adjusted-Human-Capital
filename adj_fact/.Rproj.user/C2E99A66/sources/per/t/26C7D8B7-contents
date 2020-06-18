
library(countrycode)
library(gganimate)
library(gifski)

d1 <- read.csv("./results/results_adj_fac_hlo_1970_2015.csv")

d2 <- d1 %>% 
  select(country, year, fit1, wic_mys, prop_20_64, GEO, iso) %>% 
  rename(SAMYS = fit1, 
         MYS = wic_mys) %>% 
  mutate(continent = countrycode(iso, "iso3n", "continent")) %>% 
  group_by(year) %>% 
  mutate(mean_samys = mean(SAMYS, na.rm = T), 
         mean_mys = mean(MYS, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(dif_samys = SAMYS- mean_samys, 
         dif_mys = MYS - mean_mys)


 p <-ggplot(d2, aes(x = dif_mys, y = dif_samys, frame = year)) + 
  geom_point(aes(colour = continent)) + 
  transition_states(year) +
  labs(title = 'Year: {closest_state}') +
  geom_abline() +
  theme_minimal() +
   scale_x_continuous(breaks = seq(-10,10,2), labels = seq(-10,10,2)) +
   scale_y_continuous(breaks = seq(-10,10,2), labels = seq(-10,10,2)) +
   xlab("MYS - mean MYS") +
   ylab("SAMYS - mean SAMYS") 
   
  
animate(p, height = 800, width =800)
anim_save("figures/dif_p.gif")


png("./figures/difference_from_mean2.png", height = 5, width = 5, units = "in", res = 300)
ggplot(d2, aes(x = dif_mys, y = dif_samys)) + 
  geom_point(aes(colour = continent), size = 0.5) + 
  geom_abline() +
  # xlim(-9,9) +
  # ylim(-9,9) +
  scale_x_continuous(breaks = seq(-10,10,2), labels = seq(-10,10,2)) +
  scale_y_continuous(breaks = seq(-10,10,2), labels = seq(-10,10,2)) +
  xlab("MYS - mean MYS") +
  ylab("SAMYS - mean SAMYS") +
  facet_wrap(~year) +
  theme_minimal()
dev.off()