# maps
rm(list = ls())
# install.packages(c("cowplot", "googleway", "ggrepel", 
                   # "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library(tidyverse)
library(maps)
library(countrycode)

# theme_set(theme_bw())

world <- map_data("world") %>% 
  mutate(iso = countrycode(region, "country.name", "iso3n"))
  


# # change the region names to match the region names returned by Google Trends
# world <- world %>% 
#   mutate(region = replace(region, region=="USA", "United States of America")) %>%
#   mutate(region = replace(region, region=="UK", "United Kingdom of Great Britain and Northern Ireland")) %>% 
#   mutate(region = replace(region, region=="Czech Republic", "Czechia")) %>% 
#   mutate(region = replace(region, region=="Russia", "Russian Federation"))

d1 <- read.csv("./results/results_adj_fac_hlo_1970_2015_with_adj_fct.csv")

YEAR = 2015
d2 <- d1 %>% 
   filter(year == YEAR) %>%
  rename(region = cc) 

cutpoint <- c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)

d3 <- world %>% 
  select(region, iso) %>% 
  unique()

d4 <- full_join(d2 %>% select(region, country, fit1, adj_fit, iso), d3, by = c("iso")) %>% 
  rename(SAMYS = fit1) %>% 
  mutate(Adjustment_factor = cut(adj_fit, breaks = cutpoint, na.rm= T))
# %>% 
#   right_join(world) %>% 
#   select(country, iso, cc, fit1) %>% 
#   unique()

plot_name <- paste0("./figures/world_map_fit1_", YEAR,".png")
png(plot_name, height = 5, width = 10, res = 300, units = "in")

 ggplot() +
  geom_map(data = world,
           map = world,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = d4,
           map = world,
           aes(fill = SAMYS, map_id = region.y),
           color="#ffffff", size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  ggtitle(paste0("SAMYS ", YEAR))
 dev.off()

 
plot_name <- paste0("./figures/world_map_adj_fit1_", YEAR,".png")
png(plot_name, height = 5, width = 10, res = 300, units = "in") 
 
 ggplot() +
   geom_map(data = world,
            map = world,
            aes(x = long, y = lat, map_id = region),
            fill="#ffffff", color="#ffffff", size=0.15) +
   geom_map(data = d4,
            map = world,
            aes(fill = Adjustment_factor, map_id = region.y),
            color="#ffffff", size=0.15) +
   scale_fill_brewer(palette = "BuPu", name = "Adjustment factor") +
   theme(axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank()) +
   ggtitle(paste0("Adjustment factor", YEAR))
 dev.off()
 
 
 
 
 