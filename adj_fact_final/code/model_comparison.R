rm(list = ls())
library(tidyverse)
d1 <- read.csv("./results/results_adj_fac_1970_2015.csv")%>%
  select(cc, iso, fit2, year)
d2 <- read.csv("./results/results_log_dummyyears.csv")%>%
  select(cc, iso, fit1, year)

r1 <- d1 %>% 
  left_join(d2)

p1 <- ggplot(r1, aes(x = fit1, y = fit2)) +
  geom_point() +
  geom_smooth() +
  ggtitle("model comparison") +
  xlab("samys estimates with age groups") +
  ylab("adj factor estimates with old dependency") +
  # geom_abline() +
  theme_bw()
png("./results/model_comparison.png", width = 5, height = 5, res = 300, units = "in")
print(p1)
dev.off()
