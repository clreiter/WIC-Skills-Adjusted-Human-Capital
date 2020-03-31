rm(list = ls())
library(tidyverse)
d1 <- read.csv("./results/results_adj_est.csv") %>% 
  select(cc, iso, fit2)
d2 <- read.csv("C:/Users/acer/Dropbox/research/Qualmatt/R/2015/dhs_corrected_reg/results/lm1_lm2/results_piaac_step_dhs_full_corrected_plus_pred_and_emprical_hlo_illiteracy.csv") %>% 
  select(cc, iso, fit2)%>%
  rename(fit1 = fit2)

r1 <- d1 %>% 
  left_join(d2)

p1 <- ggplot(r1, aes(x = fit1, y = fit2)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("model comparison") +
  xlab("without hlo") +
  ylab("with hlo") +
  # geom_abline() +
  theme_bw()
png("./figures/comp.png", width = 5, height = 5, res = 300, units = "in")
print(p1)
dev.off()
