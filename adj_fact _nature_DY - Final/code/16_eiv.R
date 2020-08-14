
# Errors in variance model
library(eivtools)
library(tidyverse)

lm1 <- lm(log(adj_factor) ~  highLS + old_dep + illiterate_prop + hlo +
            year_1970 + year_1975 + year_1980 + year_1985 + year_1990, data = r1)
vcov.mat <- vcov(lm1)[-1,-1]

rel.df <- data.frame(rv = rep(1,9), 
                     rv1 = c(0.95, rep(1,8)), 
                     rv2 = c(1, 0.95, rep(1, 7)), 
                     rv3 = c(rep(1,2), 0.95, rep(1,6)), 
                     rv4 = c(rep(1,3), 0.95, rep(1,5)),
                     rv5 = c(0.90, rep(1,8)), 
                     rv6 = c(1, 0.90, rep(1, 7)), 
                     rv7 = c(rep(1,2), 0.90, rep(1,6)), 
                     rv8 = c(rep(1,3), 0.90, rep(1,5)), 
                     rv9 = c(0.85, rep(1,8)), 
                     rv10 = c(1, 0.85, rep(1, 7)), 
                     rv11 = c(rep(1,2), 0.85, rep(1,6)), 
                     rv12 = c(rep(1,3), 0.85, rep(1,5)), 
                     var = c("highLS",  "old_dep","illiterate_prop", "hlo",
                             "year_1970", "year_1975", "year_1980", "year_1985", "year_1990")) %>% 
  gather(rv, value, -var) %>% 
  mutate(rel = c(rep(c(0.95, 0.90, 0.85), each = 36), rep(1,9)))


temp3 <- NULL

for(i in 1:length(unique(rel.df$rv))){
  temp1 <- rel.df %>% 
    filter(rv %in% unique(rel.df$rv)[i])
  rel.vector <-temp1$rel
  names(rel.vector) <- c("highLS",  "old_dep","illiterate_prop", "hlo",
                         "year_1970", "year_1975", "year_1980", "year_1985", "year_1990")
  
  eiv <- eivreg(log(adj_factor) ~  highLS + old_dep + (illiterate_prop) + hlo +
                  year_1970 + year_1975 + year_1980 + year_1985 + year_1990, 
                data = r1, 
                reliability = rel.vector) 
  temp2 <- data.frame(fitted.eiv = eiv[["fitted.values"]])
  temp3 <- bind_cols(temp3, temp2)
}


fitted.eiv <- temp3 %>% 
  select(-fitted.eiv) %>% 
  gather(model, Reliability) %>% 
  mutate(model.no = gsub("fitted.eiv", "", model), 
         rel = case_when(model.no %in% 1:4 ~ 0.95, 
                         model.no %in% 5:8 ~ 0.90, 
                         model.no %in% 9:12 ~0.85), 
         var = case_when(model.no %in% c(1,5,9) ~ "aboveLS", 
                         model.no %in% c(2,6,10) ~ "ODR", 
                         model.no %in% c(3,7,11) ~ "AIR", 
                         model.no %in% c(4,8,12) ~ "QEI")) 

fitted.eiv2 <- data.frame(Main = rep(temp3 %>% select(fitted.eiv),12)) %>% 
  gather(model, main) %>% 
  select(-model) %>% 
  bind_cols(fitted.eiv) %>% 
  mutate(model.no = as.numeric(model.no), 
         rel = factor(rel, levels = c("0.95", "0.9", "0.85"))) %>% 
  arrange(model.no)



p1 <-  ggplot(fitted.eiv2, aes(x = main, y = Reliability)) +
  geom_point() +
  xlab("Variables") +
  ylim(c(-1.6, 0.3)) +
  xlim(c(-1.6, 0.3)) +
  theme_bw() +
  facet_wrap(rel ~ var, ncol = 4)

pdf("./figures/vie.pdf")
print(p1)
dev.off()

png("./figures/vie.png")
print(p1)
dev.off()



