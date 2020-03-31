install.packages("gridExtra")
library(gridExtra)
library(tidyverse)
data <- read.csv("C:/Users/acer/Dropbox/research/Qualmatt/piaac-pisa match/piaac_pisa_adj.csv")
require(gridExtra)
plot1 <- ggplot(data, aes(x = PISA_math_adj , y = PIAAC_Numeracy ))+
  geom_point(aes(shape=factor(PISA_year))) +
  geom_smooth(method='lm') +
  xlab("Adjusted PISA Mathematics Score") +
  ylab("PIAAC Numeracy score") +
  scale_shape_manual(values = c(15, 16, 3, 4 )) + 
  theme(axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)) + 
  theme_bw() +
  theme(legend.position="none")
plot2 <- ggplot(data, aes(x = PISA_read_adj , y = PIAAC_Literacy ))+
  geom_point(aes(shape=factor(PISA_year))) +
  geom_smooth(method='lm') +
  xlab("Adjusted PISA Reading Score") +
  ylab("PIAAC Literacy score") +
  scale_shape_manual(values = c(15, 16, 3, 4 )) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  theme_bw() +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position=c(0.5, 0.09),
        legend.direction = "horizontal")+
  theme(legend.background = element_rect(fill = "darkgray"))+
  theme(legend.title = element_blank())
grid.arrange(plot1, plot2, ncol=2)

require(gridExtra)
plot3 <- ggplot(data, aes(x = PISA_math_adj , y = PIAAC_Numeracy ))+
  geom_point(aes(shape=factor(sex))) +
  geom_smooth(method='lm') +
  xlab("Adjusted PISA Mathematics Score") +
  ylab("PIAAC Numeracy score") +
  scale_shape_manual(values = c(3, 16)) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  theme_bw() +
  theme(legend.position="none")
plot4 <- ggplot(data, aes(x = PISA_read_adj , y = PIAAC_Literacy ))+
  geom_point(aes(shape=factor(sex))) +
  geom_smooth(method='lm') +
  xlab("Adjusted PISA Reading Score") +
  ylab("PIAAC Literacy score") +
  scale_shape_manual(values = c(3, 16)) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  theme_bw() +
  theme(legend.position=c(0.65, 0.1),
        legend.direction = "horizontal")+
  theme(legend.background = element_rect(fill = "darkgray"))+
  theme(legend.title = element_blank())
grid.arrange(plot3, plot4, ncol=2)
