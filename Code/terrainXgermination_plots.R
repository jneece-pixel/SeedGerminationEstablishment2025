library(tidyverse)

germ.summary <- read.csv("Data/germination_establishment.summaries.csv") #summary by group

## making plot of germination rate by TRI
ggplot(data = germ.summary) +
  geom_point(aes(x = TRI.30, y = sqrt(germ.rate.total), color = TPI.gen))+
  geom_smooth(aes(x = TRI.30, y = sqrt(germ.rate.total)), method = lm)+
  labs(x = "Terrain Ruggedness Index", y = "Square root - transformed germination rate", 
       color = "Topographic position")+
  theme_classic()
#ggsave("Output/germ.rate.TRI.svg")

## making plot of germination rate by TPI
ggplot(data = germ.summary) +
  geom_histogram(aes(x = germ.rate.total), bins = 10)+
  facet_wrap(~TPI.gen)
  labs(x = "Terrain Ruggedness Index", y = "Square root - transformed germination rate", 
       color = "Topographic position")+
  theme_classic()
#ggsave("Output/germ.rate.TRI.svg")
