## Setup
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
estab <- read.csv("Data/establishmentANDenviromental.csv")

## removing NA germination values (damaged)
possible.estab <- estab %>% 
  filter (germination.binary == 1 | germination.binary == 0)
possible.estab$germination.binary <- as.factor(possible.estab$germination.binary)

## plotting tomst environmental data against germination as a binary (y/n)
ggplot(possible.estab, aes(x= moisture.soil.mean.monthly, y = germ.rate.total))+
  geom_point()+
  geom_smooth(method = "lm")
#ggsave("Output/germ.binary_moisture.soil.min.svg")
