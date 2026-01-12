## Setup
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
estab <- read.csv("Data/establishmentANDenviromental.csv")

## removing NA germination values (damaged)
possible.estab <- estab %>% 
  filter (survival.binary == 1 | survival.binary == 0)
possible.estab$survival.binary <- as.factor(possible.estab$survival.binary)

## plotting tomst environmental data against survival as a binary (y/n)
ggplot(possible.estab, aes(x= temp.air.min, y = survival.binary))+
  geom_boxplot()+
  facet_wrap(~month)
#ggsave("Output/surv.binary_temp.air.min.svg")
