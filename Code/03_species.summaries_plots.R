## Setup
library(tidyverse)
library(plotrix) #this is for the standard error function
library(scales)

source("Code/02_species.summaries.R")

## Total germination
ggplot(total.germ.means) +
  geom_col(aes(y = mean.germ, x = TRI.gen, 
               fill = TPI.gen), position = "dodge")+
  geom_errorbar(aes(x = TRI.gen, ymin = mean.germ- SE.germ, 
                    ymax = mean.germ +SE.germ, group = TPI.gen),
                position = position_dodge(0.9), width = 0.2)+
  labs(x = "Terrain Ruggedness Level", 
       y = "Mean Germination Rate", 
       fill = "Topographic Position")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
#ggsave("Output/total.germ.means.svg")

## Total survival
ggplot(total.surv.means) +
  geom_col(aes(y = mean.surv, x = TRI.gen, 
               fill = TPI.gen), position = "dodge")+
  geom_errorbar(aes(x = TRI.gen, ymin = mean.surv- SE.surv, 
                    ymax = mean.surv +SE.surv, group = TPI.gen),
                position = position_dodge(0.9), width = 0.2)+ 
  labs(x = "Terrain Ruggedness Level", 
       y = "Mean Survival Rate", 
       fill = "Topographic Position")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
#ggsave("Output/total.surv.means.svg")

## Total establishment
ggplot(estab.means) +
  geom_col(aes(y = mean.estab, x = TRI.gen, 
               fill = TPI.gen), position = "dodge")+
  geom_errorbar(aes(x = TRI.gen, ymin = mean.estab- SE.estab, 
                    ymax = mean.estab +SE.estab, group = TPI.gen),
                position = position_dodge(0.9), width = 0.2)+
  labs(x = "Terrain Ruggedness Level", 
       y = "Mean Establishment Rate", 
       fill = "Topographic Position")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
#ggsave("Output/total.estab.means.svg")
