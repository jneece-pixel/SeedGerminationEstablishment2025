## Setup
library(tidyverse)
library(lme4)
library(MASS)

## reading in data
germ <- read.csv("Data/germination_establishment.summaries.csv") #summary by group


## making basic model for germination
estab.mod.TRI <- lm(sqrt(estab.rate.total)  ~ 
                     TRI.30 , 
                   data = germ)
summary(estab.mod.TRI)

plot(estab.mod.TRI, which = 1:2)
plot(sqrt(estab.rate.total) ~ TRI.30, data = germ)
abline(estab.mod.TRI)
# TRI significant p = 0.067

estab.mod.TPI <- lm(sqrt(estab.rate.total) ~
                     TPI.gen, 
                   data = germ)
summary(estab.mod.TPI)
plot(estab.mod.TPI, which = 1:2)
# TPI not significant

#### looking at interaction model 
estab.mod.interaction <- lm(sqrt(estab.rate.total) ~
                             TRI.30*TPI.gen, 
                           data = germ)
Anova(estab.mod.interaction, type = 3)
# interaction not significant

#### looking at additive model
estab.mod.additive <- lm(sqrt(estab.rate.total) ~
                           TRI.30+ TPI.gen, 
                        data = germ)
summary(estab.mod.additive)
## only TRI is significant
