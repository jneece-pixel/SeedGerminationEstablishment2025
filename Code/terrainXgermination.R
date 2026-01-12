## Setup
library(tidyverse)
library(lme4)
library(MASS)

## reading in data
germ<- read.csv("Data/germination_establishment.summaries.csv") #summary by group


## making basic model for germination
germ.mod.TRI <- lm(sqrt(germ.rate.total)  ~ 
                   Species+TRI.30 , 
                 data = germ)
summary(germ.mod.TRI)
plot(germ.mod.TRI, which = 1:2)
plot(sqrt(germ.rate.total) ~ TRI.30, data = germ)
abline(germ.mod.TRI)
 ## Increasing TRI decreases germination rate

germ.mod.TPI <- lm(sqrt(germ.rate.total) ~
                     Species+TPI.gen, 
                   data = germ)
summary(germ.mod.TPI)
## Species and TPI significant when alpha = 0.1


#### looking at interaction model 
germ.mod.interaction <- lm(sqrt(germ.rate.total) ~
                             Species+TRI.30*TPI.gen, 
                           data = germ)
Anova(germ.mod.interaction, type = 3)
## no interaction

#### looking at additive model
germ.mod.additive <- lm(sqrt(germ.rate.total) ~
                             Species+TPI.gen + TRI.30, 
                           data = germ)
summary(germ.mod.additive)
## Species, shape, and ruggedness significant additively