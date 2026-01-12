## Setup
library(tidyverse)
library(lme4)

## reading in data
surv.summary <- read.csv("Data/germination_establishment.summaries.csv") #summary by group


## making interaction model for survival
surv.interaction.mod <- lm(sqrt(surv.rate.total)  ~ 
                    Species+ TRI.30*TPI.gen , 
                  data = surv.summary)
Anova(surv.interaction.mod, type = 3)
# no interaction effect, so dropping intrxn and rerunning

surv.additive.mod <- lm(sqrt(surv.rate.total)  ~ 
                          Species+TRI.30+TPI.gen , 
                        data = surv.summary)
summary(surv.additive.mod)
#no additive effects, now checking individuals
surv.TPI <- lm(sqrt(surv.rate.total)  ~ 
                          Species+TPI.gen , 
                        data = surv.summary)
summary(surv.TPI) # no effect

surv.TRI <- lm(sqrt(surv.rate.total)  ~ 
                 Species+TRI.30 , 
               data = surv.summary)
summary(surv.TRI) # no effect
qqPlot(surv.TPI)
