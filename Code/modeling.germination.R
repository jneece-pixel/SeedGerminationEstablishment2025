germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(tidyverse)
library(lme4)
library(car)
library(MASS)
library(pscl) #for zero-inflated models

hist(germ$germination.total, breaks = 10) ## very zero inflated. If we ignore the zeros...
germ.nozero <- germ %>% filter(germination.total >0) ## ignoring zeros

## removing NAs from germination.total 
## germ.nona <- germ %>%
##   drop_na(germination.total)

basic.mod.TRI <- lm(germination.total ~ TRI.30*TPI.gen +potential.germ.total, data = germ.nozero)
hist(basic.mod.TRI$residuals) #looking at distribution of residuals

boxcox(basic.mod.TRI) #seeing which transformation is recommended by boxcox
## it suggests log transforming
basic.mod.TRI.log <- lm(log(germination.total) ~ potential.germ.total+TRI.gen, data = germ.nozero)
hist(basic.mod.TRI.log$residuals) #looking at distribution of residuals
summary(basic.mod.TRI)

## can I just use a negative binomial model? 
neg.bin.mod<-  glm.nb(germination.total~ potential.germ.total+TRI.gen, data = germ)
summary(neg.bin.mod)
hist(neg.bin.mod$residuals) #residuals do in fact appear to follow a neg
#binomial distribution
data.frame("Odds Ratios" = exp(coef(neg.bin.mod)))


## trying the zero-inflated model package
zero.inflated.mod <- zeroinfl(germination.total ~ potential.germ.total+TRI.gen, 
                              data = germ, dist = "negbin")
summary(zero.inflated.mod)
