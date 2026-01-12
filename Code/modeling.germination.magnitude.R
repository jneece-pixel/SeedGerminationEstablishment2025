germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(tidyverse)

#making new df that only includes sites with non-zero germination
germ.true <- germ %>% 
  filter(germination.binary == 1)
# 21 of the 24 site*species combos had some germination

hist((germ.true$germ.rate.total))
#distribution is heavily right skewed, maybe use Poisson or NB distribution?
#Both require whole number responses, so I'm going to use total germination
#numbers and the number of damaged cells as a predictor. 
#Since response variance > mean (germination.total), I'll use neg binomial


## Trying negative binomial model 
library(MASS)
germination.nb <- glm.nb(germination.total ~ potential.germ.total+ 
                             TRI.30 + TPI.gen, data = germ.true)
summary(germination.nb)

newdata <- data.frame(potential.germ.total = c(rep(mean(germ.true$potential.germ.total), 218)), 
                      TRI.30 = c(rep(seq(min(germ.true$TRI.30), max(germ.true$TRI.30), by = 0.1), 2)), 
                      TPI.gen = c(rep("convex", 109), rep("concave", 109)))
newdata$predictions<- predict(germination.nb, newdata = newdata, 
                              type = "response")
newdata$pred.germ.rate <- newdata$predictions / newdata$potential.germ.total

ggplot()+
  geom_smooth(data = newdata, aes(x = TRI.30, y = pred.germ.rate, color = TPI.gen), 
              se = T) +
  geom_point(data = germ.true, aes(x = TRI.30, y = germ.rate.total, color = TPI.gen))+
  labs(y = "germination rate", x = "TRI", color = "terrain shape")+
  theme_classic()

## Not using I think ##
# using TRI.30 to get a better understanding of terrain shape
germination.poisson <- glm(germination.total ~ potential.germ.total+ 
                             TRI.30 + TPI.gen, data = germ.true, 
                           family = "poisson")
summary(germination.poisson)

newdata <- data.frame(potential.germ.total = c(rep(mean(germ.true$potential.germ.total), 218)), 
                      TRI.30 = c(rep(seq(min(germ.true$TRI.30), max(germ.true$TRI.30), by = 0.1), 2)), 
                      TPI.gen = c(rep("convex", 109), rep("concave", 109)))
newdata$predictions<- predict(germination.poisson, newdata = newdata, 
                              type = "response")
newdata$pred.germ.rate <- newdata$predictions / newdata$potential.germ.total

## plotting predictions
ggplot()+
  geom_smooth(data = newdata, aes(x = TRI.30, y = pred.germ.rate, color = TPI.gen), 
              se = T) +
  geom_point(data = germ.true, aes(x = TRI.30, y = germ.rate.total, color = TPI.gen))+
  labs(y = "germination rate", x = "TRI", color = "terrain shape")+
  theme_classic()
# the model doesn't seem to be a great fit. 
