germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(tidyverse)

## Species is a blocking factor, need to include that first in model
## Since survival and germination are binary, we can use a glm with binomial errors
germ_TRI.gen.model <- glm(germination.binary ~ TRI.gen, data = germ, 
                          family = binomial(link = "logit"))
anova(germ_TRI.gen.model)
summary(germ_TRI.gen.model)

germ_TPI.gen.model <- glm(germination.binary ~ TPI.gen, data = germ, family = binomial(link = "logit"))
anova(germ_TPI.gen.model)
summary(germ_TPI.gen.model)

germ_interaction.model <- glm(germination.binary ~ TPI.gen*TRI.gen, data = germ, family = binomial(link = "logit"))
anova(germ_interaction.model)
summary(germ_interaction.model)

## visualizing but using the numeric TRI
TRI.30<- c(rep(seq(min(germ$TRI.30), max(germ$TRI.30), by = 0.1), 2))
#Species <- c(rep("pico", 109), rep("psme", 109))
LogOdds<- predict(glm(germination.binary ~ TRI.30, data = germ, family = binomial(link = "logit")),
                  newdata= data.frame(X = TRI.30))
expected.germ= exp(LogOdds) / (1+exp(LogOdds))

preddata.TRI30.binomilafrequeny<- data.frame(TRI.30, expected.germ)
ggplot(preddata.TRI30.binomilafrequeny, aes(x= TRI.30, y= expected.germ))+
  geom_smooth() +
  labs(caption = "Expected germination likelihood based on a binary germination model")

## visualizing using numeric TPI
TPI.30<- c(rep(seq(min(germ$TPI.30), max(germ$TPI.30), by = 0.1), 2))
Species <- c(rep("pico", 114), rep("psme", 114))
LogOdds<- predict(glm(germination.binary ~ Species+TPI.30, data = germ, family = binomial(link = "logit")),
                  newdata= data.frame(X = TPI.30))
expected.germ= exp(LogOdds) / (1+exp(LogOdds))

preddata.TPI30pico.binomilafrequeny<- data.frame(TPI.30, Species, expected.germ)
ggplot(preddata.TPI30pico.binomilafrequeny, aes(x= TPI.30, y= expected.germ))+
  geom_smooth(aes(color = Species)) +
  labs(caption = "Expected germination likelihood based on a binary germination model")
## both species more likely to germinate in concave terrain