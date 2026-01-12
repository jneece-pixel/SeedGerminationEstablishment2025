germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(tidyverse)

## adding column for binary establishment
germ <- germ %>% mutate(
  establishment.binary = ifelse(estab.rate.total > 0, 1, 0), 
                                .after = germination.binary)

## Checking binomial models for establishment
estab_TRI.gen.model <- glm(establishment.binary ~ TRI.30, data = germ, 
                          family = binomial(link = "logit"))
anova(estab_TRI.gen.model)
summary(estab_TRI.gen.model)

estab_TPI.gen.model <- glm(establishment.binary ~ TPI.gen, data = germ, family = binomial(link = "logit"))
anova(estab_TPI.gen.model)
summary(estab_TPI.gen.model)

estab_interaction.model <- glm(establishment.binary ~ TPI.gen*TRI.30, data = germ, family = binomial(link = "logit"))
anova(estab_interaction.model)
summary(estab_interaction.model)
## TPI and TRI are not significant in any models

## visualizing using numeric TRI
newdata <- data.frame(TRI.30 = c(rep(seq(min(germ$TRI.30), max(germ$TRI.30), by = 0.1), 2)), 
                      TPI.gen = c(rep("convex", 109), rep("concave", 109)))

newdata$LogOdds<- predict(glm(establishment.binary ~ TRI.30+TPI.gen, data = germ, 
                      family = binomial(link = "logit")),
                  newdata= newdata)
newdata$expected.estab= exp(newdata$LogOdds) / (1+exp(newdata$LogOdds))

preddata.TRI30.binomilafrequeny<- data.frame(TRI.30, expected.germ)

ggplot(newdata)+
  geom_smooth(aes(x= TRI.30, y= expected.estab, color = TPI.gen)) +
  labs(caption = "Expected estab likelihood based on a binary germination model")
