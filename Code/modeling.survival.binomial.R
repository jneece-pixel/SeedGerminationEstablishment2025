germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(tidyverse)

## Group and Species are a blocking factor, need to include that first in model
## Since survival and germination are binary, we can use a glm with binomial errors
surv_TRI.gen.model <-  glm(survival.binary ~ Species + TRI.gen, data = germ, 
                           family = binomial(link = "logit"))
anova(surv_TRI.gen.model)
# TRI.gen is NOT significant

surv_TPI.gen.model <- glm(survival.binary ~ Species + TPI.gen, data = germ, 
                          family = binomial(link = "logit"))
anova(surv_TPI.gen.model)
#TPI. gen is NOT significant

surv_interaction.model <- glm(survival.binary ~ Species + TPI.gen*TRI.gen, data = germ, 
                              family = binomial(link = "logit"))
anova(surv_interaction.model)
# TRI*TPI is significant, but not the individuals

## Alternatively, we can separate out Species into two separate models. I think 
## this would make sense since we wouldn't expect both species to respond in the 
## same way