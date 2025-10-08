germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

library(dplyr)

## Running regression on germination rate using logit transformation
## as in Hoecker et al. 2020. (logit of proportion of successes)
all.sp.germ.factorial<- lm(germ.rate.total.logit~TRI.gen*TPI.gen, data = germ)
summary(all.sp.germ.factorial)
anova(all.sp.germ.factorial)

## Regression for survival rate
all.sp.surv.factorial<- lm(surv.rate.total.logit~canopy.cover.mean, data = germ)
summary(all.sp.surv.factorial)
anova(all.sp.surv.factorial)
