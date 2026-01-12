library(dplyr)

germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

## Checking for differences in germination
t.test(sqrt(germ.rate.total)~TPI.gen, data = germ)
t.test(sqrt(germ.rate.total)~TRI.gen, data = germ, 
       var.equal = F) #**Significant difference
t.test(sqrt(germ.rate.total)~Species, data = germ) #**Significant difference

## Checking for difference in survival
t.test(sqrt(surv.rate.total)~TPI.gen, data = germ)
t.test(sqrt(surv.rate.total)~TRI.gen, data = germ, var.equal = F) 
t.test(sqrt(surv.rate.total)~Species, data = germ)

## Checking for difference in establishment
t.test(sqrt(estab.rate.total)~TPI.gen, data = germ)
t.test(sqrt(estab.rate.total)~TRI.gen, data = germ, var.equal = F) 
t.test(sqrt(estab.rate.total)~Species, data = germ)

