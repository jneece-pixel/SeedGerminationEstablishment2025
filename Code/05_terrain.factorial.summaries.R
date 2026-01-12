germ<- read.csv("Data/germination_establishment.summaries.csv")
library(dplyr)
library(plotrix)

## total germination by species
germ %>% group_by(Species, TPI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

germ %>% group_by(Species, TRI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

germ %>% group_by(Species, TRI.gen, TPI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

## Total survival by species
germ %>% group_by(Species, TPI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= n())

germ %>% group_by(Species, TRI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= n())

germ %>% group_by(Species, TRI.gen, TPI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= n())

## Total establishment by species
germ %>% group_by(Species, TPI.gen) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= n())

germ %>% group_by(Species, TRI.gen) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= n())

germ %>% group_by(Species, TRI.gen, TPI.gen) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= n())
