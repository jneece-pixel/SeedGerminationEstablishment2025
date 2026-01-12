## Setup
library(dplyr)
library(plotrix) #this is for the standard error function

germ<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

## Getting means of plot attributes

germ %>% summarize(mean.TRI= mean(TRI.30), SE.TRI= std.error(TRI.30), 
                   min.TRI= min(TRI.30), med.TRI= median(TRI.30), max.TRI= max(TRI.30))
germ %>% summarize(mean.TPI= mean(TPI.30), SE.TPI= std.error(TPI.30), 
                   min.TPI= min(TPI.30), med.TPI= median(TPI.30), max.TPI= max(TPI.30))
germ %>% summarize(mean.elevation= mean(elevation.m), SE.elevation= std.error(elevation.m), 
                   min.elevation= min(elevation.m), med.elevation= median(elevation.m), max.elevation= max(elevation.m))
germ %>% summarize(mean.aspect= mean(aspect), SE.aspect= std.error(aspect), 
                   min.aspect= min(aspect), med.aspect= median(aspect), max.elevation= max(aspect))
germ %>% summarize(mean= mean(canopy.cover.mean), SE.canopy= std.error(canopy.cover.mean), 
                   min.canopy= min(canopy.cover.mean), med.canopy= median(canopy.cover.mean), max.canopy= max(canopy.cover.mean))

## Germination and survival means by species, factorial combo, and season

#Spring germination means 
spring.germ.means <- germ %>% group_by(TPI.gen, TRI.gen, Species) %>% 
  summarize(mean.germ= mean(germ.rate.spring, na.rm = TRUE), SE.germ= std.error(germ.rate.spring, na.rm = TRUE), 
            min.germ= min(germ.rate.spring, na.rm = TRUE), med.germ= median(germ.rate.spring, na.rm = TRUE), 
            max.germ= max(germ.rate.spring, na.rm = TRUE), n= n())

#Total germination
germ %>%summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())
total.germ.means <- germ %>% group_by(TPI.gen, TRI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())
germ %>% group_by(TPI.gen, Species) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())
germ %>% group_by(TRI.gen, Species) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

#Spring survival means 
spring.surv.means <- germ %>% group_by(TPI.gen, TRI.gen, Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring, na.rm = TRUE), SE.surv= std.error(surv.rate.spring, na.rm = TRUE), 
            min.surv= min(surv.rate.spring, na.rm = TRUE), med.surv= median(surv.rate.spring, na.rm = TRUE), 
            max.surv= max(surv.rate.spring, na.rm = TRUE), n= 3-sum(is.na(surv.rate.spring)))
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring, na.rm = TRUE), SE.surv= std.error(surv.rate.spring, na.rm = TRUE), 
            min.surv= min(surv.rate.spring, na.rm = TRUE), med.surv= median(surv.rate.spring, na.rm = TRUE), 
            max.surv= max(surv.rate.spring, na.rm = TRUE), n= 3-sum(is.na(surv.rate.spring)))
germ %>% group_by(TRI.gen,Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring, na.rm = TRUE), SE.surv= std.error(surv.rate.spring, na.rm = TRUE), 
            min.surv= min(surv.rate.spring, na.rm = TRUE), med.surv= median(surv.rate.spring, na.rm = TRUE), 
            max.surv= max(surv.rate.spring, na.rm = TRUE), n= 3-sum(is.na(surv.rate.spring)))
germ %>% group_by(TPI.gen,Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring, na.rm = TRUE), SE.surv= std.error(surv.rate.spring, na.rm = TRUE), 
            min.surv= min(surv.rate.spring, na.rm = TRUE), med.surv= median(surv.rate.spring, na.rm = TRUE), 
            max.surv= max(surv.rate.spring, na.rm = TRUE), n= 3-sum(is.na(surv.rate.spring)))

#survival (total) means 
total.surv.means <- germ %>% group_by(TPI.gen, TRI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= 6-sum(is.na(surv.rate.total)))
germ %>% group_by(TPI.gen, Species) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= 6-sum(is.na(surv.rate.total)))
germ %>% group_by(TRI.gen, Species) %>% 
  summarize(mean.surv= mean(surv.rate.total, na.rm= TRUE), SE.surv= std.error(surv.rate.total, na.rm= TRUE), 
            min.surv= min(surv.rate.total, na.rm= TRUE), med.surv= median(surv.rate.total, na.rm= TRUE), 
            max.surv= max(surv.rate.total, na.rm= TRUE), n= 6-sum(is.na(surv.rate.total)))

#establishment (total survival / potential germination)
estab.means <- germ %>% group_by(TPI.gen, TRI.gen) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= 6-sum(is.na(estab.rate.total)))
germ %>% group_by(Species) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= 6-sum(is.na(estab.rate.total)))
germ %>% group_by(TPI.gen, Species) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= 6-sum(is.na(estab.rate.total)))
germ %>% group_by(TRI.gen, Species) %>% 
  summarize(mean.estab= mean(estab.rate.total, na.rm= TRUE), SE.estab= std.error(estab.rate.total, na.rm= TRUE), 
            min.estab= min(estab.rate.total, na.rm= TRUE), med.estab= median(estab.rate.total, na.rm= TRUE), 
            max.estab= max(estab.rate.total, na.rm= TRUE), n= 6-sum(is.na(estab.rate.total)))
