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

## getting germination and survival means by species, by season

#Spring germination means by species
germ %>% group_by(Species) %>% 
  summarize(mean.germ= mean(germ.rate.spring, na.rm = TRUE), SE.germ= std.error(germ.rate.spring, na.rm = TRUE), 
            min.germ= min(germ.rate.spring, na.rm = TRUE), med.germ= median(germ.rate.spring, na.rm = TRUE), 
            max.germ= max(germ.rate.spring, na.rm = TRUE), n= n())

#Total germination by species
germ %>% group_by(Species) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

#Spring survival means by species
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring), SE.surv= std.error(surv.rate.spring), 
            min.surv= min(surv.rate.spring), med.surv= median(surv.rate.spring), 
            max.surv= max(surv.rate.spring), n= n())

#survival (total) means by species
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.fall, na.rm= TRUE), SE.surv= std.error(surv.rate.fall, na.rm= TRUE), 
            min.surv= min(surv.rate.fall, na.rm= TRUE), med.surv= median(surv.rate.fall, na.rm= TRUE), 
            max.surv= max(surv.rate.fall, na.rm= TRUE), n= n())
