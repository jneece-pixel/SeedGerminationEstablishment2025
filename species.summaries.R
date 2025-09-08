germ<- read.csv("germination_establishment.summaries.csv", row.names = NULL)

## Getting means of plot attributes

germ %>% summarize(mean.TRI= mean(TRI.30), SE.TRI= std.error(TRI.30), 
                   min.TRI= min(TRI.30), med.TRI= median(TRI.30), max.TRI= max(TRI.30))
germ %>% summarize(mean.TPI= mean(TPI.30), SE.TPI= std.error(TPI.30), 
                   min.TPI= min(TPI.30), med.TPI= median(TPI.30), max.TPI= max(TPI.30))
germ %>% summarize(mean.elevation= mean(elevation.m), SE.elevation= std.error(elevation.m), 
                   min.elevation= min(elevation.m), med.elevation= median(elevation.m), max.elevation= max(elevation.m))
germ %>% summarize(mean.aspect= mean(aspect), SE.aspect= std.error(aspect), 
                   min.aspect= min(aspect), med.aspect= median(aspect), max.elevation= max(aspect))

## getting germination and survival means by species, by season

library(plotrix) #for the standard error calculations

#germination means by species
germ %>% group_by(Species) %>% 
  summarize(mean.germ= mean(germ.rate.spring), SE.germ= std.error(germ.rate.spring), 
            min.germ= min(germ.rate.spring), med.germ= median(germ.rate.spring), 
            max.germ= max(germ.rate.spring), n= n())

#germination (spring or fall) by species
germ %>% group_by(Species) %>% 
  summarize(mean.germ= mean(germ.rate.fall), SE.germ= std.error(germ.rate.fall), 
            min.germ= min(germ.rate.fall), med.germ= median(germ.rate.fall), 
            max.germ= max(germ.rate.fall), n= n())

#survival (so far) means by species
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.spring), SE.surv= std.error(surv.rate.spring), 
            min.surv= min(surv.rate.spring), med.surv= median(surv.rate.spring), 
            max.surv= max(surv.rate.spring), n= n())

#survival (total) means by species
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.fall), SE.surv= std.error(surv.rate.fall), 
            min.surv= min(surv.rate.fall), med.surv= median(surv.rate.fall), 
            max.surv= max(surv.rate.fall), n= n())

#following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ %>% 
  summarize(mean= mean(canopy.cover.mean), SE.canopy= std.error(canopy.cover.mean), 
            min.canopy= min(canopy.cover.mean), med.canopy= median(canopy.cover.mean), max.canopy= max(canopy.cover.mean))
