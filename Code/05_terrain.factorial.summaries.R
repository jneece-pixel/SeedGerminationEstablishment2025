germ<- read.csv("Data/germination_establishment.summaries.csv")
library(dplyr)

germ %>% group_by(Species, TPI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

germ %>% group_by(Species, TRI.gen) %>% 
  summarize(mean.germ= mean(germ.rate.total, na.rm = TRUE), SE.germ= std.error(germ.rate.total, na.rm = TRUE), 
            min.germ= min(germ.rate.total, na.rm = TRUE), med.germ= median(germ.rate.total, na.rm = TRUE), 
            max.germ= max(germ.rate.total, na.rm = TRUE), n= n())

germ %>% group_by(Species, TPI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.fall, na.rm= TRUE), SE.surv= std.error(surv.rate.fall, na.rm= TRUE), 
            min.surv= min(surv.rate.fall, na.rm= TRUE), med.surv= median(surv.rate.fall, na.rm= TRUE), 
            max.surv= max(surv.rate.fall, na.rm= TRUE), n= n())

germ %>% group_by(Species, TRI.gen) %>% 
  summarize(mean.surv= mean(surv.rate.fall, na.rm= TRUE), SE.surv= std.error(surv.rate.fall, na.rm= TRUE), 
            min.surv= min(surv.rate.fall, na.rm= TRUE), med.surv= median(surv.rate.fall, na.rm= TRUE), 
            max.surv= max(surv.rate.fall, na.rm= TRUE), n= n())
