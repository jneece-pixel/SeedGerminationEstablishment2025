#getting germination summary information
germ.data<- read.csv("germination_data.csv")
germ.plot<- read.csv("germination_data.plot_info.csv")

library(dplyr)

germ.summary<- germ.data %>% 
  group_by(SiteID, Group, Species) %>% 
  summarize(germination= sum(germination), 
            survival= sum(survival), 
            damage= sum(tray.damage), potential.germ= 50-sum(tray.damage))

plot.summary<- germ.summary %>% 
  group_by(SiteID, Species) %>% 
  summarize(germ.rate.june2025= sum(germination)/sum(potential.germ), 
             surv.rate.june2025= ifelse(sum(germination)==0, 0, sum(survival)/sum(germination)))
plot.summary<-plot.summary %>% filter(Species != "control")

germ<- merge(plot.summary, germ.plot, by= "SiteID")

#logit transforming germination and survival proportions, similar to Hoecker et
#al 2020. Logit transforms binary response data to a continuous value from 
#neg inf to pos inf by taking ln(p / 1-p)
library(car)
germ$germ.rate.june2025.logit<- logit(germ$germ.rate.june2025, percents= FALSE, 
                                      adjust= 1/(2*12))
germ$surv.rate.june2025.logit<- logit(germ$surv.rate.june2025, percents= FALSE, 
                                      adjust= 1/(2*12))

##following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ<-germ %>% mutate(canopy.cover.E= 100- (canopy.cover.E*1.04), 
                canopy.cover.N= 100- (canopy.cover.N*1.04),
                canopy.cover.S= 100- (canopy.cover.S*1.04),
                canopy.cover.W= 100- (canopy.cover.W*1.04))
germ$canopy.cover.mean<- rowSums(germ[, 12:15])/4

#organizing columns to be more intuitive
germ<-germ[, c(1:3,20,4,21,5:8,16:18,12:15,22, 9:11,19)]

#write.csv(germ, "germination.summaries.csv")
rm(germ.data); rm(germ.plot); rm(germ.summary); rm(plot.summary)

pico.germ.june2025<- germ%>% filter(Species== "pico")
psme.germ.june2025<- germ %>% filter(Species == "psme")

#visualising data with histograms
hist(pico.germ.june2025$germ.rate.june2025.logit, breaks= 12)
#definitely not normally distributed. Heavily skewed towards 0. 
hist(psme.germ.june2025$germ.rate.june2025.logit, breaks= 12)

#total means for June data collection
library(plotrix)
#germination means by species
germ %>% group_by(Species) %>% 
  summarize(mean.germ= mean(germ.rate.june2025), SE.germ= std.error(germ.rate.june2025), 
            min.germ= min(germ.rate.june2025), med.germ= median(germ.rate.june2025), 
            max.germ= max(germ.rate.june2025), n= n())
#survival (so far) means by species
germ %>% group_by(Species) %>% 
  summarize(mean.surv= mean(surv.rate.june2025), SE.surv= std.error(surv.rate.june2025), 
            min.surv= min(surv.rate.june2025), med.surv= median(surv.rate.june2025), 
            max.surv= max(surv.rate.june2025), n= n())

#Plot attributes
germ %>% summarize(mean.TRI= mean(TRI.30), SE.TRI= std.error(TRI.30), 
                   min.TRI= min(TRI.30), med.TRI= median(TRI.30), max.TRI= max(TRI.30))
germ %>% summarize(mean.TPI= mean(TPI.30), SE.TPI= std.error(TPI.30), 
                   min.TPI= min(TPI.30), med.TPI= median(TPI.30), max.TPI= max(TPI.30))
germ %>% summarize(mean.elevation= mean(elevation), SE.elevation= std.error(elevation), 
                   min.elevation= min(elevation), med.elevation= median(elevation), max.elevation= max(elevation))
germ %>% summarize(mean.aspect= mean(aspect), SE.aspect= std.error(aspect), 
                   min.aspect= min(aspect), med.aspect= median(aspect), max.elevation= max(aspect))
#following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ %>% 
  summarize(mean= mean(canopy.cover.mean), SE.canopy= std.error(canopy.cover.mean), 
            min.canopy= min(canopy.cover.mean), med.canopy= median(canopy.cover.mean), max.canopy= max(canopy.cover.mean))

#starting to look at models. Separate models for each species
#PICO germination rate (as of June 2025)
anova(lm(germ.rate.june2025.logit~shape:ruggedness, data= pico.germ.june2025))
anova(lm(germ.rate.june2025.logit~shape, data= pico.germ.june2025))
anova(lm(germ.rate.june2025.logit~ruggedness, data= pico.germ.june2025))
anova(lm(germ.rate.june2025.logit~elevation, data= pico.germ.june2025))
#NOT significant: shape, ruggedness, interaction of shape:ruggedness, aspect, 
# aspect.gen, TRI.30, TPI.30, elevation

anova(lm(germ.rate.june2025.logit~shape+ruggedness, data= pico.germ.june2025))
#shape+ruggedness is significant

summary(lm(germ.rate.june2025.logit~ruggedness*shape, data= psme.germ.june2025))
qqPlot(lm(germ.rate.june2025.logit~ruggedness*shape, data= psme.germ.june2025))
#no evidence against normality. not perfect but small sample size
