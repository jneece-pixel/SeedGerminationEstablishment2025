#getting germination summary information
germ.data<- read.csv("germination_data.csv")
germ.plot<- read.csv("germination_data.plot_info.csv")

library(dplyr)

germ_est.summary<- germ.data %>% 
  group_by(SiteID, Group, Species) %>% 
  summarize(germination.spring= sum(germination.spring), 
            survival.spring= sum(survival.spring), 
            damage.spring= sum(tray.damage.spring), 
            potential.germ.spring= 50-sum(tray.damage.spring),
            germination.fall= sum(germination.fall), 
            survival.fall= sum(survival.fall), 
            damage.fall= sum(tray.damage.fall), 
            potential.germ.fall= 50-sum(tray.damage.fall))

plot.summary<- germ_est.summary %>% 
  group_by(SiteID, Species) %>% 
  summarize(germ.rate.spring= sum(germination.spring)/sum(potential.germ.spring), 
             surv.rate.spring= ifelse(sum(germination.spring)==0, 0, sum(survival.spring)/sum(germination.spring)),
            germ.rate.fall= sum(germination.fall)/sum(potential.germ.fall), 
            surv.rate.fall= ifelse(sum(germination.fall)==0, 0, sum(survival.fall)/sum(germination.fall)))
plot.summary<-plot.summary %>% filter(Species != "control")

germ<- merge(plot.summary, germ.plot, by= "SiteID")

#logit transforming germination and survival proportions, similar to Hoecker et
#al 2020. Logit transforms binary response data to a continuous value from 
#neg inf to pos inf by taking ln(p / 1-p)
library(car)
germ$germ.rate.spring.logit<- logit(germ$germ.rate.spring, percents= FALSE, 
                                      adjust= 1/(2*12))
germ$surv.rate.spring.logit<- logit(germ$surv.rate.spring, percents= FALSE, 
                                      adjust= 1/(2*12))
germ$germ.rate.fall.logit<- logit(germ$germ.rate.fall, percents= FALSE, 
                                    adjust= 1/(2*12))
germ$surv.rate.fall.logit<- logit(germ$surv.rate.fall, percents= FALSE, 
                                    adjust= 1/(2*12))

##following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ<-germ %>% mutate(canopy.cover.E= 100- (canopy.cover.E*1.04), 
                canopy.cover.N= 100- (canopy.cover.N*1.04),
                canopy.cover.S= 100- (canopy.cover.S*1.04),
                canopy.cover.W= 100- (canopy.cover.W*1.04))
germ$canopy.cover.mean<- rowSums(germ[, 12:15])/4

#write.csv(germ, "germination_establishment.summaries.csv")
rm(germ.data); rm(germ.plot); rm(germ.summary); rm(plot.summary)

pico.germ_est<- germ%>% filter(Species== "pico")
psme.germ_est<- germ %>% filter(Species == "psme")

#visualising data with histograms
hist(pico.germ_est$germ.rate.spring.logit, breaks= 12)
#definitely not normally distributed. Heavily skewed towards 0. 
hist(psme.germ_est$germ.rate.spring.logit, breaks= 12)

#total means for June data collection
library(plotrix)
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
