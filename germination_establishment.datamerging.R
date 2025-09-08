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
germ$canopy.cover.mean<- rowSums(germ[, 10:13])/4

rm(germ.data); rm(germ.plot); rm(germ_est.summary); rm(plot.summary)

#reading in plot topography data
gis.data<- read.csv("intens.all.gis.csv")
gis.data<- gis.data %>% 
  rename("SiteID"= "Plot.ID", 
         "elevation.m"="Elevation..m.", 
         "TRI.30"= "TRI", 
         "TPI.30"= "TPI", 
         "aspect"= "Aspect")

#merging gis and summary data
germ<- merge(germ, gis.data, by= "SiteID")

write.csv(germ, "germination_establishment.summaries.csv", row.names = FALSE)
