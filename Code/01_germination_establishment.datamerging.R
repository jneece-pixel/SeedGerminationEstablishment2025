#getting germination summary information

## Setup 
library(dplyr)
library(readxl)


## Reading in data 
germ.data<- read_excel("Data/germination_data.xlsx", sheet= "germination_data")
germ.plot<- read_excel("Data/germination_data.xlsx", sheet = "Plot Info")

## data cleaning. removing an empty row at the end
germ.data <- germ.data[-c(9001),]

## adding column for damage to cell in either spring or fall
germ.data <- germ.data |>
  mutate(damage.any = ifelse(germ.data$tray.damage.spring == 1 | germ.data$tray.damage.fall == 1, 
                             1, 0 ) )

## getting germination, survial, and damage summaries
germ_est.summary<- germ.data %>% 
  group_by(SiteID, Group, Species) %>% 
  reframe(germination.spring= sum(germination.spring), 
          potential.germ.spring= 50-sum(tray.damage.spring),
          germination.fall.new= sum(germination.fall.new), 
          germination.total = sum(germination.spring,germination.fall.new),
          potential.germ.total = 50 - sum(damage.any),
          
          survival.spring= sum(survival.spring), 
          survival.fall= sum(survival.fall), 
          damage.spring= sum(tray.damage.spring),
          damage.any = sum(damage.any),
          
          germ.rate.spring= round(sum(germination.spring)/sum(potential.germ.spring), 3), 
          surv.rate.spring= round(ifelse(
                      sum(germination.spring)==0, 0, sum(survival.spring)/sum(germination.spring)), 3),
                    
          germ.rate.total= round(sum(germination.total)/sum(potential.germ.total), 3), 
          surv.rate.total= round(ifelse(sum(germination.total)==0, NA, 
                                        sum(survival.fall)/sum(germination.total)), 3), 
          
          estab.rate.total = round(ifelse(potential.germ.total == 0, NA, 
                                          sum(survival.fall)/ sum(potential.germ.total)), 3))

## calculating rates ##If not using Group as a block, could remove group_by(Group)

#since all control plots had no germination or survival, I'm removing those for simplicity's sake
plot.summary<-germ_est.summary %>% filter(Species != "control")

## merging germ and surv rate data with plot data. 
germ<- merge(plot.summary, germ.plot, by= "SiteID")

#logit transforming germination and survival proportions, similar to Hoecker et
#al 2020. Logit transforms binary response data to a continuous value from 
#neg inf to pos inf by taking ln(p / 1-p). Need to adjust to avoid 0 or 1 values
library(car)
germ$germ.rate.spring.logit<- logit(germ$germ.rate.spring, percents= FALSE, 
                                      adjust= 1/(2*60))
germ$surv.rate.spring.logit<- logit(germ$surv.rate.spring, percents= FALSE, 
                                      adjust= 1/(2*60))
germ$germ.rate.total.logit<- logit(germ$germ.rate.total, percents= FALSE, 
                                    adjust= 1/(2*60))
germ$surv.rate.total.logit<- logit(germ$surv.rate.total, percents= FALSE, 
                                    adjust= 1/(2*60))
germ$estab.rate.total.logit<- logit(germ$estab.rate.total, percents= FALSE, 
                                   adjust= 1/(2*60))

##following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ<-germ %>% mutate(canopy.cover.E= 100- (canopy.cover.E*1.04), 
                canopy.cover.N= 100- (canopy.cover.N*1.04),
                canopy.cover.S= 100- (canopy.cover.S*1.04),
                canopy.cover.W= 100- (canopy.cover.W*1.04))
germ$canopy.cover.mean<- rowSums(germ[, 23:26])/4

rm(germ.data); rm(germ.plot); rm(germ_est.summary); rm(plot.summary)

## cleaning the germ data frame: reordering columns, removing ones I don't need
germ <-   germ |>
  reframe(SiteID, Species, Group, 
          germ.rate.total, surv.rate.total, estab.rate.total, #overall rates
          germ.rate.total.logit, surv.rate.total.logit, estab.rate.total.logit, #logit transformed overall rates
          
          germ.rate.spring, surv.rate.spring, #spring rates
          germ.rate.spring.logit, surv.rate.spring.logit, #logit transformed spring rates
          
          germination.total, potential.germ.total, survival.total = survival.fall, damage.any,
          
          canopy.cover.mean)

#reading in plot topography data
gis.data<- read.csv("Data/intens.all.gis.csv")
gis.data<- gis.data %>% 
  rename("SiteID"= "Plot.ID", 
         "elevation.m"="Elevation..m.", 
         "TRI.30"= "TRI", 
         "TPI.30"= "TPI", 
         "aspect"= "Aspect") %>%
  mutate(TRI.gen = ifelse(TRI.30 > 7, "high", "low"),
         .after = TRI.30) 

#merging gis and summary data
germ<- merge(germ, gis.data, by= "SiteID")

write.csv(germ, "Data/germination_establishment.summaries.csv", row.names = FALSE)
