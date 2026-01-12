#getting germination summary information

## Setup 
library(dplyr)
library(readxl)


## Reading in data 
germ.data<- read_excel("Data/germination_data.xlsx", sheet= "germination_data")
germ.plot<- read_excel("Data/germination_data.xlsx", sheet = "Plot Info")

## data cleaning
## removing an empty row at the end and non-data columns
germ.data <- germ.data[-c(9001), -c(12:15)]

## adding column for damage to cell in either spring or fall
germ.data <- germ.data |>
  mutate(damage.any = ifelse(germ.data$tray.damage.spring == 1 | germ.data$tray.damage.fall == 1, 
                             1, 0 ), 
         nodamage.any = ifelse(damage.any == 1, 0, 1),
         .after = tray.damage.fall) %>%
  mutate(germ.any = ifelse(germ.data$germination.spring > 0 | germ.data$germination.fall.new > 0, 
                           1, 0), 
         .after = germination.fall.new) %>%
  mutate(surv.any = ifelse(germ.data$survival.spring > 1, 1, 0), 
         .after = survival.fall) #Since some cells had >1 seeds germination 
                                #through accidental double planting

## getting germination, survival, and damage summaries
germ_est.summary<- germ.data %>%
  group_by(SiteID, Species) %>% 
  reframe(n = n(), 
          germination.spring= sum(germination.spring), 
          potential.germ.spring= n-sum(tray.damage.spring),
          germination.fall.new= sum(germination.fall.new), 
          germination.total = sum(germination.spring,germination.fall.new),
          potential.germ.total = n - sum(damage.any),
          
          survival.spring= sum(survival.spring), 
          survival.fall= sum(survival.fall), 
          damage.spring= sum(tray.damage.spring),
          damage.any = sum(damage.any),
          
          germ.rate.spring= round(sum(germination.spring)/sum(potential.germ.spring), 3), 
          surv.rate.spring= ifelse(germ.rate.spring == 0, NA, 
                                         ifelse(
                      sum(germination.spring)==0, 0, 
                            round(sum(survival.spring)/sum(germination.spring),3))),
                    
          germ.rate.total= round(sum(germination.total)/sum(potential.germ.total), 3), 
          surv.rate.total= ifelse(sum(germination.total)==0, NA, 
                                        round(sum(survival.fall)/sum(germination.total), 3)), 
          
          estab.rate.total = round(ifelse(potential.germ.total == 0, NA, 
                                          sum(survival.fall)/ sum(potential.germ.total)), 3))


## calculating rates 
## If not using Group as a block, could remove group_by(Group)

#since all control plots had no germination or survival, I'm removing those for simplicity's sake
plot.summary<-germ_est.summary %>% filter(Species != "control")
germ.data <- germ.data %>% filter (Species != "control")

## merging rate data and total data with plot data. 
germ<- merge(plot.summary, germ.plot, by= "SiteID")
germ.data <- merge(germ.data, germ.plot, by = "SiteID")

#logit transforming germination and survival proportions, similar to Hoecker et
#al 2020. Logit transforms binary response data to a continuous value from 
#neg inf to pos inf by taking ln(p / 1-p). Need to adjust to avoid 0 or 1 values
#library(car)
#germ$germ.rate.spring.logit<- logit(germ$germ.rate.spring, percents= FALSE, 
#                                      adjust= 1/(2*60))
#germ$surv.rate.spring.logit<- logit(germ$surv.rate.spring, percents= FALSE, 
#                                      adjust= 1/(2*60))
#germ$germ.rate.total.logit<- logit(germ$germ.rate.total, percents= FALSE, 
#                                    adjust= 1/(2*60))
#germ$surv.rate.total.logit<- logit(germ$surv.rate.total, percents= FALSE, 
#                                    adjust= 1/(2*60))
#germ$estab.rate.total.logit<- logit(germ$estab.rate.total, percents= FALSE, 
#                                   adjust= 1/(2*60))

##following instructions on densiometer for canopy cover measurements. Multiply count of open quadrants
#by 1.04 for the percent of non-canopy cover. Take 100 minus value for estimate of canopy cover. 
germ<-germ %>% mutate(canopy.cover.E= 100- (canopy.cover.E*1.04), 
                canopy.cover.N= 100- (canopy.cover.N*1.04),
                canopy.cover.S= 100- (canopy.cover.S*1.04),
                canopy.cover.W= 100- (canopy.cover.W*1.04))
germ$canopy.cover.mean<- rowSums(germ[, 23:26])/4

germ.data <- germ.data %>% mutate(canopy.cover.E= 100- (canopy.cover.E*1.04), 
                                  canopy.cover.N= 100- (canopy.cover.N*1.04),
                                  canopy.cover.S= 100- (canopy.cover.S*1.04),
                                  canopy.cover.W= 100- (canopy.cover.W*1.04))
germ.data$canopy.cover.mean <- rowSums(germ.data[, 22:25])/4

rm(germ.plot); rm(germ_est.summary); rm(plot.summary)

## cleaning the germ data frame: 
## reordering columns, removing ones I don't need
germ <-   germ |>
  reframe(SiteID, Species, n,
          
          survival.binary = ifelse(surv.rate.total > 0, 1, 0), 
          germination.binary = ifelse(germ.rate.total > 0, 1, 0), 
          
          germ.rate.total, surv.rate.total, estab.rate.total, #overall rates
          
          germ.rate.spring, surv.rate.spring, #spring rates

          germination.total, potential.germ.total, survival.total = survival.fall, damage.any,
          
          canopy.cover.mean)

germ.data <- germ.data[, c(1:16, 28)]
  

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

#merging summary data and all data with gis
germ<- merge(germ, gis.data, by= "SiteID")
germ.data<- merge(germ.data, gis.data, by = "SiteID")

# making column for factorial combo
germ <- germ %>% 
  mutate(site.type = ifelse(SiteID == "CR01" | SiteID =="CR02" | SiteID =="CR03", "convex.rugged", 
                          ifelse(SiteID == "CR04" | SiteID =="CR05"| SiteID =="CR06", "convex.gentle", 
                                 ifelse(SiteID == "CR07" | SiteID =="CR08" | SiteID =="CR09", "concave.rugged",
                                        "concave.gentle"))))
germ.data <- germ.data %>% 
  mutate(site.type = ifelse(SiteID == "CR01" | SiteID =="CR02" | SiteID =="CR03", "convex.rugged", 
                            ifelse(SiteID == "CR04" | SiteID =="CR05"| SiteID =="CR06", "convex.gentle", 
                                   ifelse(SiteID == "CR07" | SiteID =="CR08" | SiteID =="CR09", "concave.rugged",
                                          "concave.gentle"))))


## finalizing data frames
write.csv(germ.data, "Data/germ.data.all.csv")
write.csv(germ, "Data/germination_establishment.summaries.csv", row.names = FALSE)
