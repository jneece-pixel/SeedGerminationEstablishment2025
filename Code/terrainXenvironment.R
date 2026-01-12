## Setup
library(tidyverse)
library(car)
library(mgcv)

## reading in data frames
tomst.daily.detrended <- read.csv("Output/detrended.tomst.daily.csv")
estab<- read.csv("Data/germination_establishment.summaries.csv", row.names = NULL)

## making sure the columns are formatted correctly
tomst.daily.detrended$date <- ymd(tomst.daily.detrended$date)
tomst.daily.detrended<- tomst.daily.detrended %>% 
  mutate(aspect.radians = Aspect*pi/180) # the trig functions assume radians
tomst.daily.detrended <- merge(tomst.daily.detrended, estab[, c(1, 15)], by = 
                                 "SiteID" ) # adding mean canopy cover to df

## does terrain shape and ruggedness affect microclimate?

## Including cos-transformed aspect and mean canopy cover in all models since those
## could affect temp and moisture

summary(lm(detrended.temp.soil.mean ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) # Sig interaction for detrended data
summary(lm(temp.soil.mean.daily ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) #Sig interaction w/o detrending
## When accounting for canopy cover and aspect, the interaction between terrain 
## shape and ruggedness is significant

summary(lm(detrended.temp.ground.mean ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) # sig interaction with dt data
summary(lm(temp.ground.mean.daily ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) # sig interaction w/o dt data
## When accounting for aspect and canopy cover, there is a significant interaction
## between terrain shape and ruggedness

summary(lm(detrended.moisture.soil.mean ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) # sig interaction with detrended data
summary(lm(moisture.soil.mean.daily ~ cos(aspect.radians)+canopy.cover.mean+
             TRI.gen*TPI.gen, 
           data = tomst.daily.detrended)) 
## when accounting for aspect and canopy cover, there is a significant interaction 
## between terrain shape and ruggedness




################################################################################
## OLD NOT USING ## 
#decide whether to use mean or median
enviro.data.list <- list("temp.soil.min.monthly", "temp.soil.median.monthly", "temp.soil.max.monthly", 
                      "temp.ground.min.monthly", "temp.ground.median.monthly", "temp.ground.max.monthly", 
                      "temp.air.min.monthly", "temp.air.median.monthly", "temp.air.max.monthly",
                      "moisture.soil.min.monthly", "moisture.soil.median.monthly", "moisture.soil.max.monthly")

## Running a bunch of lms for each enviro variable against terrain
## to access the models follow this formula: TRI.30.models[["temp.soil.min"]][["mod"]]

## removing June data from analysis since we only have data for a few days

#estab <- subset(estab, month != "6")

## looking at TRI * TPI interaction
TPI.TRI.gen.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  TPI <- estab[,"TPI.gen"]
  TRI <- estab[, "TRI.30"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    mod = Anova(lm(sqrt(yvar) ~ TPI*TRI), type = 3))
  TPI.TRI.gen.models.nomonth[[j]]<- var.mods
  rm(TPI); rm(TRI); rm(yvar)
}
TPI.TRI.gen.models.nomonth[["temp.air.median.monthly"]][["mod"]]

## there is no interaction for any combination, so now I'll look for additive effects

TPI.TRI.30.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  TPI <- estab[,"TPI.gen"]
  TRI <- estab[, "TRI.30"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    summary = summary(lm(sqrt(yvar) ~ TPI+TRI)),
                    mod = Anova(lm(sqrt(yvar) ~ TPI+TRI), type = 2))
  TPI.TRI.30.models.nomonth[[j]]<- var.mods
  rm(TPI); rm(TRI); rm(yvar)
}
TPI.TRI.30.models.nomonth[["temp.air.median.monthly"]][["summary"]]

## TRI alone models
# don't need to check soil moisture, since those were significant in the additive
TRI.30.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  xvar <- estab[,"TRI.30"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    mod = summary(lm(sqrt(yvar) ~ xvar)))
  TRI.30.models.nomonth[[j]]<- var.mods
  rm(xvar); rm(yvar)
}
TRI.30.models.nomonth[["moisture.soil.median.monthly"]][["mod"]]

## now for TPI alone
# don't need to look at soil moisture since that was significant in the additive
TPI.gen.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  xvar <- estab[,"TPI.gen"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    mod = summary(lm(sqrt(yvar) ~ xvar)))
  TPI.gen.models.nomonth[[j]]<- var.mods
  rm(xvar); rm(yvar)
}
TPI.gen.models.nomonth[["moisture.soil.median.monthly"]][["mod"]]

## note: I should run a type II anova when the interaction is not significant, 
## and a type III anova if the interaction is significant. 


## Checking residuals for significant models. 
plot(lm(sqrt(moisture.soil.mean.monthly) ~TPI.gen, data = estab),
     which = 1:2) 
  # residuals are definitely not normally distributed. 
  # Should I just try a t-test here instead? 
plot(lm(sqrt(moisture.soil.max.monthly) ~ TPI.30, data = estab), 
     which = 1:2)
  # Residuals not normally distributed
  # Also potentially unequal variance
plot(lm(temp.ground.mean.monthly ~ TRI.30, data = estab), 
     which = 1:2)
  # Residuals definitely not normal
  # They seem to have equal variance though!
plot(lm(temp.air.mean.monthly ~ TRI.30, data = estab), 
     which = 1:2)
  # Residuals not normal
  # They may have equal variance though!






####################################################
## breaking them up by month: probably not helpful
june <- estab %>% 
  filter(month == 6)
july <- estab %>% 
  filter(month == 7)
august <- estab %>% 
  filter(month == 8)
september <- estab %>% 
  filter(month == 9)

month.list <- list(june, july, august, september)


TPI.30.models <- list()
var.mods <- list()
month.mods <- list()
for (i in month.list) { 
  xvar <- i[,"TPI.30"]
  for (j in unique(enviro.data.list)){
    yvar <- i[,j]
    var.mods <-  list(month = head(i$month), 
                      var = head(j), 
                      mod = summary(lm(yvar ~ xvar)))
    month.mods[[j]]<- var.mods
  }
  TPI.30.models[[as.character(i[1,30])]] <- month.mods
  rm(xvar); rm(yvar)
}

## if aspect is included as a predictor first, are TRI and TPI still significant?
TRI.30.aspect.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  xvar <- estab[,"TRI.30"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    mod = anova(lm(yvar ~ cos(estab[,"aspect"]) + xvar)))
  TRI.30.aspect.models.nomonth[[j]]<- var.mods
  rm(xvar); rm(yvar)
}
TRI.30.aspect.models.nomonth[["temp.air.max.monthly"]][["mod"]]

TPI.30.aspect.models.nomonth <- list()
var.mods <- list()
for (j in unique(enviro.data.list)){
  xvar <- estab[,"TPI.30"]
  yvar <- estab[,j]
  var.mods <-  list(month = head(estab$month), 
                    var = head(j), 
                    mod = anova(lm(yvar ~ cos(estab[,"aspect"]) + xvar)))
  TPI.30.aspect.models.nomonth[[j]]<- var.mods
  rm(xvar); rm(yvar)
}
TPI.30.aspect.models.nomonth[["moisture.soil.max.monthly"]][["mod"]]



## How does aspect fit into things? 
summary(lm(temp.soil.mean.monthly ~ cos(aspect), data= estab))
summary(lm(temp.ground.mean.monthly ~ cos(aspect), data= estab)) 
summary(lm(temp.air.mean.monthly ~ cos(aspect), data= estab)) 
summary(lm(moisture.soil.mean.monthly ~ cos(aspect), data= estab)) 

summary(lm(temp.soil.max.monthly ~ cos(aspect), data= estab))
summary(lm(temp.ground.max.monthly ~ cos(aspect), data= estab)) 
summary(lm(temp.air.max.monthly ~ cos(aspect), data= estab))  
summary(lm(moisture.soil.max.monthly ~ cos(aspect), data= estab)) 

anova(lm(moisture.soil.max.monthly~TPI.gen*TRI.gen, data = estab))
