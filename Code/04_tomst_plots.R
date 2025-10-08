## setup
library(tidyverse)
source("02_tomst_summary_stats.R")

## All data
## Making plots of temperature and moisture
ggplot(tomst, 
       aes(x = date, y = temp.soil.c)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Soil Temperature (C)- every 15 mins")+
  ylim(c(0,60))

ggplot(tomst, 
       aes(x = date, y = temp.ground.c)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Temperature (C) at ground level- every 15 mins") +
  ylim(c(0,60))

ggplot(tomst, 
       aes(x = date, y = temp.air.c)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Air Temperature (C)- every 15 mins")+
  ylim(c(0,60))

ggplot(tomst, 
       aes(x = date, y = soil.moisture.content.percentVol)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID) +
  labs(title = "Soil moisture content - every 15 mins")
## something is funky with the percent soil moisture readings

## Daily Max
ggplot(tomst.daily, 
       aes(x = date, y = temp.soil.max)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Soil Temperature (C)- daily max")+
  ylim(c(0,60))

ggplot(tomst.daily, 
       aes(x = date, y = temp.ground.max)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Temperature (C) at ground level- Daily Max") +
  ylim(c(0,60))

ggplot(tomst.daily, 
       aes(x = date, y = temp.air.max)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Air Temperature (C)- Daily max")+
  ylim(c(0,60))

#ggplot(tomst, 
#       aes(x = date, y = soil.moisture.content.percentVol)) +
#  geom_line(aes(col = as.factor(month)))+
#  facet_wrap(~SiteID) +
#  labs(title = "Soil moisture content - every 15 mins")


## Daily Means
ggplot(tomst.daily, 
       aes(x = date, y = temp.soil.mean)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Soil Temperature (C)- daily mean")+
  ylim(c(0,60))

ggplot(tomst.daily, 
       aes(x = date, y = temp.ground.mean)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Temperature (C) at ground level- Daily mean") +
  ylim(c(0,60))

ggplot(tomst.daily, 
       aes(x = date, y = temp.air.mean)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Air Temperature (C)- Daily mean")+
  ylim(c(0,60))

## Daily SE
ggplot(tomst.daily, 
       aes(x = date, y = temp.soil.se)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Soil Temperature (C)- daily se")+
  ylim(c(0,0.75))

ggplot(tomst.daily, 
       aes(x = date, y = temp.ground.se)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Temperature (C) at ground level- Daily se") +
  ylim(c(0,1.75))

ggplot(tomst.daily, 
       aes(x = date, y = temp.air.se)) +
  geom_line(aes(col = as.factor(month)))+
  facet_wrap(~SiteID)+
  labs(title = "Air Temperature (C)- Daily se")+
  ylim(c(0,1.75))
