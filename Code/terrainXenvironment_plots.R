## Setup
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
estab <- read.csv("Data/establishmentANDenviromental.csv")

## making plots of the environmental factors significantly 
## impacted by TRI and/or TPI

## TRI and environmental factors for all species
ggplot(data = estab, aes( x = TRI.30, y = temp.ground.mean.monthly, group = site.type)) +
  geom_point(aes( x = TRI.30, y = temp.ground.mean.monthly, color = site.type))+
  geom_smooth(method = lm) +
  #facet_wrap(~as.factor(month))+
  labs(x = "Terrain Ruggedness Index", y = "Mean monthly ground temperature (C)", 
       color = "Month")
#ggsave("Output/TRI30_GroundTempMean.svg")
ggplot(data = estab, aes( x = TRI.30, y = temp.ground.max.monthly)) +
  geom_point(aes( x = TRI.30, y = temp.ground.max.monthly, color = as.factor(month)))+
  geom_smooth(method = lm)+
  facet_wrap(~ factor(month))+
  labs(x = "Terrain Ruggedness Index", y = "Max monthly ground temperature (C)", 
       color = "Month")
#ggsave("Output/TRI30_GroundTempMax.svg")

ggplot(data = estab, aes( x = TRI.30, y = temp.air.mean.monthly)) +
  geom_point(aes( x = TRI.30, y = temp.air.mean.monthly, color = as.factor(month)))+
  geom_smooth(method = lm)+
  labs(x = "Terrain Ruggedness Index", y = "Mean monthly air temperature (C)", 
       color = "Month")
#ggsave("Output/TRI30_AirTempMean.svg")
ggplot(data = estab, aes( x = TRI.30, y = temp.air.max.monthly)) +
  geom_point(aes( x = TRI.30, y = temp.air.max.monthly, color = as.factor(month)))+
  geom_smooth(method = lm)+
  labs(x = "Terrain Ruggedness Index", y = "Max monthly air temperature (C)", 
       color = "Month")
#ggsave("Output/TRI30_AirTempMax.svg")

ggplot(data = estab, aes( x = TRI.30, y = moisture.soil.mean.monthly)) +
  geom_point(aes( x = TRI.30, y = moisture.soil.mean.monthly, color = as.factor(month)))+
  geom_smooth(method = lm) +
  labs(x = "Terrain Ruggedness Index", y = "Mean monthly soil moisture (%)", 
       color = "Month")
#ggsave("Output/TRI30_SoilMoistureMean.svg")
ggplot(data = estab, aes( x = TRI.30, y = moisture.soil.min.monthly)) +
  geom_point(aes( x = TRI.30, y = moisture.soil.min.monthly, color = as.factor(month)))+
  geom_smooth(method = lm) +
  labs(x = "Terrain Ruggedness Index", y = "Min monthly soil moisture (%)", 
       color = "Month")
#ggsave("Output/TRI30_SoilMoistureMin.svg")
ggplot(data = estab, aes( x = TRI.30, y = moisture.soil.max.monthly)) +
  geom_point(aes( x = TRI.30, y = moisture.soil.max.monthly, color = as.factor(month)))+
  geom_smooth(method = lm) +
  labs(x = "Terrain Ruggedness Index", y = "Max monthly soil moisture (%)", 
       color = "Month")
#ggsave("Output/TRI30_SoilMoistureMax.svg")


## TPI and environmental measures

ggplot(data = estab, aes( x = TPI.30, y = temp.soil.min.monthly)) +
  geom_point(aes( x = TPI.30, y = temp.soil.min.monthly, color = as.factor(month)))+
  geom_smooth(method = lm)+
  labs(x = "Topographic Position Index", y = "Min monthly soil temperature (C)", 
       color = "Month")
#ggsave("Output/TPI30_SoilTempMin.svg")

ggplot(data = estab, aes( x = TPI.30, y = moisture.soil.max.monthly)) +
  geom_point(aes( x = TPI.30, y = moisture.soil.max.monthly, color = as.factor(month)))+
  geom_smooth(method = lm)+
  labs(x = "Topographic Position Index", y = "Max monthly soil moisture (%)", 
       color = "Month")
#ggsave("Output/TPI30_SoilMoistureMax.svg")


