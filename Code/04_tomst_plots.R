## setup
library(tidyverse)
library(mgcv)
#source("Code/02_tomst_summary_stats.R")
tomst.daily.detrended <- read.csv("Output/detrended.tomst.daily.csv")
#tomst.monthly.means <- read.csv("Data/tomst.monthly.means.csv")
SiteType.daily.dt <- read.csv("Output/detrended.SiteType.daily.csv")
SiteType.monthly.dt <- read.csv("Output/detrended.SiteType.monthly.csv")

tomst.daily.detrended$date <- ymd(tomst.daily.detrended$date)
SiteType.daily.dt$date <- ymd(SiteType.daily.dt$date)

## Soil Temperature
ggplot(tomst.daily.detrended)+
  geom_point(aes(x = date, y = detrended.temp.soil.mean, group = site.type, color = SiteID))+
  facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Soil Temperature (C)- Daily average", 
       group = "Terrain")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
  #annotate("rect", xmin = "2025-06-26", xmax = "2025-09-12", ymin = 20, ymax = 25, alpha = 0.2, fill = "grey")
# Mean of all three site reps
ggplot(SiteType.daily.dt)+
  geom_point(aes(x = date, y = temp.soil.mean.dt, group = site.type, color = site.type))+
  geom_smooth(aes(x = date, y = temp.soil.mean.dt, group = site.type, color = site.type), method = "lm")+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Soil Temperature (C)- Daily average", 
       color = "Terrain")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))

## Ground Temperature
ggplot(tomst.daily.detrended) +
  geom_point(aes(x = date, y = detrended.temp.ground.mean, group = site.type, color = SiteID))+
  facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Temperature (C) at ground level- Daily average") +
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
#ggsave("Output/daily.average.ground.temp.svg")
# Mean of all three site reps
ggplot(SiteType.daily.dt, 
       aes(x = date, y = temp.ground.mean.dt, group = site.type, color = site.type)) +
  geom_point()+
  geom_smooth(method = "lm")+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Temperature (C) at ground level- Daily average", 
       color = "Terrain") +
  #ylim(c(0,25))+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
  #annotate("rect", xmin = "2025-06-26", xmax = "2025-09-12", ymin = 20, ymax = 25, alpha = 0.2, fill = "grey")


ggplot(SiteType.daily.dt, 
       aes(x = date, y = moisture.soil.mean.dt, group = site.type, color = site.type)) +
  geom_line()+
  #facet_wrap(~site.type, nrow = 2, ncol = 2) +
  labs(title = "Soil moisture content - Daily average", 
       color = "Terrain")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
  #annotate("rect", xmin = "2025-06-26", xmax = "2025-09-12", ymin = 0, ymax = 5, alpha = 0.2, fill = "grey")
ggplot(tomst.daily.detrended, 
       aes(x = date, y = detrended.moisture.soil.mean, group = site.type, color = site.type)) +
  geom_point()+
  #facet_wrap(~site.type, nrow = 2, ncol = 2) +
  labs(title = "Soil moisture content - Daily average")+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))
 # annotate("rect", xmin = "2025-06-26", xmax = "2025-09-12", ymin = 0, ymax = 5, alpha = 0.2, fill = "grey")
#ggsave("Output/daily.average.soil.moisture.svg")
