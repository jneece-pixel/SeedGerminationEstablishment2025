## setup
library(tidyverse)
library(mgcv)
library(EWSmethods)
library(plotrix)

## Loading in data frames
tomst.daily <- read.csv("Data/tomst.daily.csv") #daily measurements for each site (n = 12)
SiteType.daily <- read.csv("Data/SiteType.daily.csv") #mean daily measurements for each site type (n = 4)
intens_all_gis <- read_csv("Data/intens.all.gis.csv")

## date is currently a character in this date frame, so I'm changing it to a date
SiteType.daily <- SiteType.daily %>% mutate(
  date = ymd(SiteType.daily$date))
tomst.daily <- tomst.daily %>% mutate(
  date = ymd(tomst.daily$date))
colnames(intens_all_gis)[1] <- "SiteID"

## Merging tomst.daily with DEM-extracted site slope, TRI, and TPI
tomst.daily <- merge(tomst.daily, intens_all_gis[, c(1,3,5:8)], by = "SiteID")
tomst.daily$X <-NULL

## First looking at soil temperature. Using all sites (n=12) to preserve variation

# Plotting mean of all three site reps
ggplot(tomst.daily, aes(x = ymd(date)))+
  geom_point(aes(y = temp.soil.mean.daily, group = site.type, color = site.type))+
  geom_smooth(aes(y = temp.soil.mean.daily), method = "loess", se = F, span = 0.1)+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Soil Temperature (C) - Daily average", 
       color = "Terrain")+
  ylim(c(10,25))+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))+
  annotate("rect", xmin = ymd("2025-06-26"), xmax = ymd("2025-09-12"), ymin = 20, 
           ymax = 25, alpha = 0.2, fill = "grey")

## Trying loess model
loess_model <- loess(temp.soil.mean.daily ~ as.numeric(date), data = tomst.daily, 
                     span = 0.1)

new_data <- data.frame(date = as.numeric(tomst.daily$date))
predictions_loess_soiltemp <- predict(loess_model, newdata =new_data, type = "response", se.fit = T)

plot(loess_model$fitted, loess_model$residuals)

## adding loess prediction to original df
tomst.daily <- tomst.daily %>% 
  mutate(loess_predictions = loess_model$fitted, 
         detrended.temp.soil.mean = temp.soil.mean.daily - loess_predictions)
## plotting loess predictions against original data
ggplot(tomst.daily, aes(x = date))+
  geom_point(aes(y = temp.soil.mean.daily, group = site.type, color = site.type))+
  geom_smooth(aes(y = loess_predictions), se = F, span = 0.1)+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(color = "Terrain", y = "mean daily soil temperature (C)", 
       x = "date")+
  ylim(c(10,25))+
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))+
  annotate("rect", xmin = ymd("2025-06-26"), xmax = ymd("2025-09-12"), ymin = 20, 
           ymax = 25, alpha = 0.2, fill = "grey")
#ggsave("Output/loess_MeanSoilTemp.svg")

## plotting detrended soil temp data
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.temp.soil.mean, color = site.type))+
  geom_smooth(aes(y = detrended.temp.soil.mean, color = site.type), method = "lm")+
  facet_wrap(~site.type)+
  labs(title = "Detrended Avg. Soil Temps", 
       color = "Terrain")+
  theme_classic()
#ggsave("Output/loess_detrendedMeanSoilTemp.svg")
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.temp.soil.mean, color = site.type))+
  geom_smooth(aes(y = detrended.temp.soil.mean, color = site.type), method = "lm")+
  labs(title = "Detrended Avg. Soil Temps", 
       color = "Terrain")+
  theme_classic()
#ggsave("Output/loess_detrendedMeanSoilTemp_overlay.svg")
ggplot(tomst.daily %>% 
  group_by(site.type) %>% 
  summarize(mean = mean(detrended.temp.soil.mean), 
            SE = plotrix::std.error(detrended.temp.soil.mean), 
            n = n()) )+
  geom_col(aes(y = mean, x = site.type, fill = site.type))+
  geom_errorbar(aes(x = site.type, y = mean, ymin = mean-SE, ymax = mean+SE, 
                    width = 0.2))+
  labs(title = "Detrended Avg. Soil Temps", 
       fill = "Terrain")+ 
  theme_classic()
#ggsave("Output/loess_residualsMeanSoilTemp.svg")

## Probably less informative, but still trying other climate variables
## repeating for ground temp

# Plotting mean of all three site reps
ggplot(tomst.daily, aes(x = ymd(date)))+
  geom_point(aes(y = temp.ground.mean.daily, group = site.type, color = site.type))+
  geom_smooth(aes(y = temp.ground.mean.daily), method = "loess", se = F, span = 0.1)+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Ground Temperature (C) - Daily average", 
       color = "Terrain", 
       x = "Date")+
  ylim(c(10,25))+
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))+
  annotate("rect", xmin = ymd("2025-06-26"), xmax = ymd("2025-09-12"), ymin = 20, 
           ymax = 25, alpha = 0.2, fill = "grey")
#ggsave("Output/loess_MeanGroundTemp.svg")

## Trying loess model
loess_model <- loess(temp.ground.mean.daily ~ as.numeric(date), data = tomst.daily, 
                     span = 0.1)

new_data <- data.frame(date = as.numeric(tomst.daily$date))
predictions_loess <- predict(loess_model, newdata =new_data, type = "response", se.fit = T)

plot(loess_model$fitted, loess_model$residuals)
## adding loess prediction to original df
tomst.daily <- tomst.daily %>% 
  mutate(loess_predictions_ground = loess_model$fitted, 
         detrended.temp.ground.mean = temp.ground.mean.daily - loess_predictions)

tomst.daily %>% 
  group_by(site.type) %>% 
  summarize(mean = mean(detrended.temp.ground.mean), 
            SE = plotrix::std.error(detrended.temp.ground.mean), 
            n = n())
ggplot(tomst.daily %>% 
         group_by(site.type) %>% 
         summarize(mean = mean(detrended.temp.ground.mean), 
                   SE = plotrix::std.error(detrended.temp.ground.mean), 
                   n = n()))+
  geom_col(aes(y = mean, x = site.type, fill = site.type))+
  geom_errorbar(aes(x = site.type, y = mean, ymin = mean-SE, ymax = mean+SE, 
                    width = 0.2))+
  labs(title = "Detrended Avg. Ground Temps", 
       fill = "Terrain")+ 
  theme_classic()+
  theme(legend.position = "bottom")
#ggsave("Output/loess_residualMeanGroundTemp.svg")

## plotting detrended ground temp data
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.temp.ground.mean, color = site.type))+
  geom_smooth(aes(y = detrended.temp.ground.mean, color = site.type), method = "lm")+
  facet_wrap(~site.type)+
  labs(title = "Detrended Avg. Ground Temps", 
       color = "Terrain")+
  theme_classic()+
  theme(legend.position = "bottom")
#ggsave("Output/loess_detrendedMeanGroundTemp.svg")
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.temp.ground.mean, color = site.type))+
  geom_smooth(aes(y = detrended.temp.ground.mean, color = site.type), method = "lm")+
  labs(title = "Detrended Avg. Ground Temps", 
       color = "Terrain")+
  theme_classic()+
  theme(legend.position = "bottom")
ggsave("Output/loess_residualMeanGroundTemp_overlay.svg")

## plotting soil moisture 
# Plotting mean of all three site reps
ggplot(tomst.daily, aes(x = ymd(date)))+
  geom_point(aes(y = moisture.soil.mean.daily, group = site.type, color = site.type))+
  geom_smooth(aes(y = moisture.soil.mean.daily), method = "loess", se = F, span = 0.1)+
  #facet_wrap(~site.type, nrow = 2, ncol = 2)+
  labs(title = "Soil Temperature (C)- Daily average", 
       color = "Terrain")+
  #ylim(c(10,25))+
  theme(legend.position = "top", 
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA))+
  annotate("rect", xmin = ymd("2025-06-26"), xmax = ymd("2025-09-12"), ymin = 0, 
           ymax = 2.5, alpha = 0.2, fill = "grey")

## Trying loess model
loess_model <- loess(moisture.soil.mean.daily ~ as.numeric(date), data = tomst.daily, 
                     span = 0.1)

new_data <- data.frame(date = as.numeric(tomst.daily$date))
predictions_loess <- predict(loess_model, newdata =new_data, type = "response", se.fit = T)

plot(loess_model$fitted, loess_model$residuals)

## adding loess prediction to original df
tomst.daily <- tomst.daily %>% 
  mutate(loess_predictions_soilmoisture = loess_model$fitted, 
         detrended.moisture.soil.mean = moisture.soil.mean.daily - loess_predictions)

## plotting detrended soil moisture data
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.moisture.soil.mean, color = site.type))+
  geom_smooth(aes(y = detrended.moisture.soil.mean, color = site.type), method = "lm")+
  facet_wrap(~site.type)+
  labs(title = "Detrended Avg. Soil Moisture", 
       color = "Terrain")+
  theme_classic()
ggplot(data = tomst.daily, aes(x = date,))+
  geom_point(aes(y = detrended.moisture.soil.mean, color = site.type))+
  geom_smooth(aes(y = detrended.moisture.soil.mean, color = site.type), method = "lm")+
  labs(title = "Detrended Avg. Soil Moisture", 
       color = "Terrain")+
  theme_classic()

tomst.daily %>% 
  group_by(site.type) %>% 
  summarize(mean = mean(detrended.moisture.soil.mean), 
            SE = plotrix::std.error(detrended.moisture.soil.mean), 
            n = n())

SiteType.daily.dt <- tomst.daily %>% 
  group_by(site.type, month, day) %>%
  reframe(n = n(), 
          date = date,
          temp.soil.mean = mean(temp.soil.mean.daily), 
          temp.soil.mean.dt = mean(detrended.temp.soil.mean), 
          temp.soil.se.daily = std.error(temp.soil.mean.daily), 
          
          
          temp.ground.mean = mean(temp.ground.mean.daily), 
          temp.ground.mean.dt = mean(detrended.temp.ground.mean), 
          temp.ground.se.daily = std.error(temp.ground.mean.daily),
          
          moisture.soil.mean = mean(moisture.soil.mean.daily), 
          moisture.soil.mean.dt = mean(detrended.moisture.soil.mean), 
          moisture.soil.se.daily = std.error(moisture.soil.mean.daily),
          )

SiteType.monthly.dt <- tomst.daily %>% 
  group_by(site.type, month) %>%
  reframe(n = n(), 
          temp.soil.mean = mean(temp.soil.mean.daily), 
          temp.soil.mean.dt = mean(detrended.temp.soil.mean), 
          temp.soil.se.daily = std.error(temp.soil.mean.daily), 
          
          
          temp.ground.mean = mean(temp.ground.mean.daily), 
          temp.ground.mean.dt = mean(detrended.temp.ground.mean), 
          temp.ground.se.daily = std.error(temp.ground.mean.daily),
          
          moisture.soil.mean = mean(moisture.soil.mean.daily), 
          moisture.soil.mean.dt = mean(detrended.moisture.soil.mean), 
          moisture.soil.se.daily = std.error(moisture.soil.mean.daily),
  )

SiteType.dt <- tomst.daily %>% 
  group_by(site.type) %>%
  reframe(n = n(),
          temp.soil.mean = mean(temp.soil.mean.daily), 
          temp.soil.mean.dt = mean(detrended.temp.soil.mean), 
          temp.soil.se.daily = std.error(temp.soil.mean.daily), 
          
          temp.ground.mean = mean(temp.ground.mean.daily), 
          temp.ground.mean.dt = mean(detrended.temp.ground.mean), 
          temp.ground.se.daily = std.error(temp.ground.mean.daily),
          
          moisture.soil.mean = mean(moisture.soil.mean.daily), 
          moisture.soil.mean.dt = mean(detrended.moisture.soil.mean), 
          moisture.soil.se.daily = std.error(moisture.soil.mean.daily),
  )

#write.csv(tomst.daily, "Output/detrended.tomst.daily.csv", row.names = F)
#write.csv(SiteType.daily.dt, "Output/detrended.SiteType.daily.csv", row.names = F)
#write.csv(SiteType.monthly.dt, "Output/detrended.SiteType.monthly.csv", row.names = F)
