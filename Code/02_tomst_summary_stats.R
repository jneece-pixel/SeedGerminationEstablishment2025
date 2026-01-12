## setup
library(tidyverse)
library(plotrix)

## reading in tomst data
tomst <- read.csv("Data/Tomst Data/tomst.csv")

## calculating daily mean, min, and max
tomst.daily <- tomst |> 
  group_by(site.type, SiteID,month, day) |>
  reframe(n = n(),
    temp.soil.mean.daily = mean(temp.soil.c), 
        temp.soil.min.daily = min(temp.soil.c), 
        temp.soil.med.daily = median(temp.soil.c), 
        temp.soil.max.daily = max(temp.soil.c), 
        temp.soil.se.daily = std.error(temp.soil.c), 
        
        temp.ground.mean.daily = mean(temp.ground.c), 
        temp.ground.min.daily = min(temp.ground.c), 
        temp.ground.med.daily = median(temp.ground.c), 
        temp.ground.max.daily = max(temp.ground.c), 
        temp.ground.se.daily = std.error(temp.ground.c), 
        
        temp.air.mean.daily = mean(temp.air.c), 
        temp.air.min.daily = min(temp.air.c), 
        temp.air.med.daily = median(temp.air.c), 
        temp.air.max.daily = max(temp.air.c), 
        temp.air.se.daily = std.error(temp.air.c), 
        
        moisture.soil.mean.daily = mean(soil.moisture.percentVol), 
        moisture.soil.min.daily = min(soil.moisture.percentVol), 
        moisture.soil.med.daily = median(soil.moisture.percentVol),
        moisture.soil.max.daily = max(soil.moisture.percentVol), 
        moisture.soil.se.daily = std.error(soil.moisture.percentVol),
        )
tomst.daily$date <- ymd(paste("2025-0",tomst.daily$month, "-", tomst.daily$day, sep = ""))

SiteType.daily <- tomst |> 
  group_by(site.type, month, day) |>
  reframe(n = n(),
          temp.soil.mean.daily = mean(temp.soil.c), 
          temp.soil.min.daily = min(temp.soil.c), 
          temp.soil.med.daily = median(temp.soil.c), 
          temp.soil.max.daily = max(temp.soil.c), 
          temp.soil.se.daily = std.error(temp.soil.c), 
          
          temp.ground.mean.daily = mean(temp.ground.c), 
          temp.ground.min.daily = min(temp.ground.c), 
          temp.ground.med.daily = median(temp.ground.c), 
          temp.ground.max.daily = max(temp.ground.c), 
          temp.ground.se.daily = std.error(temp.ground.c), 
          
          temp.air.mean.daily = mean(temp.air.c), 
          temp.air.min.daily = min(temp.air.c), 
          temp.air.med.daily = median(temp.air.c), 
          temp.air.max.daily = max(temp.air.c), 
          temp.air.se.daily = std.error(temp.air.c), 
          
          moisture.soil.mean.daily = mean(soil.moisture.percentVol), 
          moisture.soil.min.daily = min(soil.moisture.percentVol), 
          moisture.soil.med.daily = median(soil.moisture.percentVol),
          moisture.soil.max.daily = max(soil.moisture.percentVol), 
          moisture.soil.se.daily = std.error(soil.moisture.percentVol),
  )
SiteType.daily$date <- ymd(paste("2025-0",SiteType.daily$month, "-", SiteType.daily$day, sep = ""))

        
## based on the daily values, calculating the monthly mean, min, and max
tomst.monthly.means <- tomst.daily |> 
  group_by(site.type, SiteID, month) |>
  reframe(temp.soil.mean.monthly = mean(temp.soil.mean.daily),
          temp.soil.median.monthly = median(temp.soil.mean.daily), 
          temp.soil.min.monthly = min(temp.soil.mean.daily), 
          temp.soil.max.monthly = max(temp.soil.mean.daily),
          temp.soil.se.monthly = std.error(temp.soil.mean.daily), 
          
          temp.ground.mean.monthly = mean(temp.ground.mean.daily),
          temp.ground.median.monthly = median(temp.ground.mean.daily), 
          temp.ground.min.monthly = min(temp.ground.mean.daily), 
          temp.ground.max.monthly = max(temp.ground.mean.daily),
          temp.ground.se.monthly = std.error(temp.ground.mean.daily), 
          
          temp.air.mean.monthly = mean(temp.air.mean.daily), 
          temp.air.median.monthly = median(temp.air.mean.daily), 
          temp.air.min.monthly = min(temp.air.mean.daily), 
          temp.air.max.monthly = max(temp.air.mean.daily), 
          temp.air.se.monthly = std.error(temp.air.mean.daily), 
          
          moisture.soil.mean.monthly = mean(moisture.soil.mean.daily),
          moisture.soil.median.monthly = median(moisture.soil.mean.daily), 
          moisture.soil.min.monthly = min(moisture.soil.mean.daily), 
          moisture.soil.max.monthly = max(moisture.soil.mean.daily),
          moisture.soil.se.monthly = std.error(moisture.soil.mean.daily),
  )          
tomst.monthly.means$date <- ym(paste("2025-0",tomst.monthly.means$month, sep = ""))

SiteType.monthly.means <- tomst.daily |> 
  group_by(site.type, month) |>
  reframe(temp.soil.mean.monthly = mean(temp.soil.mean.daily),
          temp.soil.median.monthly = median(temp.soil.mean.daily), 
          temp.soil.min.monthly = min(temp.soil.mean.daily), 
          temp.soil.max.monthly = max(temp.soil.mean.daily),
          temp.soil.se.monthly = std.error(temp.soil.mean.daily), 
          
          temp.ground.mean.monthly = mean(temp.ground.mean.daily),
          temp.ground.median.monthly = median(temp.ground.mean.daily), 
          temp.ground.min.monthly = min(temp.ground.mean.daily), 
          temp.ground.max.monthly = max(temp.ground.mean.daily),
          temp.ground.se.monthly = std.error(temp.ground.mean.daily), 
          
          temp.air.mean.monthly = mean(temp.air.mean.daily), 
          temp.air.median.monthly = median(temp.air.mean.daily), 
          temp.air.min.monthly = min(temp.air.mean.daily), 
          temp.air.max.monthly = max(temp.air.mean.daily), 
          temp.air.se.monthly = std.error(temp.air.mean.daily), 
          
          moisture.soil.mean.monthly = mean(moisture.soil.mean.daily),
          moisture.soil.median.monthly = median(moisture.soil.mean.daily), 
          moisture.soil.min.monthly = min(moisture.soil.mean.daily), 
          moisture.soil.max.monthly = max(moisture.soil.mean.daily),
          moisture.soil.se.monthly = std.error(moisture.soil.mean.daily),
  )          
SiteType.monthly.means$date <- ym(paste("2025-0",SiteType.monthly.means$month, sep = ""))

SiteType.monthly.means <- SiteType.monthly.means %>% 
  separate(site.type, c("TPI.gen", "TRI.gen")) %>%
  mutate(site.type = paste(TPI.gen, TRI.gen, sep = "."), .before = "TPI.gen")
SiteType.daily <- SiteType.daily %>% 
  separate(site.type, c("TPI.gen", "TRI.gen")) %>%
  mutate(site.type = paste(TPI.gen, TRI.gen, sep = "."), .before = "TPI.gen")
tomst.monthly.means <- tomst.monthly.means %>% 
  separate(site.type, c("TPI.gen", "TRI.gen")) %>%
  mutate(site.type = paste(TPI.gen, TRI.gen, sep = "."), .before = "TPI.gen")
tomst.daily <- tomst.daily %>% 
  separate(site.type, c("TPI.gen", "TRI.gen")) %>%
  mutate(site.type = paste(TPI.gen, TRI.gen, sep = "."), .before = "TPI.gen")


#write.csv(tomst.monthly.means, "Data/tomst.monthly.means.csv")
#write.csv(tomst.daily, "Data/tomst.daily.csv")
#write.csv(SiteType.monthly.means, "Data/SiteType.monthly.means.csv")
#write.csv(SiteType.daily, "Data/SiteType.daily.csv")

