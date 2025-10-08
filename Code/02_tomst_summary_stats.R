## setup
library(tidyverse)
library(plotrix)

## reading in tomst data
tomst <- read.csv("Data/Tomst Data/tomst.csv")

tomst.daily <- tomst |> 
  group_by(SiteID, month, day) |>
  reframe(temp.soil.mean = mean(temp.soil.c), 
        temp.soil.min = min(temp.soil.c), 
        temp.soil.max = max(temp.soil.c), 
        temp.soil.se = std.error(temp.soil.c), 
        
        temp.ground.mean = mean(temp.ground.c), 
        temp.ground.min = min(temp.ground.c), 
        temp.ground.max = max(temp.ground.c), 
        temp.ground.se = std.error(temp.ground.c), 
        
        temp.air.mean = mean(temp.air.c), 
        temp.air.min = min(temp.air.c), 
        temp.air.max = max(temp.air.c), 
        temp.air.se = std.error(temp.air.c), 
        )
tomst.daily$date <- ymd(paste("2025-0",tomst.daily$month, "-", tomst.daily$day, sep = ""))
        
tomst.monthly <- tomst |> 
  group_by(SiteID, month) |>
  reframe(temp.soil.mean = mean(temp.soil.c), 
          temp.soil.min = min(temp.soil.c), 
          temp.soil.max = max(temp.soil.c), 
          temp.soil.se = std.error(temp.soil.c), 
          
          temp.ground.mean = mean(temp.ground.c), 
          temp.ground.min = min(temp.ground.c), 
          temp.ground.max = max(temp.ground.c), 
          temp.ground.se = std.error(temp.ground.c), 
          
          temp.air.mean = mean(temp.air.c), 
          temp.air.min = min(temp.air.c), 
          temp.air.max = max(temp.air.c), 
          temp.air.se = std.error(temp.air.c), 
  )          
tomst.monthly$date <- ymd(paste("2025-0",tomst.monthly$month, sep = ""))


#Monthly summaries 
tomst %>%
  group_by(month) %>%
  reframe(temp.soil.mean = mean(temp.soil.c), 
          temp.soil.min = min(temp.soil.c),
          temp.soil.med = median(temp.soil.c),
          temp.soil.max = max(temp.soil.c), 
          temp.soil.se = std.error(temp.soil.c), 
          n = n())
tomst %>%
  group_by(month) %>%
  reframe(          
          temp.ground.mean = mean(temp.ground.c), 
          temp.ground.min = min(temp.ground.c), 
          temp.soil.med = median(temp.soil.c),
          temp.ground.max = max(temp.ground.c), 
          temp.ground.se = std.error(temp.ground.c),
          n= n())
tomst %>%
  group_by(month) %>%
  reframe(          
          temp.air.mean = mean(temp.air.c), 
          temp.air.min = min(temp.air.c),
          temp.soil.med = median(temp.soil.c),
          temp.air.max = max(temp.air.c), 
          temp.air.se = std.error(temp.air.c), 
          n= n())
