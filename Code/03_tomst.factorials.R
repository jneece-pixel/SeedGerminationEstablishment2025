## setup
library(tidyverse)
library(plotrix)
library(readxl)

## reading in tomst data
tomst.daily.detrended <- read.csv("Output/detrended.tomst.daily.csv")

## calculating summary stats by factorial combo for raw and detrended data
tomst.monthly.medians <- tomst.daily.detrended |> 
  group_by(month, site.type) |>
  reframe(n = n(), 
          temp.soil.median.month = median(temp.soil.mean.daily), 
          temp.soil.min.month = min(temp.soil.mean.daily), 
          temp.soil.max.month = max(temp.soil.mean.daily),  
          
          temp.soil.median.month.dt = median(detrended.temp.soil.mean), 
          temp.soil.min.month.dt = min(detrended.temp.soil.mean), 
          temp.soil.max.month.dt = max(detrended.temp.soil.mean),
          
          
          temp.ground.median.month = median(temp.ground.mean.daily), 
          temp.ground.min.month = min(temp.ground.mean.daily), 
          temp.ground.max.month = max(temp.ground.mean.daily),  
          
          temp.ground.median.month.dt = median(detrended.temp.ground.mean), 
          temp.ground.min.month.dt = min(detrended.temp.ground.mean), 
          temp.ground.max.month.dt = max(detrended.temp.ground.mean), 
          
          
          moisture.soil.median.month = median(moisture.soil.mean.daily), 
          moisture.soil.min.month = min(moisture.soil.mean.daily), 
          moisture.soil.max.month = max(moisture.soil.mean.daily), 
          
          moisture.soil.median.month.dt = median(detrended.moisture.soil.mean), 
          moisture.soil.min.month.dt = min(detrended.moisture.soil.mean), 
          moisture.soil.max.month.dt = max(detrended.moisture.soil.mean), 
  )

tomst.medians <- tomst.daily.detrended |> 
  group_by(site.type) |>
  reframe(n = n(), 
          temp.soil.median.month = median(temp.soil.mean.daily), 
          temp.soil.min.month = min(temp.soil.mean.daily), 
          temp.soil.max.month = max(temp.soil.mean.daily),  
          
          temp.soil.median.month.dt = median(detrended.temp.soil.mean), 
          temp.soil.min.month.dt = min(detrended.temp.soil.mean), 
          temp.soil.max.month.dt = max(detrended.temp.soil.mean),
          
          
          temp.ground.median.month = median(temp.ground.mean.daily), 
          temp.ground.min.month = min(temp.ground.mean.daily), 
          temp.ground.max.month = max(temp.ground.mean.daily),  
          
          temp.ground.median.month.dt = median(detrended.temp.ground.mean), 
          temp.ground.min.month.dt = min(detrended.temp.ground.mean), 
          temp.ground.max.month.dt = max(detrended.temp.ground.mean), 
          
          
          moisture.soil.median.month = median(moisture.soil.mean.daily), 
          moisture.soil.min.month = min(moisture.soil.mean.daily), 
          moisture.soil.max.month = max(moisture.soil.mean.daily), 
          
          moisture.soil.median.month.dt = median(detrended.moisture.soil.mean), 
          moisture.soil.min.month.dt = min(detrended.moisture.soil.mean), 
          moisture.soil.max.month.dt = max(detrended.moisture.soil.mean), 
  )

