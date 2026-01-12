## setup
library(tidyverse)
library(mgcv)
library(EWSmethods)

## Loading in data frames
tomst.daily <- read.csv("Data/tomst.daily.csv") #daily measurements for each site (n = 12)
SiteType.daily <- read.csv("Data/SiteType.daily.csv") #mean daily measurements for each site type (n = 4)


## date is currently a character in this date frame, so I'm changing it to a date
SiteType.daily <- SiteType.daily %>% mutate(
  date = ymd(SiteType.daily$date))
tomst.daily <- tomst.daily %>% mutate(
  date = ymd(tomst.daily$date))

## Trying a gam model. I don't think I would want to include site type as a predictor 
## at this point, since I am just looking at the general trend over time. 
gam_mod <- gam(temp.soil.mean.daily ~ s(as.numeric(date)), data = tomst.daily)
summary(gam_mod)

new_data <- data.frame(date = seq(min(as.numeric(tomst.daily$date)), max(as.numeric(tomst.daily$date)),
                                  length.out = 100))
predictions <- predict(gam_mod, newdata = new_data, type = "response", se.fit = T)

ggplot()+
  geom_point(data = tomst.daily, aes(x = date, y = temp.soil.mean.daily, 
                                     color = site.type))+
  geom_line(data = data.frame(date = new_data$date, temp.soil.mean.daily = predictions$fit), 
            aes(x = date, y = temp.soil.mean.daily))

## now looking at the residuals
plot(gam_mod$fitted.values, gam_mod$residuals)

