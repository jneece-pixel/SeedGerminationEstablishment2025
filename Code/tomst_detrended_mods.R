library(tidyverse)

tomst.daily <- read.csv("Output/detrended.tomst.daily.csv")
tomst.daily[, c(1,3)] <- NULL

## visualizing detrended predictors

ggplot(data = tomst.daily, aes(x = date))+
  geom_point(aes(y = detrended.temp.soil.mean, color = TPI.gen))
## concave sites seem to have greater variability

ggplot(data = tomst.daily, aes(x = date))+
  geom_point(aes(y = detrended.temp.soil.mean, color = TRI))+
  geom_smooth(aes(y = detrended.temp.soil.mean, group = aspect.gen), method = "lm")+
  facet_wrap(~aspect.gen)
## when we account for aspect, it seems to be a mixed bag. Western sites get
## hotter with lower ruggedness, but SW and NW sites get hotter with higher 
## ruggedness
summary(lm(detrended.temp.soil.mean~ Aspect * TRI, data = tomst.daily))
summary(lm(temp.soil.mean.daily~ Aspect * TRI, data = tomst.daily))
