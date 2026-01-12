estab <- read.csv("Data/establishmentANDenviromental.csv")

library(tidyverse)

##do environmental factors affect germination? 

june <- estab %>% 
  filter(month == 6)
july <- estab %>% 
  filter(month == 7)
august <- estab %>% 
  filter(month == 8)
september <- estab %>% 
  filter(month == 9)

month.list <- list(june, july, august, september)
## looking at all tracked months, but for survival, July and Aug
## are probably the most important. 

## Using binomial response to germination and glm with binomial
## errors and logit link function like in Hoecker et al. 

for (i in month.list) { 
  print( summary(glm(survival.binary ~ temp.soil.max.monthly, 
                     data = subset(i, Species == "psme"), 
                     family = binomial(link = "logit"))))
}


for (i in month.list) { 
  print( summary(glm(survival.binary ~ temp.ground.min.monthly, 
                     data = subset(i, Species == "psme"),  
                     family = binomial(link = "logit"))))
}


for (i in month.list) { 
  print( summary(glm(survival.binary ~ temp.air.min.monthly, 
                     data = subset(i, Species == "psme"), 
                     family = binomial(link = "logit"))))
}


for (i in month.list) { 
  print( summary(glm(survival.binary ~ moisture.soil.min.monthly, 
                     data = subset(i, Species == "psme"), 
                     family = binomial(link = "logit"))))
}


