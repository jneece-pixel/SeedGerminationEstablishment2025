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
## looking at all tracked months, but for germination, June and July 
## are probably the most important. 

## Using binomial response to germination and glm with binomial
## errors and logit link function like in Hoecker et al. 

for (i in month.list) { 
  print( summary(glm(germination.binary ~ temp.soil.min, 
                     data = subset(i, Species == "psme"), 
                     family = "binomial")))
}

for (i in month.list) { 
  print( summary(glm(germination.binary ~ temp.ground.min, 
                     data = subset(i, Species == "psme"),  
                     family = binomial(link = "logit"))))
}

for (i in month.list) { 
  print( summary(glm(germination.binary ~ temp.air.mean, 
                     data = subset(i, Species == "psme"), 
                     family = binomial(link = "logit"))))
}


for (i in month.list) { 
  print( summary(glm(germination.binary ~ moisture.soil.mean, 
                     data = i, 
                     family = binomial(link = "logit"))))
}

