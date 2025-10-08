## Setup
library(dplyr)
estab <- read.csv("Data/establishmentANDenviromental.csv")


## Focusing on monthly mean temps
## Seeing if there is a correlation between TRI and enviro measurements
summary(lm(temp.soil.mean ~ TRI.30, data= estab))
summary(lm(temp.ground.mean ~ TRI.30, data= estab)) #Yes
summary(lm(temp.air.mean ~ TRI.30, data= estab))  #Yes

## Checking for TPI
summary(lm(temp.soil.mean ~ TPI.30, data= estab))
summary(lm(temp.ground.mean ~ TPI.30, data= estab)) 
summary(lm(temp.air.mean ~ TPI.30, data= estab)) #Nope for all

## Checking the TRI x TPI interaction
summary(lm(temp.soil.mean ~ TRI.30*TPI.30, data= estab))
summary(lm(temp.ground.mean ~ TRI.30*TPI.30, data= estab)) 
summary(lm(temp.air.mean ~ TRI.30*TPI.30, data= estab))  #Nope for all


## Focusing on monthly max temps
## Seeing if there is a correlation between TRI and enviro measurements
summary(lm(temp.soil.max ~ TRI.30, data= estab)) #Yes
summary(lm(temp.ground.max ~ TRI.30, data= estab))  #BIG yes
summary(lm(temp.air.max ~ TRI.30, data= estab))  #BIG yes

## Checking for TPI
summary(lm(temp.soil.max ~ TPI.30, data= estab))  #Yes
summary(lm(temp.ground.max ~ TPI.30, data= estab)) #no
summary(lm(temp.air.max ~ TPI.30, data= estab))  #no

## Checking the TRI x TPI interaction
summary(lm(temp.soil.max ~ TRI.30*TPI.30, data= estab)) #Yes
summary(lm(temp.ground.max ~ TRI.30*TPI.30, data= estab)) #interxn not significant
summary(lm(temp.air.max ~ TRI.30*TPI.30, data= estab))  #Yes

## Conclusions: Increasing TRI seems to increase mean monthly temperatures
## at ground-level and ~ 12 cm above ground-level. Monthly max temperatures
## are increased by TRI (for soil, ground, and air), TPI (for soil and 
## ground), and the TRI x TPI interaction (for soil and air). 

## How does aspect fit into things? 
summary(lm(temp.soil.mean ~ cos(aspect), data= estab))
summary(lm(temp.ground.mean ~ cos(aspect), data= estab)) 
summary(lm(temp.air.mean ~ cos(aspect), data= estab))  

summary(lm(temp.soil.max ~ cos(aspect), data= estab))
summary(lm(temp.ground.max ~ cos(aspect), data= estab)) 
summary(lm(temp.air.max ~ cos(aspect), data= estab))  


## Do the significant effects of TRI remain if aspect is also considered?
summary(lm(temp.ground.mean ~ cos(aspect)+TRI.30, data= estab))
summary(lm(temp.air.mean ~ cos(aspect)+TRI.30, data= estab))  
summary(lm(temp.soil.max ~ cos(aspect)+TRI.30, data= estab))  
summary(lm(temp.ground.max ~ cos(aspect)+TRI.30, data= estab))  
summary(lm(temp.air.max ~ cos(aspect)+TRI.30, data= estab))  


## Does the significant effect of TPI remain if aspect is also considered? 
summary(lm(temp.soil.max ~ cos(aspect)+TPI.30, data= estab))  #Yes

## TRI x TPI + Aspect
summary(lm(temp.soil.max ~ cos(aspect)+TRI.30*TPI.30, data= estab))
