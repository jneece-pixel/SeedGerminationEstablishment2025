germ<- read.csv("germination_establishment.summaries.csv")

library(dplyr)
pico.germ_est<- germ%>% filter(Species== "pico")
psme.germ_est<- germ %>% filter(Species == "psme")

#visualising data with histograms
hist(pico.germ_est$germ.rate.spring.logit, breaks= 12)
#definitely not normally distributed. Heavily skewed towards 0. 
hist(psme.germ_est$germ.rate.spring.logit, breaks= 12)
