germ<- read.csv("Data/germination_establishment.summaries.csv")

## Note: logistic regression doesn't require normality, so it's fine that the
## histograms aren't showing a normal distribution. 

library(dplyr)
library(ggplot2)
library(patchwork) #allows multiple figs in a frame
library(scales) #can override default breaks, labels, transformations, and palettes
library(ggrepel)

pico.germ_est<- germ%>% filter(Species== "pico")
psme.germ_est<- germ %>% filter(Species == "psme")

#visualising data with histograms
#Untransformed data
  ## PICO germ
pico.TPI.germ.hist <- ggplot(pico.germ_est, aes(x = germ.rate.total, fill = TPI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PICO Germination Rates")
pico.TRI.germ.hist <- ggplot(pico.germ_est, aes(x = germ.rate.total, fill = TRI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PICO Germination Rates")

pico.TPI.germ.hist / pico.TRI.germ.hist
#ggsave("Output/pico.germ.hist.svg")

  ## PICO survival
pico.TPI.surv.hist <- ggplot(pico.germ_est, aes(x = surv.rate.total, fill = TPI.gen))+
  geom_histogram(binwidth = 0.1)+
  labs(title = "PICO survival Rates")
pico.TRI.surv.hist <- ggplot(pico.germ_est, aes(x = surv.rate.total, fill = TRI.gen))+
  geom_histogram(binwidth = 0.1)+
  labs(title = "PICO survival Rates")

pico.TPI.surv.hist / pico.TRI.surv.hist
#ggsave("Output/pico.surv.hist.svg")

  ## PSME germ
psme.TPI.germ.hist <- ggplot(psme.germ_est, aes(x = germ.rate.total, fill = TPI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PSME Germination Rates")
psme.TRI.germ.hist <- ggplot(psme.germ_est, aes(x = germ.rate.total, fill = TRI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PSME Germination Rates")

psme.TPI.germ.hist / psme.TRI.germ.hist
#ggsave("Output/psme.germ.hist.svg")

  ## PSME survival
psme.TPI.surv.hist <- ggplot(subset(psme.germ_est, !is.na(surv.rate.total)), aes(x = surv.rate.total, fill = TPI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PSME Survival Rates")
psme.TRI.surv.hist <- ggplot(subset(psme.germ_est, !is.na(surv.rate.total)), aes(x = surv.rate.total, fill = TRI.gen))+
  geom_histogram(binwidth = 0.05)+
  labs(title = "PSME Survival Rates")

psme.TPI.surv.hist / psme.TRI.surv.hist
#ggsave("Output/psme.surv.hist.svg")

#visualising data with histograms
#logit-transformed data
## PICO germ
ggplot(pico.germ_est, aes(x = germ.rate.total.logit))+
  geom_histogram(binwidth = 0.5)
## PICO survival
ggplot(pico.germ_est, aes(x = surv.rate.total.logit))+
  geom_histogram(binwidth = 0.5)
## PSME germination
ggplot(psme.germ_est, aes(x = germ.rate.total.logit))+
  geom_histogram(binwidth = 0.5)
## PSME survival
ggplot(subset(psme.germ_est, !is.na(surv.rate.total)), aes(x = surv.rate.total.logit))+
  geom_histogram(binwidth = 0.5)

#visualizing with scatterplots
#PICO
#With TPI on the x axis and TRI as the color
pico.germ.scat <- ggplot(pico.germ_est)+
  geom_jitter(aes(x= TPI.gen, y = germ.rate.total, color = TRI.gen), width = 0.2)+
  labs(title = "PICO Germination Rates")
pico.surv.scat <- ggplot(pico.germ_est)+
  geom_jitter(aes(x= TPI.gen, y = surv.rate.total, color = TRI.gen), width = 0.2)+
  labs(title = "PICO Survival Rates")

pico.germ.scat + pico.surv.scat
#ggsave("Output/pico.scatterplot.svg")

#with TRI on the x axis and TPI as the color
pico.germ.scat.TRI <- ggplot(pico.germ_est)+
  geom_jitter(aes(x= TRI.gen, y = germ.rate.total, color = TPI.gen), width = 0.2)+
  labs(title = "PICO Germination Rates")
pico.surv.scat.TRI <- ggplot(pico.germ_est)+
  geom_jitter(aes(x= TRI.gen, y = surv.rate.total, color = TPI.gen), width = 0.2)+
  labs(title = "PICO Survival Rates")

pico.germ.scat.TRI + pico.surv.scat.TRI
#ggsave("Output/pico.scatterplot.TRI.svg")

#PSME
#With TPI on the x axis and TRI as the color
psme.germ.scat <- ggplot(psme.germ_est)+
  geom_jitter(aes(x= TPI.gen, y = germ.rate.total, color = TRI.gen), width = 0.2)+
  labs(title = "PSME Germination Rates")
psme.surv.scat <- ggplot(subset(psme.germ_est, !is.na(surv.rate.total)))+
  geom_jitter(aes(x= TPI.gen, y = surv.rate.total, color = TRI.gen,), width = 0.2)+
  labs(title = "PSME Survival Rates")

psme.germ.scat + psme.surv.scat
#ggsave("Output/psme.scatterplot.TPI.svg")

#with TRI on the x axis and TPI as the color
psme.germ.scat.TRI <- ggplot(psme.germ_est)+
  geom_jitter(aes(x= TRI.gen, y = germ.rate.total, color = TPI.gen), width = 0.2)+
  labs(title = "PSME Germination Rates")
psme.surv.scat.TRI <- ggplot(subset(psme.germ_est, !is.na(surv.rate.total)))+
  geom_jitter(aes(x= TRI.gen, y = surv.rate.total, color = TPI.gen,), width = 0.2)+
  labs(title = "PSME Survival Rates")

psme.germ.scat.TRI + psme.surv.scat.TRI
#ggsave("Output/psme.scatterplot.TRI.svg")
