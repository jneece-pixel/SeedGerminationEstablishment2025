## setup
library(tidyverse)
tomst.monthly.means <- read.csv("Data/tomst.monthly.means.csv")

germ <- read.csv("Data/germination_establishment.summaries.csv")

## Combining monthly tomst data with establishment data
enviro.germ <- merge(germ, tomst.monthly.means, by = c("SiteID", "site.type"))
enviro.germ$X <- NULL
enviro.germ$TRI.gen.y <- NULL
enviro.germ$TPI.gen.y <- NULL
colnames(enviro.germ)[19] <- "TRI.gen"
colnames(enviro.germ)[22] <- "TPI.gen"

write.csv(enviro.germ, "Data/establishmentANDenviromental.csv")
