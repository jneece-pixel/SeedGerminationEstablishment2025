## setup
library(tidyverse)
source("Code/02_tomst_summary_stats.R")

germ <- read.csv("Data/germination_establishment.summaries.csv")

## Combining monthly tomst data with establishment data
enviro.germ <- merge(germ, tomst.monthly, by = "SiteID")

write.csv(enviro.germ, "Data/establishmentANDenviromental.csv")
