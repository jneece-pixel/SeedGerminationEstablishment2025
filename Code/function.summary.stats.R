## Making function for calculating summary stats

## setup
library(tidyverse)
library(plotrix)


calc.summary.stats <- function(x) { 
  c(mean = mean(x), 
  min = min(x),
  max = max(x),
  std.error = std.error(x))
  }



