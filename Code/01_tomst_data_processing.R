## setup
library(tidyverse)

## reading in tomst data
tomst_files <- c("Data/Tomst Data/cr01_calibrated_tomst.csv",
                 "Data/Tomst Data/cr02_calibrated_tomst.csv",
                 "Data/Tomst Data/cr03_calibrated_tomst.csv",
                 "Data/Tomst Data/cr04_calibrated_tomst.csv",
                 "Data/Tomst Data/cr05_calibrated_tomst.csv",
                 "Data/Tomst Data/cr06_calibrated_tomst.csv",
                 "Data/Tomst Data/cr07_calibrated_tomst.csv",
                 "Data/Tomst Data/cr08_calibrated_tomst.csv",
                 "Data/Tomst Data/cr09_calibrated_tomst.csv",
                 "Data/Tomst Data/cr10_calibrated_tomst.csv",
                 "Data/Tomst Data/cr11_calibrated_tomst.csv",
                 "Data/Tomst Data/cr12_calibrated_tomst.csv")

tomst<- read_delim(tomst_files, 
                   delim = ";", 
                   id = "file", 
                   col_names = c("index", "date.time", 
                                 "temp.soil.c", "temp.ground.c", "temp.air.c", 
                                 "soil.moisture.content.percentVol",
                                 "shake", "errFlag"), 
                   skip = 1)

## adding column for SiteID
tomst<- tomst %>% mutate(SiteID = 
  ifelse(file == "Data/Tomst Data/cr01_calibrated_tomst.csv", "CR01", 
         ifelse(file == "Data/Tomst Data/cr02_calibrated_tomst.csv","CR02", 
                ifelse(file == "Data/Tomst Data/cr03_calibrated_tomst.csv","CR03",
                       ifelse(file == "Data/Tomst Data/cr04_calibrated_tomst.csv","CR04", 
                              ifelse(file == "Data/Tomst Data/cr05_calibrated_tomst.csv","CR05", 
                                     ifelse(file == "Data/Tomst Data/cr06_calibrated_tomst.csv","CR06", 
                                            ifelse(file == "Data/Tomst Data/cr07_calibrated_tomst.csv","CR07", 
                                                   ifelse(file == "Data/Tomst Data/cr08_calibrated_tomst.csv","CR08", 
                                                          ifelse(file == "Data/Tomst Data/cr09_calibrated_tomst.csv","CR09", 
                                                                 ifelse(file == "Data/Tomst Data/cr10_calibrated_tomst.csv","CR10", 
                                                                        ifelse(file == "Data/Tomst Data/cr11_calibrated_tomst.csv","CR11", 
                                                                               "CR12"))))))))))),
  .before = index
)

## removing "file" column
tomst<- tomst[, -c(1)]


## separating date and time into separate columns
tomst<- tomst |> separate(date.time, c("date", "time"), sep = " ")
tomst$date<-ymd(tomst$date)

## filtering out dates that the logger wasn't installed 
tomst<- tomst[(tomst$date > "2025-06-23" & tomst$date < "2025-09-15"),]

## CR08 dislogdged, the data suggest it was on 7/28/25 at 21:00. Removing 
## data after that

#making new df that excludes CR08 entirely
tomst.no08<- tomst |> filter(
  SiteID != "CR08")

#making new df that only include CR08 before the dislodge
tomst.08nodislodge <- tomst |> filter(
  SiteID == "CR08" & date < "2025-07-28"
)

#merging the two new dfs
tomst.new <- rbind(tomst.no08, tomst.08nodislodge)

## CR12 was dislodged too, data suggests it was on 7/12/25

#making new df without CR12
tomst.no12 <- tomst.new |> filter(
  SiteID != "CR12"
)

#making new df that includes only CR12 before dislodge
tomst.12nodislodge <- tomst.new |>
  filter(SiteID == "CR12" & date < "2025-07-10")

#merging into final tomst data 
tomst.final <- rbind(tomst.no12, tomst.12nodislodge)

## Starting all data collection times on 6/26 since that was the 
## first full day for all plots, and ending on 9/13 since that was 
## when we started removing them
tomst.final <- tomst.final |> filter(
  date > "2025-06-25" & date < "2025-09-13"
)

## now that I have only the data I need, I'm going to split up the date column
## so I can calculate daily/ monthly averages
tomst.final <-  tomst.final |> 
  separate(date, c("year", "month", "day"), sep = "-", remove = FALSE)

write_csv(tomst.final, "Data/Tomst Data/tomst.csv")


