## setup
library(tidyverse)

## reading in tomst data
tomst_files <- c("Data/Tomst Data/CR01.csv",
                 "Data/Tomst Data/CR02.csv",
                 "Data/Tomst Data/CR03.csv",
                 "Data/Tomst Data/CR04.csv",
                 "Data/Tomst Data/CR05.csv",
                 "Data/Tomst Data/CR06.csv",
                 "Data/Tomst Data/CR07.csv",
                 "Data/Tomst Data/CR08.csv",
                 "Data/Tomst Data/CR09.csv",
                 "Data/Tomst Data/CR10.csv",
                 "Data/Tomst Data/CR11.csv",
                 "Data/Tomst Data/CR12.csv")

tomst<- read_delim(tomst_files, 
                   delim = ";", 
                   id = "file", 
                   col_names = c("index", "date.time", 
                                 "temp.soil.c", "temp.ground.c", "temp.air.c", 
                                 "soil.moisture",
                                 "soil.moisture.percentVol",
                                 "errFlag"), 
                   skip = 1)

## adding column for SiteID
tomst<- tomst %>% mutate(SiteID = 
  ifelse(file == "Data/Tomst Data/CR01.csv", "CR01", 
         ifelse(file == "Data/Tomst Data/CR02.csv","CR02", 
                ifelse(file == "Data/Tomst Data/CR03.csv","CR03",
                       ifelse(file == "Data/Tomst Data/CR04.csv","CR04", 
                              ifelse(file == "Data/Tomst Data/CR05.csv","CR05", 
                                     ifelse(file == "Data/Tomst Data/CR06.csv","CR06", 
                                            ifelse(file == "Data/Tomst Data/CR07.csv","CR07", 
                                                   ifelse(file == "Data/Tomst Data/CR08.csv","CR08", 
                                                          ifelse(file == "Data/Tomst Data/CR09.csv","CR09", 
                                                                 ifelse(file == "Data/Tomst Data/CR10.csv","CR10", 
                                                                        ifelse(file == "Data/Tomst Data/CR11.csv","CR11", 
                                                                               "CR12"))))))))))),
  .before = index
)

## removing "file" and "errFlag" columns
tomst<- tomst[, -c(1, 10)]


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


## Adding column for factorial combination
tomst.final <- tomst.final %>% 
  mutate(site.type = ifelse(SiteID == "CR01" | SiteID =="CR02" | SiteID =="CR03", "convex.rugged", 
                            ifelse(SiteID == "CR04" | SiteID =="CR05"| SiteID =="CR06", "convex.gentle", 
                                   ifelse(SiteID == "CR07" | SiteID =="CR08" | SiteID =="CR09", "concave.rugged",
                                          "concave.gentle"))))

#write_csv(tomst.final, "Data/Tomst Data/tomst.csv")
