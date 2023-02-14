#Thesis
library(tidyverse)
library(lubridate)
library(moments)
library(corrplot)
library(car)
library(gridExtra)

#Ozone Stations
#can be done in a loop, but files are excessively large
#by year gives a sense of the current progress
us_o3_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2020.csv", stringsAsFactors = FALSE)
AZ_O3_20 <- us_o3_20 %>% filter(State.Code == 4)
rm(us_o3_20)
AZ_O3_20$site_id <- AZ_O3_20$State.Code*10^7 + AZ_O3_20$County.Code*10^4 + AZ_O3_20$Site.Num
AZ_O3_20$dt_local <- format(as.POSIXct(paste(AZ_O3_20$Date.Local, AZ_O3_20$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_20 <- AZ_O3_20 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2019.csv", stringsAsFactors = FALSE)
AZ_O3_19 <- us_o3_19 %>% filter(State.Code == 4)
rm(us_o3_19)
AZ_O3_19$site_id <- AZ_O3_19$State.Code*10^7 + AZ_O3_19$County.Code*10^4 + AZ_O3_19$Site.Num
AZ_O3_19$dt_local <- format(as.POSIXct(paste(AZ_O3_19$Date.Local, AZ_O3_19$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_19 <- AZ_O3_19 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2018.csv", stringsAsFactors = FALSE)
AZ_O3_18 <- us_o3_18 %>% filter(State.Code == 4)
rm(us_o3_18)
AZ_O3_18$site_id <- AZ_O3_18$State.Code*10^7 + AZ_O3_18$County.Code*10^4 + AZ_O3_18$Site.Num
AZ_O3_18$dt_local <- format(as.POSIXct(paste(AZ_O3_18$Date.Local, AZ_O3_18$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_18 <- AZ_O3_18 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2017.csv", stringsAsFactors = FALSE)
AZ_O3_17 <- us_o3_17 %>% filter(State.Code == 4)
rm(us_o3_17)
AZ_O3_17$site_id <- AZ_O3_17$State.Code*10^7 + AZ_O3_17$County.Code*10^4 + AZ_O3_17$Site.Num
AZ_O3_17$dt_local <- format(as.POSIXct(paste(AZ_O3_17$Date.Local, AZ_O3_17$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_17 <- AZ_O3_17 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_16 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2016.csv", stringsAsFactors = FALSE)
AZ_O3_16 <- us_o3_16 %>% filter(State.Code == 4)
rm(us_o3_16)
AZ_O3_16$site_id <- AZ_O3_16$State.Code*10^7 + AZ_O3_16$County.Code*10^4 + AZ_O3_16$Site.Num
AZ_O3_16$dt_local <- format(as.POSIXct(paste(AZ_O3_16$Date.Local, AZ_O3_16$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_16 <- AZ_O3_16 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_15 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2015.csv", stringsAsFactors = FALSE)
AZ_O3_15 <- us_o3_15 %>% filter(State.Code == 4)
rm(us_o3_15)
AZ_O3_15$site_id <- AZ_O3_15$State.Code*10^7 + AZ_O3_15$County.Code*10^4 + AZ_O3_15$Site.Num
AZ_O3_15$dt_local <- format(as.POSIXct(paste(AZ_O3_15$Date.Local, AZ_O3_15$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_15 <- AZ_O3_15 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_14 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2014.csv", stringsAsFactors = FALSE)
AZ_O3_14 <- us_o3_14 %>% filter(State.Code == 4)
rm(us_o3_14)
AZ_O3_14$site_id <- AZ_O3_14$State.Code*10^7 + AZ_O3_14$County.Code*10^4 + AZ_O3_14$Site.Num
AZ_O3_14$dt_local <- format(as.POSIXct(paste(AZ_O3_14$Date.Local, AZ_O3_14$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_14 <- AZ_O3_14 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_13 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2013.csv", stringsAsFactors = FALSE)
AZ_O3_13 <- us_o3_13 %>% filter(State.Code == 4)
rm(us_o3_13)
AZ_O3_13$site_id <- AZ_O3_13$State.Code*10^7 + AZ_O3_13$County.Code*10^4 + AZ_O3_13$Site.Num
AZ_O3_13$dt_local <- format(as.POSIXct(paste(AZ_O3_13$Date.Local, AZ_O3_13$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_13 <- AZ_O3_13 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_12 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2012.csv", stringsAsFactors = FALSE)
AZ_O3_12 <- us_o3_12 %>% filter(State.Code == 4)
rm(us_o3_12)
AZ_O3_12$site_id <- AZ_O3_12$State.Code*10^7 + AZ_O3_12$County.Code*10^4 + AZ_O3_12$Site.Num
AZ_O3_12$dt_local <- format(as.POSIXct(paste(AZ_O3_12$Date.Local, AZ_O3_12$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_12 <- AZ_O3_12 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_11 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2011.csv", stringsAsFactors = FALSE)
AZ_O3_11 <- us_o3_11 %>% filter(State.Code == 4)
rm(us_o3_11)
AZ_O3_11$site_id <- AZ_O3_11$State.Code*10^7 + AZ_O3_11$County.Code*10^4 + AZ_O3_11$Site.Num
AZ_O3_11$dt_local <- format(as.POSIXct(paste(AZ_O3_11$Date.Local, AZ_O3_11$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_11 <- AZ_O3_11 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

us_o3_10 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/8hour_44201_2010.csv", stringsAsFactors = FALSE)
AZ_O3_10 <- us_o3_10 %>% filter(State.Code == 4)
rm(us_o3_10)
AZ_O3_10$site_id <- AZ_O3_10$State.Code*10^7 + AZ_O3_10$County.Code*10^4 + AZ_O3_10$Site.Num
AZ_O3_10$dt_local <- format(as.POSIXct(paste(AZ_O3_10$Date.Local, AZ_O3_10$Time.Local), "%Y-%m-%d %H:%M"))
AZ_O3_10 <- AZ_O3_10 %>% select(site_id, dt_local, Mean.Excluding.All.Flagged.Data) %>% rename(O3 = Mean.Excluding.All.Flagged.Data)

write.csv(AZ_O3_20 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_20.csv")
write.csv(AZ_O3_19 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_19.csv")
write.csv(AZ_O3_18 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_18.csv")
write.csv(AZ_O3_17 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_17.csv")
write.csv(AZ_O3_16 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_16.csv")
write.csv(AZ_O3_15 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_15.csv")
write.csv(AZ_O3_14 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_14.csv")
write.csv(AZ_O3_13 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_13.csv")
write.csv(AZ_O3_12 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_12.csv")
write.csv(AZ_O3_11 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_11.csv")
write.csv(AZ_O3_10 ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_10.csv")

AZ_O3_20 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_20.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_19 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_19.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_18 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_18.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_17 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_17.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_16 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_16.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_15 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_15.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_14 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_14.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_13 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_13.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_12 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_12.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_11 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_11.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)
AZ_O3_10 <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/Ozone/AZ_O3_10.csv", stringsAsFactors = FALSE) %>% select(site_id, dt_local, O3)

AZ_ALL <- rbind(AZ_O3_20,AZ_O3_19,AZ_O3_18,AZ_O3_17,AZ_O3_16,AZ_O3_15,AZ_O3_14,AZ_O3_13,AZ_O3_12,AZ_O3_12,AZ_O3_11,AZ_O3_10)
AZ_ALL$hour <- hour(AZ_ALL$dt_local)
AZ_ALL$dom <- day(AZ_ALL$dt_local)
AZ_ALL$month <- month(AZ_ALL$dt_local)
AZ_ALL$year <- year(AZ_ALL$dt_local)

AZ_ALL_day <- AZ_ALL %>%
  group_by(site_id, dom, month,year) %>%
  summarize(avg_O3 = mean(O3, na.rm = TRUE),
            sd_O3 = sd(O3, na.rm = TRUE),
            max_O3 = max(O3, na.rm = TRUE),
            min_O3 = min(O3, na.rm = TRUE))


#AZ_40038001 <- AZ_ALL_day %>% filter(site_id == 40038001) %>% select(dom, month, year, max_O3) %>% rename(O3_40038001 = max_O3)
#AZ_40051008 <- AZ_ALL_day %>% filter(site_id == 40051008) %>% select(dom, month, year, max_O3) %>% rename(O3_40051008 = max_O3)
#AZ_40058001 <- AZ_ALL_day %>% filter(site_id == 40058001) %>% select(dom, month, year, max_O3) %>% rename(O3_40058001 = max_O3)
#AZ_40070010 <- AZ_ALL_day %>% filter(site_id == 40070010) %>% select(dom, month, year, max_O3) %>% rename(O3_40070010 = max_O3)
#AZ_40128000 <- AZ_ALL_day %>% filter(site_id == 40128000) %>% select(dom, month, year, max_O3) %>% rename(O3_40128000 = max_O3)

AZ_40130019 <- AZ_ALL_day %>% filter(site_id == 40130019) %>% select(dom, month, year, max_O3) %>% rename(O3_40130019 = max_O3)
AZ_40131003 <- AZ_ALL_day %>% filter(site_id == 40131003) %>% select(dom, month, year, max_O3) %>% rename(O3_40131003 = max_O3)
AZ_40131004 <- AZ_ALL_day %>% filter(site_id == 40131004) %>% select(dom, month, year, max_O3) %>% rename(O3_40131004 = max_O3)
AZ_40131010 <- AZ_ALL_day %>% filter(site_id == 40131010) %>% select(dom, month, year, max_O3) %>% rename(O3_40131010 = max_O3)
AZ_40132001 <- AZ_ALL_day %>% filter(site_id == 40132001) %>% select(dom, month, year, max_O3) %>% rename(O3_40132001 = max_O3)
AZ_40132005 <- AZ_ALL_day %>% filter(site_id == 40132005) %>% select(dom, month, year, max_O3) %>% rename(O3_40132005 = max_O3)
AZ_40133002 <- AZ_ALL_day %>% filter(site_id == 40133002) %>% select(dom, month, year, max_O3) %>% rename(O3_40133002 = max_O3)
AZ_40133003 <- AZ_ALL_day %>% filter(site_id == 40133003) %>% select(dom, month, year, max_O3) %>% rename(O3_40133003 = max_O3)
AZ_40134003 <- AZ_ALL_day %>% filter(site_id == 40134003) %>% select(dom, month, year, max_O3) %>% rename(O3_40134003 = max_O3)
AZ_40134004 <- AZ_ALL_day %>% filter(site_id == 40134004) %>% select(dom, month, year, max_O3) %>% rename(O3_40134004 = max_O3)
AZ_40134005 <- AZ_ALL_day %>% filter(site_id == 40134005) %>% select(dom, month, year, max_O3) %>% rename(O3_40134005 = max_O3)
AZ_40134008 <- AZ_ALL_day %>% filter(site_id == 40134008) %>% select(dom, month, year, max_O3) %>% rename(O3_40134008 = max_O3)
AZ_40134010 <- AZ_ALL_day %>% filter(site_id == 40134010) %>% select(dom, month, year, max_O3) %>% rename(O3_40134010 = max_O3)
AZ_40134011 <- AZ_ALL_day %>% filter(site_id == 40134011) %>% select(dom, month, year, max_O3) %>% rename(O3_40134011 = max_O3)
AZ_40135100 <- AZ_ALL_day %>% filter(site_id == 40135100) %>% select(dom, month, year, max_O3) %>% rename(O3_40135100 = max_O3)
AZ_40137003 <- AZ_ALL_day %>% filter(site_id == 40137003) %>% select(dom, month, year, max_O3) %>% rename(O3_40137003 = max_O3)
AZ_40137020 <- AZ_ALL_day %>% filter(site_id == 40137020) %>% select(dom, month, year, max_O3) %>% rename(O3_40137020 = max_O3)
AZ_40137021 <- AZ_ALL_day %>% filter(site_id == 40137021) %>% select(dom, month, year, max_O3) %>% rename(O3_40137021 = max_O3)
AZ_40137022 <- AZ_ALL_day %>% filter(site_id == 40137022) %>% select(dom, month, year, max_O3) %>% rename(O3_40137022 = max_O3)
AZ_40137024 <- AZ_ALL_day %>% filter(site_id == 40137024) %>% select(dom, month, year, max_O3) %>% rename(O3_40137024 = max_O3)
AZ_40139508 <- AZ_ALL_day %>% filter(site_id == 40139508) %>% select(dom, month, year, max_O3) %>% rename(O3_40139508 = max_O3)
AZ_40139702 <- AZ_ALL_day %>% filter(site_id == 40139702) %>% select(dom, month, year, max_O3) %>% rename(O3_40139702 = max_O3)
AZ_40139704 <- AZ_ALL_day %>% filter(site_id == 40139704) %>% select(dom, month, year, max_O3) %>% rename(O3_40139704 = max_O3)
AZ_40139706 <- AZ_ALL_day %>% filter(site_id == 40139706) %>% select(dom, month, year, max_O3) %>% rename(O3_40139706 = max_O3)
AZ_40139997 <- AZ_ALL_day %>% filter(site_id == 40139997) %>% select(dom, month, year, max_O3) %>% rename(O3_40139997 = max_O3)

#AZ_40170119 <- AZ_ALL_day %>% filter(site_id == 40170119) %>% select(dom, month, year, max_O3) %>% rename(O3_40170119 = max_O3)
#AZ_40190021 <- AZ_ALL_day %>% filter(site_id == 40190021) %>% select(dom, month, year, max_O3) %>% rename(O3_40190021 = max_O3)
#AZ_40191011 <- AZ_ALL_day %>% filter(site_id == 40191011) %>% select(dom, month, year, max_O3) %>% rename(O3_40191011 = max_O3)
#AZ_40191018 <- AZ_ALL_day %>% filter(site_id == 40191018) %>% select(dom, month, year, max_O3) %>% rename(O3_40191018 = max_O3)
#AZ_40191020 <- AZ_ALL_day %>% filter(site_id == 40191020) %>% select(dom, month, year, max_O3) %>% rename(O3_40191020 = max_O3)
#AZ_40191028 <- AZ_ALL_day %>% filter(site_id == 40191028) %>% select(dom, month, year, max_O3) %>% rename(O3_40191028 = max_O3)
#AZ_40191030 <- AZ_ALL_day %>% filter(site_id == 40191030) %>% select(dom, month, year, max_O3) %>% rename(O3_40191030 = max_O3)
#AZ_40191032 <- AZ_ALL_day %>% filter(site_id == 40191032) %>% select(dom, month, year, max_O3) %>% rename(O3_40191032 = max_O3)
#AZ_40191034 <- AZ_ALL_day %>% filter(site_id == 40191034) %>% select(dom, month, year, max_O3) %>% rename(O3_40191034 = max_O3)
#AZ_40213001 <- AZ_ALL_day %>% filter(site_id == 40213001) %>% select(dom, month, year, max_O3) %>% rename(O3_40213001 = max_O3)
#AZ_40213003 <- AZ_ALL_day %>% filter(site_id == 40213003) %>% select(dom, month, year, max_O3) %>% rename(O3_40213003 = max_O3)
#AZ_40213007 <- AZ_ALL_day %>% filter(site_id == 40213007) %>% select(dom, month, year, max_O3) %>% rename(O3_40213007 = max_O3)
#AZ_40213009 <- AZ_ALL_day %>% filter(site_id == 40213009) %>% select(dom, month, year, max_O3) %>% rename(O3_40213009 = max_O3)
#AZ_40213010 <- AZ_ALL_day %>% filter(site_id == 40213010) %>% select(dom, month, year, max_O3) %>% rename(O3_40213010 = max_O3)
#AZ_40217001 <- AZ_ALL_day %>% filter(site_id == 40217001) %>% select(dom, month, year, max_O3) %>% rename(O3_40217001 = max_O3)
#AZ_40217030 <- AZ_ALL_day %>% filter(site_id == 40217030) %>% select(dom, month, year, max_O3) %>% rename(O3_40217030 = max_O3)
#AZ_40218001 <- AZ_ALL_day %>% filter(site_id == 40218001) %>% select(dom, month, year, max_O3) %>% rename(O3_40218001 = max_O3)
#AZ_40258033 <- AZ_ALL_day %>% filter(site_id == 40258033) %>% select(dom, month, year, max_O3) %>% rename(O3_40258033 = max_O3)
#AZ_40258034 <- AZ_ALL_day %>% filter(site_id == 40258034) %>% select(dom, month, year, max_O3) %>% rename(O3_40258034 = max_O3)
#AZ_40278011 <- AZ_ALL_day %>% filter(site_id == 40278011) %>% select(dom, month, year, max_O3) %>% rename(O3_40278011 = max_O3)

#ALL_O3_max <- merge(x=AZ_40038001, y=AZ_40051008, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40058001, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40070010, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40128000, by = c("dom","month","year"), all = TRUE)

#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40130019, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40131003, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=AZ_40130019, y=AZ_40131003, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40131004, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40131010, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40132001, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40132005, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40133002, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40133003, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134003, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134004, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134005, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134008, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134010, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40134011, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40135100, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40137003, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40137020, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40137021, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40137022, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40137024, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40139508, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40139702, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40139704, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40139706, by = c("dom","month","year"), all = TRUE)
ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40139997, by = c("dom","month","year"), all = TRUE)

#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40170119, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40190021, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191011, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191018, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191020, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191028, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191030, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191032, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40191034, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40213001, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40213003, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40213007, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40213009, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40213010, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40217001, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40217030, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40218001, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40258033, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40258034, by = c("dom","month","year"), all = TRUE)
#ALL_O3_max <- merge(x=ALL_O3_max, y=AZ_40278011, by = c("dom","month","year"), all = TRUE)

ALL_O3_max <- ALL_O3_max %>% select(dom,month,year,O3_40130019, O3_40131003, O3_40131004,O3_40131010, O3_40132001, O3_40132005, O3_40133002, O3_40133003, O3_40134003, O3_40134004, O3_40134005,O3_40134008, O3_40134010, O3_40134011, O3_40135100, O3_40137003, O3_40137020, O3_40137021, O3_40137022,O3_40137024, O3_40139508, O3_40139702, O3_40139704, O3_40139706, O3_40139997)

O3_max <- ALL_O3_max %>% group_by(dom, month,year) %>% summarize(max_O3 = max(O3_40130019, O3_40131003, O3_40131004,O3_40131010, O3_40132001, O3_40132005, O3_40133002, O3_40133003, O3_40134003, O3_40134004, O3_40134005,O3_40134008, O3_40134010, O3_40134011, O3_40135100, O3_40137003, O3_40137020, O3_40137021, O3_40137022,O3_40137024, O3_40139508, O3_40139702, O3_40139704, O3_40139706, O3_40139997, na.rm = TRUE))
O3_max$max_O3 <- O3_max$max_O3*1000
write.csv(O3_max ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/O3_max.csv")
#O3_max <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/O3_max.csv", stringsAsFactors = FALSE)
#O3_max <- O3_max %>% select(dom, month, year, max_O3)

#MET DATA
MET <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/MET.csv", stringsAsFactors = FALSE, header=FALSE)
colnames(MET) <- c("SP", "H", "T", "DPT", "WDX", "WDY", "WS", "launch_time", "dom", "month", "year", "MP", "MH", "IT", "DP", "DH", "DT", "WDX925", "WDY925", "WS925", "WDX850", "WDY850", "WS850", "WDX700", "WDY700", "WS700")

#ALL DATA merge
ALL_DATA <- merge(x=O3_max, y=MET, by = c("dom","month","year"), all = TRUE)
colnames(ALL_DATA) <- c("Day_of_Month", "Month", "Year", "Maximum_O3_(ppb)", "Surface Pressure_(mb)", "Surface_Height_(m)", "Surface_Temperature_(C)", "Surface_Dew_Point_Temperature_(C)", "Surface_Wind_U_(unit_component)", "Surface_Wind_V_(unit_component)", "Surface_Wind_Speed_(m/s)", "Launch_Time_(UTC)", "Mixing_Level_Pressure_(mb)", "Mixing_Level_Height_(m)", "Inversion_Temperature_(C)", "DPressure_(mb)", "DHeight_(m)", "DTemperature(C)", "mb925_Wind_U_(unit_component)", "mb925_Wind_V_(unit_component)", "mb925_Wind_Speed_(m/s)", "mb850_Wind_U_(unit_component)", "mb850_Wind_V_(unit_component)", "mb850_Wind_Speed_(m/s)", "mb700_Wind_U_(unit_component)", "mb700_Wind_V_(unit_component)", "mb700_Wind_Speed_(m/s)")
write.csv(ALL_DATA ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/ALL_DATA.csv")

##Go to MATLAB to eliminate incomplete rows and add an exceedence index

##Repackage with headers
FINAL <- read.csv("C://Users/treeo/OneDrive/Documents/R/Thesis/FINAL.csv", stringsAsFactors = FALSE, header=FALSE)
colnames(FINAL) <- c("Day_of_Month", "Month", "Year", "Maximum_O3_(ppb)", "Surface Pressure_(mb)", "Surface_Height_(m)", "Surface_Temperature_(C)", "Surface_Dew_Point_Temperature_(C)", "Surface_Wind_U_(unit_component)", "Surface_Wind_V_(unit_component)", "Surface_Wind_Speed_(m/s)", "Launch_Time_(UTC)", "Mixing_Level_Pressure_(mb)", "Mixing_Level_Height_(m)", "Inversion_Temperature_(C)", "DPressure_(mb)", "DHeight_(m)", "DTemperature(C)", "mb925_Wind_U_(unit_component)", "mb925_Wind_V_(unit_component)", "mb925_Wind_Speed_(m/s)", "mb850_Wind_U_(unit_component)", "mb850_Wind_V_(unit_component)", "mb850_Wind_Speed_(m/s)", "mb700_Wind_U_(unit_component)", "mb700_Wind_V_(unit_component)", "mb700_Wind_Speed_(m/s)", "Exceedence")
write.csv(FINAL ,file="C://Users/treeo/OneDrive/Documents/R/Thesis/THESIS_DATA.csv")

# for ease of use
colnames(FINAL) <- c("dom", "month", "year", "O3", "SP", "SH", "ST", "SDPT", "SU", "SV", "SWS", "time", "MLP", "MLH", "IT", "DP", "DH", "DT", "U925", "V925", "WS925", "U850", "V850", "WS850", "U700", "V700", "WS700", "exceedence")

FINAL_STAT <- FINAL %>% summarize(avg_O3 = mean(O3, na.rm = TRUE),
            sd_O3 = sd(O3, na.rm = TRUE),
            avg_SP= mean(SP, na.rm = TRUE),
            sd_SP= sd(SP, na.rm = TRUE),
            avg_ST= mean(ST, na.rm = TRUE),
            sd_ST= sd(ST, na.rm = TRUE),
            avg_SDPT= mean(SDPT, na.rm = TRUE),
            sd_SDPT= sd(SDPT, na.rm = TRUE),
            avg_SU= mean(SU, na.rm = TRUE),
            sd_SU= sd(SU, na.rm = TRUE),
            avg_SV= mean(SV, na.rm = TRUE),
            sd_SV= sd(SV, na.rm = TRUE),
            avg_SWS= mean(SWS, na.rm = TRUE),
            sd_SWS= sd(SWS, na.rm = TRUE),
            avg_MLP= mean(MLP, na.rm = TRUE),
            sd_MLP= sd(MLP, na.rm = TRUE),
            avg_MLH= mean(MLH, na.rm = TRUE),
            sd_MLH= sd(MLH, na.rm = TRUE),
            avg_IT= mean(IT, na.rm = TRUE),
            sd_IT= sd(IT, na.rm = TRUE),
            avg_DP= mean(DP, na.rm = TRUE),
            sd_DP= sd(DP, na.rm = TRUE),
            avg_DH= mean(DH, na.rm = TRUE),
            sd_DH= sd(DH, na.rm = TRUE),
            avg_DT= mean(DT, na.rm = TRUE),
            sd_DT= sd(DT, na.rm = TRUE))

print(FINAL_STAT)

SE_SP <- 2 * FINAL_STAT$sd_SP/sqrt(nrow(FINAL))
SE_ST <- 2 * FINAL_STAT$sd_ST/sqrt(nrow(FINAL))
SE_SDPT <- 2 * FINAL_STAT$sd_SDPT/sqrt(nrow(FINAL))
SE_SU <- 2 * FINAL_STAT$sd_SU/sqrt(nrow(FINAL))
SE_SV <- 2 * FINAL_STAT$sd_SV/sqrt(nrow(FINAL))
SE_SWS<- 2 * FINAL_STAT$sd_SWS/sqrt(nrow(FINAL))
SE_MLP <- 2 * FINAL_STAT$sd_MLP/sqrt(nrow(FINAL))
SE_MLH <- 2 * FINAL_STAT$sd_MLH/sqrt(nrow(FINAL))
SE_IT <- 2 * FINAL_STAT$sd_IT/sqrt(nrow(FINAL))
SE_DP <- 2 * FINAL_STAT$sd_DP/sqrt(nrow(FINAL))
SE_DH <- 2 * FINAL_STAT$sd_DH/sqrt(nrow(FINAL))
SE_DT <- 2 * FINAL_STAT$sd_DT/sqrt(nrow(FINAL))

set.seed(1234)
skewness(FINAL, na.rm = TRUE)
kurtosis(FINAL, na.rm = TRUE)

CORR_FINAL <- FINAL %>% select(O3, SP, ST, SDPT, SU, SV, SWS, MLP, MLH, IT)
M_CORR_FINAL=cor(CORR_FINAL, use = "pairwise.complete.obs", method = 'pearson')
M_CORR_FINAL=cor(CORR_FINAL, use = "pairwise.complete.obs", method = 'spearman')
P_CORR_FINAL <- corrplot(M_CORR_FINAL, is.corr = TRUE, addCoef.col ='black', method = 'shade', type = 'lower')

cor.test(FINAL$O3, FINAL$SP, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$ST, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$SDPT, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$SU, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$SV, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$SWS, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$MLP, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$MLH, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$IT, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$DP, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$DH, use = "pairwise.complete.obs", method = 'spearman')
cor.test(FINAL$O3, FINAL$DT, use = "pairwise.complete.obs", method = 'spearman')

cor.test(FINAL$O3, FINAL$SP, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$ST, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$SDPT, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$SU, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$SV, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$SWS, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$MLP, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$MLH, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$IT, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$DP, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$DH, use = "pairwise.complete.obs", method = 'pearson')
cor.test(FINAL$O3, FINAL$DT, use = "pairwise.complete.obs", method = 'pearson')


FINAL_1 <- FINAL %>% filter(exceedence == 1)
FINAL_0 <- FINAL %>% filter(exceedence == 0)

#ggplot() + 
#	geom_boxplot(aes(x = FINAL_1$exceedence, y = FINAL_1$ST)) +
#	geom_boxplot(aes(x = FINAL_0$exceedence, y = FINAL_0$ST)) + 
#	#geom_violin(aes(x = FINAL_1$exceedence, y = FINAL_1$ST)) +
#	#geom_violin(aes(x = FINAL_0$exceedence, y = FINAL_0$ST)) +
#	stat_boxplot(aes(x = FINAL_1$exceedence, y = FINAL_1$ST),geom = "errorbar") + 
#	stat_boxplot(aes(x = FINAL_0$exceedence, y = FINAL_0$ST),geom = "errorbar")

T_test_SP <- t.test(SP ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_ST <- t.test(ST ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_SDPT <- t.test(SDPT ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_SU <- t.test(SU ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_SV <- t.test(SV ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_SWS <- t.test(SWS ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_MLP <- t.test(MLP ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_MLH <- t.test(MLH ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_IT <- t.test(IT ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_DP <- t.test(DP ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_DH <- t.test(DH ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")
T_test_DT <- t.test(DT ~ exceedence, data = FINAL, var.equal = FALSE, alternative = "two.sided")

Mean_SP <- T_test_SP$estimate
SE_high_SP1 = Mean_SP[1] + SE_SP
SE_low_SP1 = Mean_SP[1] - SE_SP
SE_high_SP2 = Mean_SP[2] + SE_SP
SE_low_SP2 = Mean_SP[2] - SE_SP
Plot_SP <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_SP[1], Mean_SP[2]),
                      SE_high = c(SE_high_SP1, SE_high_SP2),
                      SE_low = c(SE_low_SP1, SE_low_SP2))

Mean_ST <- T_test_ST$estimate
SE_high_ST1 = Mean_ST[1] + SE_ST
SE_low_ST1 = Mean_ST[1] - SE_ST
SE_high_ST2 = Mean_ST[2] + SE_ST
SE_low_ST2 = Mean_ST[2] - SE_ST
Plot_ST <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_ST[1], Mean_ST[2]),
                      SE_high = c(SE_high_ST1, SE_high_ST2),
                      SE_low = c(SE_low_ST1, SE_low_ST2))

Mean_SDPT <- T_test_SDPT$estimate
SE_high_SDPT1 = Mean_SDPT[1] + SE_SDPT
SE_low_SDPT1 = Mean_SDPT[1] - SE_SDPT
SE_high_SDPT2 = Mean_SDPT[2] + SE_SDPT
SE_low_SDPT2 = Mean_SDPT[2] - SE_SDPT
Plot_SDPT <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_SDPT[1], Mean_SDPT[2]),
                      SE_high = c(SE_high_SDPT1, SE_high_SDPT2),
                      SE_low = c(SE_low_SDPT1, SE_low_SDPT2))

Mean_SU <- T_test_SU$estimate
SE_high_SU1 = Mean_SU[1] + SE_SU
SE_low_SU1 = Mean_SU[1] - SE_SU
SE_high_SU2 = Mean_SU[2] + SE_SU
SE_low_SU2 = Mean_SU[2] - SE_SU
Plot_SU <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_SU[1], Mean_SU[2]),
                      SE_high = c(SE_high_SU1, SE_high_SU2),
                      SE_low = c(SE_low_SU1, SE_low_SU2))

Mean_SV <- T_test_SV$estimate
SE_high_SV1 = Mean_SV[1] + SE_SV
SE_low_SV1 = Mean_SV[1] - SE_SV
SE_high_SV2 = Mean_SV[2] + SE_SV
SE_low_SV2 = Mean_SV[2] - SE_SV
Plot_SV <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_SV[1], Mean_SV[2]),
                      SE_high = c(SE_high_SV1, SE_high_SV2),
                      SE_low = c(SE_low_SV1, SE_low_SV2))

Mean_SWS <- T_test_SWS$estimate
SE_high_SWS1 = Mean_SWS[1] + SE_SWS
SE_low_SWS1 = Mean_SWS[1] - SE_SWS
SE_high_SWS2 = Mean_SWS[2] + SE_SWS
SE_low_SWS2 = Mean_SWS[2] - SE_SWS
Plot_SWS <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_SWS[1], Mean_SWS[2]),
                      SE_high = c(SE_high_SWS1, SE_high_SWS2),
                      SE_low = c(SE_low_SWS1, SE_low_SWS2))

Mean_MLP <- T_test_MLP$estimate
SE_high_MLP1 = Mean_MLP[1] + SE_MLP
SE_low_MLP1 = Mean_MLP[1] - SE_MLP
SE_high_MLP2 = Mean_MLP[2] + SE_MLP
SE_low_MLP2 = Mean_MLP[2] - SE_MLP
Plot_MLP <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_MLP[1], Mean_MLP[2]),
                      SE_high = c(SE_high_MLP1, SE_high_MLP2),
                      SE_low = c(SE_low_MLP1, SE_low_MLP2))

Mean_MLH <- T_test_MLH$estimate
SE_high_MLH1 = Mean_MLH[1] + SE_MLH
SE_low_MLH1 = Mean_MLH[1] - SE_MLH
SE_high_MLH2 = Mean_MLH[2] + SE_MLH
SE_low_MLH2 = Mean_MLH[2] - SE_MLH
Plot_MLH <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_MLH[1], Mean_MLH[2]),
                      SE_high = c(SE_high_MLH1, SE_high_MLH2),
                      SE_low = c(SE_low_MLH1, SE_low_MLH2))

Mean_IT <- T_test_IT$estimate
SE_high_IT1 = Mean_IT[1] + SE_IT
SE_low_IT1 = Mean_IT[1] - SE_IT
SE_high_IT2 = Mean_IT[2] + SE_IT
SE_low_IT2 = Mean_IT[2] - SE_IT
Plot_IT <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_IT[1], Mean_IT[2]),
                      SE_high = c(SE_high_IT1, SE_high_IT2),
                      SE_low = c(SE_low_IT1, SE_low_IT2))

Mean_DP <- T_test_DP$estimate
SE_high_DP1 = Mean_DP[1] + SE_DP
SE_low_DP1 = Mean_DP[1] - SE_DP
SE_high_DP2 = Mean_DP[2] + SE_DP
SE_low_DP2 = Mean_DP[2] - SE_DP
Plot_DP <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_DP[1], Mean_DP[2]),
                      SE_high = c(SE_high_DP1, SE_high_DP2),
                      SE_low = c(SE_low_DP1, SE_low_DP2))

Mean_DH <- T_test_DH$estimate
SE_high_DH1 = Mean_DH[1] + SE_DH
SE_low_DH1 = Mean_DH[1] - SE_DH
SE_high_DH2 = Mean_DH[2] + SE_DH
SE_low_DH2 = Mean_DH[2] - SE_DH
Plot_DH <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_DH[1], Mean_DH[2]),
                      SE_high = c(SE_high_DH1, SE_high_DH2),
                      SE_low = c(SE_low_DH1, SE_low_DH2))

Mean_DT <- T_test_DT$estimate
SE_high_DT1 = Mean_DT[1] + SE_DT
SE_low_DT1 = Mean_DT[1] - SE_DT
SE_high_DT2 = Mean_DT[2] + SE_DT
SE_low_DT2 = Mean_DT[2] - SE_DT
Plot_DT <- data.frame(X = c("Non-Exceedance","Exceedance"),
                      Avg = c(Mean_DT[1], Mean_DT[2]),
                      SE_high = c(SE_high_DT1, SE_high_DT2),
                      SE_low = c(SE_low_DT1, SE_low_DT2))

Color=c("darkred", "darkblue")
PSP <- ggplot(Plot_SP, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Millibars", x = "Surface Level Pressure") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PST <- ggplot(Plot_ST, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Degrees Celsius", x = "Surface Level Temperature") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PSDPT <- ggplot(Plot_SDPT, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Degrees Celsius", x = "Surface Level Dew Point Temperature") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PSU <- ggplot(Plot_SU, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Unit Component U", x = "Surface Level Wind Direction U") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PSV <- ggplot(Plot_SV, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Unit Component V", x = "Surface Level Wind Direction V") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PSWS <- ggplot(Plot_SWS, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Meters per Second", x = "Surface Level Wind Speed") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PMLP <- ggplot(Plot_MLP, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Millibars", x = "Mixing Level Pressure") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PMLH <- ggplot(Plot_MLH, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Meters", x = "Mixing Level Height") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PIT <- ggplot(Plot_IT, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Degrees Celsius", x = "Inversion Temperature") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PDP <- ggplot(Plot_DP, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Millibars", x = "Change in Pressure") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PDH <- ggplot(Plot_DH, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Meters", x = "Change in Height") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)

PDT <- ggplot(Plot_DT, aes(x= X, y=Avg)) + geom_point(size = 2) + labs(y = "Degrees Celsius", x = "Change in Temperature") +
  geom_errorbar(aes(ymin = SE_low, ymax = SE_high), size = .1, colour = Color)


grid.arrange(PSP,PST,PSDPT,PSU,PSV,PSWS,PMLP,PMLH,PIT,PDP,PDH,PDT,ncol=6)



moment(FINAL, order = 4, central = TRUE, absolute = FALSE, na.rm = TRUE)
sample.moments(FINAL$SP, na.rm = F, raw = T, central = T, coef = T, order = 1:4)
library(CoSMoS)


