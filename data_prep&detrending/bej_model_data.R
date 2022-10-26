rm( list = ls())

library(data.table)
library(tidyverse)
library(lubridate)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)

###Preparing Daily Observed Data for PM2.5

#setting specific directory
setwd("D:/Professional/Projects/Beijing/Bej_model")

#list files
data.files <- list.files("D:/Professional/Projects/Beijing/Bej_model")

#Loop over file list importing them and binding them together
data <- lapply(data.files, fread) %>% 
  bind_cols()

#convert to data frame
data_df <- data.frame(data)

#subset to remove repeated dates
data_sub <- data_df[c(1, 2, 3, 6, 9, 12, 15, 18, 21, 24)]

#renaming columns
data_sub <- data_sub %>% rename(Date = Date...1)
data_sub <- data_sub %>% rename(Time = Time...2)
data_sub <- data_sub %>% rename(Prec = Total.precipitation.Ground.or.water.surface)
data_sub <- data_sub %>% rename(Pbl = Planetary.boundary.layer.height.Ground.or.water.surface)
data_sub <- data_sub %>% rename(RH = Relative.humidity.Isobaric.surface..1000.mbar)
data_sub <- data_sub %>% rename(Pres = Pressure.Mean.sea.level)
data_sub <- data_sub %>% rename(Albedo = Albedo.Ground.or.water.surface)
data_sub <- data_sub %>% rename(SM = Soil.moisture.content.Layer.between.two..Depth.below.land.surface...Bottom.2.m..Top.0.m)
data_sub <- data_sub %>% rename(SR = Downward.shortwave.radiation.flux.Ground.or.water.surface)
data_sub <- data_sub %>% rename(Cloud = Total.cloud.cover.Convective.cloud.layer)

##creating daily values from hourly values
#modifying date
data_sub$Date <- as.Date(data_sub$Date,"%m/%d/%Y")
data_sub$day <- day(data_sub$Date)
data_sub$month <- month(data_sub$Date)
data_sub$year <- year(data_sub$Date)

#doing aggregate

#Calculate daily summaries
daily.summary_bej <- aggregate(cbind(data_sub$Prec, data_sub$Pbl, data_sub$RH,
                                     data_sub$Pres, data_sub$Albedo, data_sub$SR,
                                     data_sub$SM, data_sub$Cloud),
                               list(data_sub$day,
                                    data_sub$month,
                                    data_sub$year),
                                 mean, na.rm = TRUE)


#rename
daily.summary_bej <- daily.summary_bej %>% rename(day = Group.1)
daily.summary_bej <- daily.summary_bej %>% rename(month = Group.2)
daily.summary_bej <- daily.summary_bej %>% rename(year = Group.3)
daily.summary_bej <- daily.summary_bej %>% rename(Prec = V1)
daily.summary_bej <- daily.summary_bej %>% rename(Pbl = V2)
daily.summary_bej <- daily.summary_bej %>% rename(RH = V3)
daily.summary_bej <- daily.summary_bej %>% rename(Pres = V4)
daily.summary_bej <- daily.summary_bej %>% rename(Albedo = V5)
daily.summary_bej <- daily.summary_bej %>% rename(SR = V6)
daily.summary_bej <- daily.summary_bej %>% rename(SM = V7)
daily.summary_bej <- daily.summary_bej %>% rename(Cloud = V8)

#adjust date
daily.summary_bej$Date<-as.Date(with(daily.summary_bej,paste(year,month,day,sep="-")),"%Y-%m-%d")

#subset relevant columns
bejmod_final <- subset(daily.summary_bej, select = -c(year,month,day))

#save data
fwrite(bejmod_final, file = "daily_model_data.csv")
bejmod_final <- fread(file = "daily_model_data.csv")


#merge with previous data
bej_final <- merge(Bej_tog, bejmod_final)
bej_final <- as.data.table(bej_final)


#save data
fwrite(bej_final, file = "final_data.csv")


