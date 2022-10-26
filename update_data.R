library(raster)
library(lubridate)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)

###Preparing Hourly Observed Data for PM2.5

#setting specific directory
setwd("D:/Professional/Projects/Beijing/Bej_PM2.5")

#list files
pm.files <- list.files("D:/Professional/Projects/Beijing/Bej_PM2.5")

#Loop over file list importing them and binding them together
pm.data <- lapply(pm.files, fread) %>% 
  bind_rows()

##Manipulate the erroneous values##
pm.data$`Raw Conc.`[pm.data$`Raw Conc.` < 0] <- NA

#remove unnecessary columns
pm.data_reduced <- subset(pm.data, select = c(Year, Month, Day, Hour, `Raw Conc.`))

#check summary
summary(pm.data_reduced)

#remove double dates
pm.data_reduced.2 <- pm.data_reduced[ which(Year < 2021),]

#Calculate daily summaries
daily.summary_pm <- aggregate(pm.data_reduced.2$`Raw Conc.`, list(pm.data_reduced.2$Year, 
                                                                pm.data_reduced.2$Month, 
                                                                pm.data_reduced.2$Day), 
                              mean, na.rm = TRUE)

#rename
daily.summary_pm <- daily.summary_pm %>% rename(year = Group.1)
daily.summary_pm <- daily.summary_pm %>% rename(month = Group.2)
daily.summary_pm <- daily.summary_pm %>% rename(day = Group.3)
daily.summary_pm <- daily.summary_pm %>% rename(pm = x)

#adjust date
daily.summary_pm$Date<-as.Date(with(daily.summary_pm,paste(year,month,day,sep="-")),"%Y-%m-%d")

#subset relevant columns
pm_final <- subset(daily.summary_pm, select = -c(year,month,day))

#save data
fwrite(pm_final, file = "daily_pm.csv")

###Preparing Hourly Observed Data for Ozone

#setting specific directory
setwd("D:/Professional/Projects/Beijing/Bej_ozone")

#list files
ozone.files <- list.files("D:/Professional/Projects/Beijing/Bej_ozone")

#Loop over file list importing them and binding them together
ozone.data <- lapply(ozone.files, fread) %>% 
  bind_rows()

##Manipulate the erroneous values##
ozone.data$`Raw Conc.`[ozone.data$`Raw Conc.` < 0] <- NA

#remove unnecessary columns
ozone.data_reduced <- subset(ozone.data, select = c(Year, Month, Day, Hour, `Raw Conc.`))

#check summary
summary(ozone.data_reduced)

#Calculate daily summaries
daily.summary_ozone <- aggregate(ozone.data_reduced$`Raw Conc.`, list(ozone.data_reduced$Year, 
                                                                      ozone.data_reduced$Month, 
                                                                      ozone.data_reduced$Day), 
                                 mean, na.rm = TRUE)

#rename
daily.summary_ozone <- daily.summary_ozone %>% rename(year = Group.1)
daily.summary_ozone <- daily.summary_ozone %>% rename(month = Group.2)
daily.summary_ozone <- daily.summary_ozone %>% rename(day = Group.3)
daily.summary_ozone <- daily.summary_ozone %>% rename(ozone = x)


#adjust date
daily.summary_ozone$Date<-as.Date(with(daily.summary_ozone,paste(year,month,day,sep="-")),"%Y-%m-%d")

#subset relevant columns
oz_final <- subset(daily.summary_ozone, select = -c(year,month,day))

#save data
fwrite(oz_final, file = "daily_ozone.csv")

##Merge the pollutant data
#merge data
pol_daily <- merge(pm_final, oz_final, by = "Date")

#save data
fwrite(pol_daily, file = "daily_pm_ozone.csv")


#open saved data
pol_daily <- read.csv("daily_pm_ozone.csv")



###Preparing Hourly Observed Data

#setting specific directory
setwd("D:/Professional/Projects/Beijing/Beijing_obs/table_data")

#list files
data.files <- list.files("D:/Professional/Projects/Beijing/Beijing_obs/table_data")

#Loop over file list importing them and binding them together
data <- lapply(data.files, fread) %>% 
  bind_rows()


#renaming columns
data <- data %>% rename(year = V1)
data <- data %>% rename(month = V2)
data <- data %>% rename(day = V3)
data <- data %>% rename(hour = V4)
data <- data %>% rename(temp = V5)
data <- data %>% rename(dewtemp = V6)
data <- data %>% rename(pressure = V7)
data <- data %>% rename(winddir = V8)
data <- data %>% rename(windspeed = V9)
data <- data %>% rename(sky = V10)
data <- data %>% rename(prec1 = V11)
data <- data %>% rename(prec6 = V12)


##Manipulate the erroneous values##
data$temp[data$temp == -9999] <- NA
data$dewtemp[data$dewtemp == -9999] <- NA
data$pressure[data$pressure == -9999] <- NA
data$windspeed[data$windspeed == -9999] <- NA
data$winddir[data$winddir == -9999] <- NA
data$sky[data$sky == -9999] <- NA
data$prec1[data$prec1 == -9999] <- NA
data$prec6[data$prec6 == -9999] <- NA

#check summary
summary(data)

#remove data with too much NA's
data_reduced <- subset(data, select = -c(prec1, prec6, sky, pressure))

#remove scaling factors
data_reduced$temp <- data_reduced$temp/10
data_reduced$dewtemp <- data_reduced$dewtemp/10
data_reduced$windspeed <- data_reduced$windspeed/10

#save data
fwrite(data_reduced, "met_hourly.csv")

#open saved data
data_reduced <- read.csv("met_hourly.csv")

#Calculate daily summaries
daily.summary_temp <- aggregate(data_reduced[5], list(data_reduced$year, data_reduced$month,
                                                      data_reduced$day), mean, na.rm = TRUE)
daily.summary_dewtemp <- aggregate(data_reduced[6], list(data_reduced$year, data_reduced$month,
                                                         data_reduced$day), mean, na.rm = TRUE)
daily.summary_winddir <- aggregate(data_reduced[7], list(data_reduced$year, data_reduced$month,
                                                         data_reduced$day), mean, na.rm = TRUE)
daily.summary_windspeed <- aggregate(data_reduced[8], list(data_reduced$year, data_reduced$month,
                                                           data_reduced$day), mean, na.rm = TRUE)

#join the datasets
met_daily <- merge(daily.summary_temp, daily.summary_dewtemp, all = TRUE)
met_daily_1 <- merge(met_daily, daily.summary_winddir, all = TRUE)
met_daily_2 <- merge(met_daily_1, daily.summary_windspeed, all = TRUE)

#rename
met_daily_2 <- met_daily_2 %>% rename(year = Group.1)
met_daily_2 <- met_daily_2 %>% rename(month = Group.2)
met_daily_2 <- met_daily_2 %>% rename(day = Group.3)

#adjust date
met_daily_2$Date<-as.Date(with(met_daily_2,paste(year,month,day,sep="-")),"%Y-%m-%d")
pol_daily$Date <- as.Date(pol_daily$Date)

#subset relevant columns
met_final <- subset(met_daily_2, select = -c(year,month,day))

#save data
fwrite(met_final, file = "daily_met.csv")

#merge everything together
Bej_tog <- merge(pol_daily, met_final, by = "Date")

#save data
setwd("D:/Professional/Projects/Beijing")
fwrite(Bej_tog, file = "Beijing_final_data.csv")
Bej_tog <- fread(file = "Beijing_final_data.csv")


