library(raster)
library(lubridate)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)

####Preparing Daily Data####

#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents")

##read model data
#here meteorological data is from cfsv2
#pollutant data is from airnow.gov
Beijing_2011_18 = read.csv("Beijing_2011_18.csv")

#Adjusting Date
Beijing_2011_18$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")

#calculate wind speed
Beijing_2011_18$wind <- sqrt (Beijing_2011_18$u10^2 + Beijing_2011_18$v10^2)


#calculate wind direction
Beijing_2011_18$wind_dir_rad = atan2(Beijing_2011_18$u10/Beijing_2011_18$wind, 
                                     Beijing_2011_18$v10/Beijing_2011_18$wind)

Beijing_2011_18$wind_dir <- Beijing_2011_18$wind_dir_rad * 180/pi
Beijing_2011_18$wind_dir_met <- Beijing_2011_18$wind_dir + 180
Beijing_2011_18$wind_dir_cardinal <- 90 - Beijing_2011_18$wind_dir_met

#changing wind direction to different factors
#angle measured in degrees from true north and direction from which wind is blowing
Beijing_2011_18$wind_dir_factor <- round(Beijing_2011_18$wind_dir_cardinal/45, digits = 0)

#subset
Beijing_model_data <- subset(Beijing_2011_18, 
                             select = -c(X, date_new, TCO, u10, v10, wind_dir_factor, 
                                         wind_dir_rad, wind_dir_met, wind_dir))

#rename specific columns
Beijing_model_data <- Beijing_model_data %>% rename(wind_dir_model = wind_dir_cardinal)
Beijing_model_data <- Beijing_model_data %>% rename(wind_speed_model = wind)
Beijing_model_data <- Beijing_model_data %>% rename(precip_model = PRCP)
Beijing_model_data <- Beijing_model_data %>% rename(temp_avg_model = TAVG)
Beijing_model_data <- Beijing_model_data %>% rename(PBL_model = PBL)
Beijing_model_data <- Beijing_model_data %>% rename(RH_model = RH)

#save data
fwrite(Beijing_model_data, file = "Beijing_model_data.csv")



###Preparing Hourly Observed Data

#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_obs/table_data")

#list files
data.files <- list.files("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_obs/table_data")

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

##Variables with highest numbers of missing values
#prec1 has all NA's: 100% missing values
#prec6: 98.29% missing values
#pressure: 66.92% missing values
#Sky cover: 43.95%

#Hence removing these columns
data_reduced <- subset(data, select = -c(prec1, prec6, sky, pressure))

#remove scaling factors
data_reduced$temp <- data_reduced$temp/10
data_reduced$dewtemp <- data_reduced$dewtemp/10
data_reduced$windspeed <- data_reduced$windspeed/10

#save data
fwrite(data_reduced, "met_hourly.csv")

#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents")

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

#subset relevant columns
met_final <- subset(met_daily_2, select = -c(year,month,day))

#save data
fwrite(met_final, file = "daily_met.csv")

##Merge everything##
Bej_mod <- read.csv("Beijing_model_data.csv")
Bej_obs <- read.csv("daily_met.csv")

#rename to match column names
Bej_obs <- Bej_obs %>% rename(date = Date)

#merge data
Bej_tog <- merge(Bej_mod, Bej_obs, by = "date")

#save data
fwrite(Bej_tog, file = "Beijing_final_data.csv")

#open saved data
Bej_good <- read.csv("Beijing_final_data.csv")

#temperature and wind speed are recorded by both observation and model
#other met variables are non-overlapping

#check their relations/agreements by simple plots
plot(x = Bej_good$wind_speed_model, y = Bej_good$windspeed, type = 'l')
plot(x = Bej_good$temp_avg_model, y = Bej_good$temp, type = 'l')
plot(x = Bej_good$wind_dir_model, y = Bej_good$winddir, type = 'l')

#model does not agree with observation in case of wind speed, 
#but agrees in case of temperature (probably)
#statistical evaluation should be done!

###Preparing Hourly Observed Data for PM2.5

#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_PM2.5")

#list files
pm.files <- list.files("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Beijing_PM2.5")

#Loop over file list importing them and binding them together
pm.data <- lapply(pm.files, fread) %>% 
  bind_rows()

##Manipulate the erroneous values##
pm.data$`Raw Conc.`[pm.data$`Raw Conc.` < 0] <- NA

#remove unnecessary columns
pm.data_reduced <- subset(pm.data, select = c(Year, Month, Day, Hour, `Raw Conc.`))

#check summary
summary(pm.data_reduced)

#Calculate daily summaries
daily.summary_pm <- aggregate(pm.data_reduced$`Raw Conc.`, list(pm.data_reduced$Year, 
                                                                pm.data_reduced$Month, 
                                                                pm.data_reduced$Day), 
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
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Bej_Data_Prep/Bej_ozone")

#list files
ozone.files <- list.files("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents/Bej_Data_Prep/Bej_ozone")

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
Bej_good <- read.csv("Beijing_final_data.csv")
pol_daily <- read.csv("daily_pm_ozone.csv")

#subset to remove old PM2.5 and ozone data
Bej_good_reduced <- subset(Bej_good, select = -c(PM2.5_avg_conc, Ozone)) 

#rename to match columns
Bej_good_reduced <- Bej_good_reduced %>% rename(Date = date)

#merge
Bej_daily <- merge(Bej_good_reduced, pol_daily, by = "Date")

#save data
fwrite(Bej_daily, file = "daily_bej_det.csv")

#read haotian data
hao_daily <- read.csv("Hao_pm.csv")

#subset for nearest station
hao_daily_comp <- subset(hao_daily, hao_daily$sta.ID == "1003A")

#remove unnecessary columns
hao_data <- subset(hao_daily_comp, select = -c(sta.ID, variable))

#adjusting date
hao_data$date <- as.Date(hao_data$date)
hao_data <- hao_data %>% rename(Date = date)

#rename column
hao_data <- hao_data %>% rename(hao_pm = day_mean)

#save data
fwrite(hao_data, file = "hao_beijing.csv")

#merge with airnow data
obs_daily <- merge(pol_daily, hao_data, by = "Date", na.rm = "TRUE")

#plot to compare
theme_set(theme_bw())
ggplot(obs_daily, aes(x = Date, y = PM2.5)) + 
  geom_line(aes(x = Date, y=pm),colour="red") +  
  geom_line(aes(x = Date, y=hao_pm), colour="springgreen4") +
  scale_color_manual(values= c("AirNow" = "red",
                               "Haotian" = "springgreen4"))

#compare
summary(obs_daily$pm)
summary(obs_daily$hao_pm)
hist(obs_daily$pm)
hist(obs_daily$hao_pm)


