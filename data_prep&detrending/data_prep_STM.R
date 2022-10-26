rm( list = ls())
library(lubridate)
library(sp)
library(raster)
library(sf)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)


#setting specific directory
setwd("D:/Professional/Projects/Beijing")
#read data
Bej_daily <- read.csv("final_data.csv")
object.size(Bej_daily) #975kb

#assigning numbers 1 to 7 to days
Bej_daily$Date <- as.Date(Bej_daily$Date)
Bej_daily$days <- wday(Bej_daily$Date)
#1 and 7 are weekends

#subseting to remove columns which we do not intend to scale to store scaling variables
Beijing_numeric <- subset(Bej_daily, select = -c(Date, pm, ozone, winddir, Prec, days)) 

##storing minimum and maximum values for un-scaling in future
meanvec <- sapply(Beijing_numeric, mean, na.rm = TRUE)
sdvec <- sapply(Beijing_numeric, sd, na.rm = TRUE)

##scaling data##
#define scaling function8
scaledf <- function(x, na.rm = TRUE) {
  return ((x - mean(x, na.rm = TRUE)) / (sd(x, na.rm = TRUE)))
}

#not intending to scale PM2.5 or ozone, hence excluded
#only scaling columns having meteorological variables except wind direction
Bej_daily_scaled <- lapply(Bej_daily[-c(1, 2, 3, 6, 8, 16)], scaledf)
Bej_daily_df <- as.data.frame(Bej_daily_scaled)

#take the unscaled columns
Bej_daily_unscaled <- subset(Bej_daily, select = c(1, 2, 3, 6, 8, 16))

#join the scaled and unscaled columns
Bej_data <- cbind(Bej_daily_unscaled, Bej_daily_df, all = TRUE)

#manipulate the date in date format
Bej_data$Date <- as.Date(Bej_data$Date)

#changing precipitation to categorical variable
Bej_data$PRCPBin [Bej_data$Prec != 0.0000] <- 1
Bej_data$PRCPBin [Bej_data$Prec == 0.0000] <- 0

#check number of rainy days
summary(Bej_data$PRCPBin)

#wind direction as factors
Bej_data$Wdf <- 1 + abs(round(Bej_data$winddir%/%90, digits = 0))

#as factors
Bej_data$PRCPBool <- as.factor(Bej_data$PRCPBin)
Bej_data$Wdfac <- as.factor(Bej_data$Wdf)
Bej_data$dayf <- as.factor(Bej_data$days)

object.size(Bej_data)
#604kb

###Introducing Detrending by KZ filter###
###Using KZ filter###
library("kza")

###Long-term###
Bej_data$kz_PM_annual <- kz(Bej_data$pm, m = 365, k = 3)
Bej_data$kz_ozone_annual <- kz(Bej_data$ozone, m = 365, k = 3)
Bej_data$kz_Temp_annual <- kz(Bej_data$temp, m = 365, k = 3)
Bej_data$kz_Td_annual <- kz(Bej_data$dewtemp, m = 365, k = 3)
Bej_data$kz_Ws_annual <- kz(Bej_data$windspeed, m = 365, k = 3)
Bej_data$kz_RH_annual <- kz(Bej_data$RH, m = 365, k = 3)
Bej_data$kz_PBL_annual <- kz(Bej_data$Pbl, m = 365, k = 3)
Bej_data$kz_Pres_annual <- kz(Bej_data$Pres, m = 365, k = 3)
Bej_data$kz_Albedo_annual <- kz(Bej_data$Albedo, m = 365, k = 3)
Bej_data$kz_SR_annual <- kz(Bej_data$SR, m = 365, k = 3)
Bej_data$kz_SM_annual <- kz(Bej_data$SM, m = 365, k = 3)

##longterm, remaining##
Bej_data$PM_LT_rem <- Bej_data$pm - Bej_data$kz_PM_annual
Bej_data$ozone_LT_rem <- Bej_data$ozone - Bej_data$kz_ozone_annual
Bej_data$Temp_LT_rem <- Bej_data$temp - Bej_data$kz_Temp_annual
Bej_data$Td_LT_rem <- Bej_data$dewtemp - Bej_data$kz_Td_annual
Bej_data$Ws_LT_rem <- Bej_data$windspeed - Bej_data$kz_Ws_annual
Bej_data$kz_RH_rem <- Bej_data$RH - Bej_data$kz_RH_annual
Bej_data$kz_PBL_rem <- Bej_data$Pbl - Bej_data$kz_PBL_annual
Bej_data$kz_Pres_rem <- Bej_data$Pres - Bej_data$kz_Pres_annual
Bej_data$kz_Albedo_rem <- Bej_data$Albedo - Bej_data$kz_Albedo_annual
Bej_data$kz_SR_rem <- Bej_data$SR - Bej_data$kz_SR_annual
Bej_data$kz_SM_rem <- Bej_data$SM - Bej_data$kz_SM_annual


##Seasonal##
Bej_data$kz_PM_seasonal <- kz(Bej_data$PM_LT_rem, m = 15, k = 5)
Bej_data$kz_ozone_seasonal <- kz(Bej_data$ozone_LT_rem, m = 15, k = 5)
Bej_data$kz_Temp_seasonal <- kz(Bej_data$Temp_LT_rem, m = 15, k = 5)
Bej_data$kz_Td_seasonal <- kz(Bej_data$Td_LT_rem, m = 15, k = 5)
Bej_data$kz_Ws_seasonal <- kz(Bej_data$Ws_LT_rem, m = 15, k = 5)
Bej_data$kz_RH_seasonal <- kz(Bej_data$kz_RH_rem, m = 15, k = 5)
Bej_data$kz_PBL_seasonal <- kz(Bej_data$kz_PBL_rem, m = 15, k = 5)
Bej_data$kz_Pres_seasonal <- kz(Bej_data$kz_Pres_rem, m = 15, k = 5)
Bej_data$kz_Albedo_seasonal <- kz(Bej_data$kz_Albedo_rem, m = 15, k = 5)
Bej_data$kz_SR_seasonal <- kz(Bej_data$kz_SR_rem, m = 15, k = 5)
Bej_data$kz_SM_seasonal <- kz(Bej_data$kz_SM_rem, m = 15, k = 5)


###Short-term###
Bej_data$PM_STM <- Bej_data$PM_LT_rem - Bej_data$kz_PM_seasonal
Bej_data$ozone_STM <- Bej_data$ozone_LT_rem - Bej_data$kz_ozone_seasonal
Bej_data$Temp_STM <- Bej_data$Temp_LT_rem - Bej_data$kz_Temp_seasonal
Bej_data$Td_STM <- Bej_data$Td_LT_rem - Bej_data$kz_Td_seasonal
Bej_data$Ws_STM <- Bej_data$Ws_LT_rem - Bej_data$kz_Ws_seasonal
Bej_data$RH_STM <- Bej_data$kz_RH_rem - Bej_data$kz_RH_seasonal 
Bej_data$PBL_STM <- Bej_data$kz_PBL_rem - Bej_data$kz_PBL_seasonal
Bej_data$Pres_STM <- Bej_data$kz_Pres_rem - Bej_data$kz_Pres_seasonal 
Bej_data$Albedo_STM <- Bej_data$kz_Albedo_rem - Bej_data$kz_Albedo_seasonal 
Bej_data$SR_STM <- Bej_data$kz_SR_rem - Bej_data$kz_SR_seasonal 
Bej_data$SM_STM <- Bej_data$kz_SM_rem - Bej_data$kz_SM_seasonal 

#save data
fwrite(Bej_data, file = "Bej_data.stm.csv")

#subset to remove irrelevant columns
Bej_data_stm <- subset(Bej_data, select = c(Date, PM_STM, ozone_STM, 
                                            Temp_STM, Td_STM, Ws_STM,
                                            Wdfac, PRCPBool, RH_STM, PBL_STM, 
                                            Pres_STM, Albedo_STM, SR_STM, SM_STM,
                                            Cloud, dayf))

#save this data too
library(fst)
write.fst( Bej_data_stm, "Bej.fst")



