library(lubridate)
library(sp)
library(raster)
library(sf)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)


#setting specific directory
setwd("C:/Users/15712/Dropbox/My PC (LAPTOP-40KM1A54)/Documents")
#read data
Bej_daily <- read.csv("daily_bej_det.csv")
object.size(Bej_daily) #477kb

#assigning numbers 1 to 7 to days
Bej_daily$Date <- as.Date(Bej_daily$Date)
Bej_daily$days <- wday(Bej_daily$Date)
#1 and 7 are weekends

#remove columns with repeated values from model
Bej_non_repeated <- subset(Bej_daily, select = -c(temp_avg_model, 
                                                  wind_speed_model, wind_dir_model))

#check distributions of modeled met variables
hist(Bej_non_repeated$RH_model)
hist(Bej_non_repeated$PBL_model)
hist(Bej_data$RH_model)
hist(Bej_data$PBL_model)

#subseting to remove columns which we do not intend to scale to store scaling variables
Beijing_numeric <- subset(Bej_non_repeated, select = -c(Date, pm, ozone, winddir, days,
                                                        precip_model)) 


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
Bej_daily_scaled <- lapply(Bej_non_repeated[-c(1, 2, 7, 9, 10, 11)], scaledf)
Bej_daily_df <- as.data.frame(Bej_daily_scaled)


#take the unscaled columns
Bej_daily_unscaled <- subset(Bej_non_repeated, select = c(1, 2, 7, 9, 10, 11))

#join the scaled and unscaled columns
Bej_data <- cbind(Bej_daily_unscaled, Bej_daily_df, all = TRUE)

#manipulate the date in date format
Bej_data$Date <- as.Date(Bej_data$Date)

#changing precipitation to categorical variable
Bej_data$PRCPBin [Bej_data$precip_model != 0.0000] <- 1
Bej_data$PRCPBin [Bej_data$precip_model == 0.0000] <- 0

#check number of rainy days
summary(Bej_data$PRCPBin)

#wind direction as factors
Bej_data$Wdf <- 1 + abs(round(Bej_data$winddir%/%90, digits = 0))

#as factors
Bej_data$PRCPBool <- as.factor(Bej_data$PRCPBin)
Bej_data$Wdfac <- as.factor(Bej_data$Wdf)
Bej_data$dayf <- as.factor(Bej_data$days)

object.size(Bej_data)
#340.84

###Introducing Detrending by KZ filter###
###Using KZ filter###
library("kza")

###Step-1: Detrending unscaled data###

###Long-term###
Bej_daily$kz_PM_annual <- kz(Bej_daily$PM2.5_avg_conc, m = 365, k = 3)
Bej_daily$kz_ozone_annual <- kz(Bej_daily$Ozone, m = 365, k = 3)
Bej_daily$kz_Temp_annual <- kz(Bej_daily$temp, m = 365, k = 3)
Bej_daily$kz_Td_annual <- kz(Bej_daily$dewtemp, m = 365, k = 3)
Bej_daily$kz_Ws_annual <- kz(Bej_daily$windspeed, m = 365, k = 3)
Bej_daily$kz_precip_annual <- kz(Bej_daily$precip_model, m = 365, k = 3)
Bej_daily$kz_RH_annual <- kz(Bej_daily$RH_model, m = 365, k = 3)
Bej_daily$kz_PBL_annual <- kz(Bej_daily$PBL_model, m = 365, k = 3)
Bej_daily$kz_Tempmod_annual <- kz(Bej_daily$temp_avg_model, m = 365, k = 3)
Bej_daily$kz_Wsmod_annual <- kz(Bej_daily$wind_speed_model, m = 365, k = 3)


##longterm, remaining##
Bej_daily$PM_LT_rem <- Bej_daily$PM2.5_avg_conc - Bej_daily$kz_PM_annual
Bej_daily$ozone_LT_rem <- Bej_daily$Ozone - Bej_daily$kz_ozone_annual
Bej_daily$Temp_LT_rem <- Bej_daily$temp - Bej_daily$kz_Temp_annual
Bej_daily$Td_LT_rem <- Bej_daily$dewtemp - Bej_daily$kz_Td_annual
Bej_daily$Ws_LT_rem <- Bej_daily$windspeed - Bej_daily$kz_Ws_annual
Bej_daily$kz_precip_rem <- Bej_daily$precip_model - Bej_daily$kz_precip_annual
Bej_daily$kz_RH_rem <- Bej_daily$RH_model - Bej_daily$kz_RH_annual
Bej_daily$kz_PBL_rem <- Bej_daily$PBL_model - Bej_daily$kz_PBL_annual
Bej_daily$kz_Tempmod_rem <- Bej_daily$temp_avg_model - Bej_daily$kz_Tempmod_annual 
Bej_daily$kz_Wsmod_rem <- Bej_daily$wind_speed_model - Bej_daily$kz_Wsmod_annual

##Seasonal##
Bej_daily$kz_PM_seasonal <- kz(Bej_daily$PM_LT_rem, m = 15, k = 5)
Bej_daily$kz_ozone_seasonal <- kz(Bej_daily$ozone_LT_rem, m = 15, k = 5)
Bej_daily$kz_Temp_seasonal <- kz(Bej_daily$Temp_LT_rem, m = 15, k = 5)
Bej_daily$kz_Td_seasonal <- kz(Bej_daily$Td_LT_rem, m = 15, k = 5)
Bej_daily$kz_Ws_seasonal <- kz(Bej_daily$Ws_LT_rem, m = 15, k = 5)
Bej_daily$kz_precip_seasonal <- kz(Bej_daily$kz_precip_rem, m = 15, k = 5)
Bej_daily$kz_RH_seasonal <- kz(Bej_daily$kz_RH_rem, m = 15, k = 5)
Bej_daily$kz_PBL_seasonal <- kz(Bej_daily$kz_PBL_rem, m = 15, k = 5)
Bej_daily$kz_Tempmod_seasonal <- kz(Bej_daily$kz_Tempmod_rem, m = 15, k = 5)
Bej_daily$kz_Wsmod_seasonal <- kz(Bej_daily$kz_Wsmod_rem, m = 15, k = 5)


###Short-term###
Bej_daily$PM_STM <- Bej_daily$PM_LT_rem - Bej_daily$kz_PM_seasonal
Bej_daily$ozone_STM <- Bej_daily$ozone_LT_rem - Bej_daily$kz_ozone_seasonal
Bej_daily$Temp_STM <- Bej_daily$Temp_LT_rem - Bej_daily$kz_Temp_seasonal
Bej_daily$Td_STM <- Bej_daily$Td_LT_rem - Bej_daily$kz_Td_seasonal
Bej_daily$Ws_STM <- Bej_daily$Ws_LT_rem - Bej_daily$kz_Ws_seasonal
Bej_daily$precip_STM <- Bej_daily$kz_precip_rem - Bej_daily$kz_precip_seasonal
Bej_daily$RH_STM <- Bej_daily$kz_RH_rem - Bej_daily$kz_RH_seasonal 
Bej_daily$PBL_STM <- Bej_daily$kz_PBL_rem - Bej_daily$kz_PBL_seasonal 
Bej_daily$tempmod_STM <- Bej_daily$kz_Tempmod_rem - Bej_daily$kz_Tempmod_seasonal
Bej_daily$wsmod_STM <- Bej_daily$kz_Wsmod_rem - Bej_daily$kz_Wsmod_seasonal


##Step-2: Detrending scaled variables with scaled values, 
#although we're unsure of certain values

###Long-term###
Bej_data$kz_PM_annual <- kz(Bej_data$pm, m = 365, k = 3)
Bej_data$kz_ozone_annual <- kz(Bej_data$ozone, m = 365, k = 3)
Bej_data$kz_Temp_annual <- kz(Bej_data$temp, m = 365, k = 3)
Bej_data$kz_Td_annual <- kz(Bej_data$dewtemp, m = 365, k = 3)
Bej_data$kz_Ws_annual <- kz(Bej_data$windspeed, m = 365, k = 3)
Bej_data$kz_RH_annual <- kz(Bej_data$RH_model, m = 365, k = 3)
Bej_data$kz_PBL_annual <- kz(Bej_data$PBL_model, m = 365, k = 3)

##longterm, remaining##
Bej_data$PM_LT_rem <- Bej_data$pm - Bej_data$kz_PM_annual
Bej_data$ozone_LT_rem <- Bej_data$ozone - Bej_data$kz_ozone_annual
Bej_data$Temp_LT_rem <- Bej_data$temp - Bej_data$kz_Temp_annual
Bej_data$Td_LT_rem <- Bej_data$dewtemp - Bej_data$kz_Td_annual
Bej_data$Ws_LT_rem <- Bej_data$windspeed - Bej_data$kz_Ws_annual
Bej_data$kz_RH_rem <- Bej_data$RH_model - Bej_data$kz_RH_annual
Bej_data$kz_PBL_rem <- Bej_data$PBL_model - Bej_data$kz_PBL_annual

##Seasonal##
Bej_data$kz_PM_seasonal <- kz(Bej_data$PM_LT_rem, m = 15, k = 5)
Bej_data$kz_ozone_seasonal <- kz(Bej_data$ozone_LT_rem, m = 15, k = 5)
Bej_data$kz_Temp_seasonal <- kz(Bej_data$Temp_LT_rem, m = 15, k = 5)
Bej_data$kz_Td_seasonal <- kz(Bej_data$Td_LT_rem, m = 15, k = 5)
Bej_data$kz_Ws_seasonal <- kz(Bej_data$Ws_LT_rem, m = 15, k = 5)
Bej_data$kz_RH_seasonal <- kz(Bej_data$kz_RH_rem, m = 15, k = 5)
Bej_data$kz_PBL_seasonal <- kz(Bej_data$kz_PBL_rem, m = 15, k = 5)


###Short-term###
Bej_data$PM_STM <- Bej_data$PM_LT_rem - Bej_data$kz_PM_seasonal
Bej_data$ozone_STM <- Bej_data$ozone_LT_rem - Bej_data$kz_ozone_seasonal
Bej_data$Temp_STM <- Bej_data$Temp_LT_rem - Bej_data$kz_Temp_seasonal
Bej_data$Td_STM <- Bej_data$Td_LT_rem - Bej_data$kz_Td_seasonal
Bej_data$Ws_STM <- Bej_data$Ws_LT_rem - Bej_data$kz_Ws_seasonal
Bej_data$RH_STM <- Bej_data$kz_RH_rem - Bej_data$kz_RH_seasonal 
Bej_data$PBL_STM <- Bej_data$kz_PBL_rem - Bej_data$kz_PBL_seasonal 


#save data
fwrite(Bej_data, file = "Bej_data.stm.csv")

#subset to remove irrelevant columns
Bej_data_stm <- subset(Bej_data, select = c(Date, PM_STM, ozone_STM, 
                                            Temp_STM, Td_STM, Ws_STM,
                                            Wdfac, PRCPBool, RH_STM, PBL_STM, dayf))

#save this data too
fwrite(Bej_data_stm, file = "Bej_data_for_det.csv")
#faster and smaller way
library(fst)
write.fst( Bej_data_stm, "Bej.fst")


lm_comb <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + winddir + precip_STM + RH_STM + PBL_STM +
                tempmod_STM + wsmod_STM + wind_dir_model + as.factor(days),
              data = Bej_data,
              data = Bej_data)
summary(lm_comb)

lm_comb <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + winddir + precip_STM + RH_STM + PBL_STM +
                tempmod_STM + wsmod_STM + wind_dir_model + as.factor(days),
              data = Bej_data)
summary(lm_comb)

lm_comb_1 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + winddir + precip_STM + RH_STM + PBL_STM +
                tempmod_STM + wsmod_STM + wind_dir_model + as.factor(days),
              data = Bej_daily)
summary(lm_comb_1)

lm_comb_1 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + winddir + precip_STM + RH_STM + PBL_STM +
                tempmod_STM + wsmod_STM + wind_dir_model + as.factor(days),
              data = Bej_daily)
summary(lm_comb_1)

lm_comb_10 <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + Wdf + as.factor(days),
                 data = Bej_daily)
summary(lm_comb_10)

lm_comb_3 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                   Ws_STM + Wdf + as.factor(days) + Sky_p,
                 data = Bej_daily)
summary(lm_comb_3)
