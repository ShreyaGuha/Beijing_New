####Meteorological detrending for PM2.5 and ozone for Beijing 2011-18####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###


###Preparing the Data###
##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function
library(dplyr)

##read data
Beijing_2011_18 = read.csv("Beijing_2011_18.csv")

#calculate wind speed
Beijing_2011_18$wind <- sqrt (Beijing_2011_18$u10^2 + Beijing_2011_18$v10^2)

##Manipulate the erroneous values##
#Replace negative numbers in PM2.5 data with NA
Beijing_2011_18$PM2.5_avg_conc[Beijing_2011_18$PM2.5_avg_conc < 0] <- NA
Beijing_2011_18$TCO[Beijing_2011_18$TCO < 0] <- NA
Beijing_2011_18$PRCP[Beijing_2011_18$PRCP < 0] <- NA
Beijing_2011_18$TAVG[Beijing_2011_18$TAVG < 0] <- NA
Beijing_2011_18$RH[Beijing_2011_18$RH < 0] <- NA
Beijing_2011_18$wind[Beijing_2011_18$wind < 0] <- NA
Beijing_2011_18$PBL[Beijing_2011_18$PBL < 0] <- NA
Beijing_2011_18$Ozone[Beijing_2011_18$Ozone < 0] <- NA

#replace NAs with zero
Beijing_2011_18[is.na(Beijing_2011_18)] = 0


##create new column for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
Beijing_2011_18$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
Beijing_2011_18$year <- as.factor(format(Beijing_2011_18$date, "%y"))
Beijing_2011_18$month <- as.numeric(format(Beijing_2011_18$date, "%m"))
Beijing_2011_18$dayno <- seq(1,2922, by = 1) #dummy variable for days

#statistical libraries
install.packages("Metrics")
install.packages("tdr")
library(Metrics)
library(lattice)
library(tdr)

##storing minimum and maximum values for un-scaling in future
meanvec <- sapply(Beijing_numeric,mean)
sdvec <- sapply(Beijing_numeric,sd)

##scaling data##
#define scaling function8
scaledf <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}
#not intending to scale PM2.5 or ozone, hence excluded
Beijing_2011_18_scale <- cbind(as.data.frame(Beijing_2011_18[-c(4,5,6,10,12)]),
                               lapply(Beijing_2011_18[c(4,5,6,10,12)], scaledf))
Beijing_2011_18 <- Beijing_2011_18_scale


##PM2.5 and PRCP have logarithmic distributions, linear regression models assume normal distributions
#Transforming into normal distribution 
#Beijing_2011_18$PM2.5_avg_conc <- log10(Beijing_2011_18$PM2.5_avg_conc + 1)
#Beijing_2011_18$PRCP <- log10(Beijing_2011_18$PRCP + 1)

###Introducing Detrending by KZ filter###
###Using KZ filter###
install.packages("kza")
library("kza")

#kz_annual <- function(x) {
#return(kz(x, m=365, k = 3))
#}

###Long-term###
Beijing_2011_18$kz_PM_annual <- kz(Beijing_2011_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2011_18$kz_TCO_annual <- kz(Beijing_2011_18$TCO, m = 365, k = 3)
Beijing_2011_18$kz_ozone_annual <- kz(Beijing_2011_18$Ozone, m = 365, k = 3)
Beijing_2011_18$kz_prec_annual <- kz(Beijing_2011_18$PRCP, m = 365, k = 3)
Beijing_2011_18$kz_temp_annual <- kz(Beijing_2011_18$TAVG, m = 365, k = 3)
Beijing_2011_18$kz_RH_annual <- kz(Beijing_2011_18$RH, m = 365, k = 3)
Beijing_2011_18$kz_wind_annual <- kz(Beijing_2011_18$wind, m = 365, k =3)
Beijing_2011_18$kz_pbl_annual <- kz(Beijing_2011_18$PBL, m = 365, k =3)

##longterm, remaining##
Beijing_2011_18$PM_LT_rem <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$kz_PM_annual
Beijing_2011_18$TCO_LT_rem <- Beijing_2011_18$TCO - Beijing_2011_18$kz_TCO_annual
Beijing_2011_18$Ozone_LT_rem <- Beijing_2011_18$Ozone - Beijing_2011_18$kz_ozone_annual
Beijing_2011_18$prec_LT_rem <- Beijing_2011_18$PRCP - Beijing_2011_18$kz_prec_annual
Beijing_2011_18$temp_LT_rem <- Beijing_2011_18$TAVG - Beijing_2011_18$kz_temp_annual
Beijing_2011_18$rh_LT_rem <- Beijing_2011_18$RH - Beijing_2011_18$kz_RH_annual
Beijing_2011_18$wind_LT_rem <- Beijing_2011_18$wind - Beijing_2011_18$kz_wind_annual
Beijing_2011_18$pbl_LT_rem <- Beijing_2011_18$PBL - Beijing_2011_18$kz_pbl_annual

##Seasonal##
Beijing_2011_18$kz_PM_seasonal <- kz(Beijing_2011_18$PM_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_TCO_seasonal <- kz(Beijing_2011_18$TCO_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_Ozone_seasonal <- kz(Beijing_2011_18$Ozone_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_prec_seasonal <- kz(Beijing_2011_18$prec_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_temp_seasonal <- kz(Beijing_2011_18$temp_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_rh_seasonal <- kz(Beijing_2011_18$rh_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_wind_seasonal <- kz(Beijing_2011_18$wind_LT_rem, m = 15, k = 5)
Beijing_2011_18$kz_pbl_seasonal <- kz(Beijing_2011_18$pbl_LT_rem, m = 15, k = 5)


###Short-term###
Beijing_2011_18$PM_STM <- Beijing_2011_18$PM_LT_rem - Beijing_2011_18$kz_PM_seasonal
Beijing_2011_18$TCO_STM <- Beijing_2011_18$TCO_LT_rem - Beijing_2011_18$kz_TCO_seasonal
Beijing_2011_18$Ozone_STM <- Beijing_2011_18$Ozone_LT_rem - Beijing_2011_18$kz_Ozone_seasonal
Beijing_2011_18$prec_STM <- Beijing_2011_18$prec_LT_rem - Beijing_2011_18$kz_prec_seasonal
Beijing_2011_18$temp_STM <- Beijing_2011_18$temp_LT_rem - Beijing_2011_18$kz_temp_seasonal
Beijing_2011_18$rh_STM <- Beijing_2011_18$rh_LT_rem - Beijing_2011_18$kz_rh_seasonal
Beijing_2011_18$wind_STM <- Beijing_2011_18$wind_LT_rem - Beijing_2011_18$kz_wind_annual
Beijing_2011_18$pbl_STM <- Beijing_2011_18$pbl_LT_rem - Beijing_2011_18$kz_pbl_annual


##Calculate residual for pollutant concentration##
Beijing_2011_18$PM_res <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$PM_STM
Beijing_2011_18$ozone_res <- Beijing_2011_18$Ozone - Beijing_2011_18$Ozone_STM
Beijing_2011_18$TCO_res <- Beijing_2011_18$TCO - Beijing_2011_18$TCO_STM

##correlation_models for short-term data, detrended##
#For PM2.5
lm_comb_2 <- lm(PM_STM ~ -1 + temp_STM +
                  prec_STM + rh_STM +
                  wind_STM + pbl_STM , data = Beijing_2011_18)
summary(lm_comb_2)


##correlation_models for PM2.5 for short-term data to check yearly contributions##
lm_comb <- lm(PM_STM ~ -1 + temp_STM * year +
                prec_STM *year + rh_STM *year +
                wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb)

lm_comb <- lm(Ozone_STM ~ -1 + temp_STM * year +
                prec_STM *year + rh_STM *year +
                wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb)




#extract coefficients
coef_lm_comb <- coef(lm_comb)
#convert it into data frame
#coef_lm_comb <- data.frame(coef_lm_comb)


##Un-scaling the data##
unscale <- function(x,mean,sd) {
  x*(sd) + mean
}  
Beijing_2011_18_unscale <- as.data.frame(Map(unscale, Beijing_2011_18_scale[c(12, 13, 14, 15, 16)],
                                             meanvec, sdvec))
Beijing_2011_18<- cbind(as.data.frame(Beijing_2011_18[-c(12, 13, 14, 15, 16)]),
                        as.data.frame(Beijing_2011_18_unscale[c(1, 2, 3, 4, 5)]))


#Recreate yearly contributions
Beijing_2011 <- subset.data.frame(Beijing_2011_18, year == "11")
PM_temp_11 <- (coef_lm_comb["temp_STM"] * Beijing_2011$TAVG) 
PM_RH_11 <- (coef_lm_comb["rh_STM"] * Beijing_2011$RH) 
PM_wind_11 <- (coef_lm_comb["wind_STM"] * Beijing_2011$wind) 
PM_prec_11 <- (coef_lm_comb["prec_STM"] * Beijing_2011$PRCP) 
PM_pbl_11 <- (coef_lm_comb["pbl_STM"] * Beijing_2011$PBL) 
PM_11 <- coef_lm_comb["year11"] + PM_temp_11 + PM_RH_11 + PM_wind_11 + PM_prec_11 + PM_pbl_11

Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb["temp_STM"] * Beijing_2012$TAVG) + (coef_lm_comb["temp_STM:year12"] * Beijing_2012$temp_STM)
PM_RH_12 <- (coef_lm_comb["rh_STM"] * Beijing_2012$RH) + (coef_lm_comb["year12:rh_STM"] * Beijing_2012$rh_STM)
PM_wind_12 <- (coef_lm_comb["wind_STM"] * Beijing_2012$wind) + (coef_lm_comb["year12:wind_STM"] * Beijing_2012$wind_STM)
PM_prec_12 <- (coef_lm_comb["prec_STM"] * Beijing_2012$PRCP) + (coef_lm_comb["year12:prec_STM"] * Beijing_2012$prec_STM)
PM_pbl_12 <- (coef_lm_comb["pbl_STM"] * Beijing_2012$PBL) + (coef_lm_comb["year12:pbl_STM"] * Beijing_2012$pbl_STM)
PM_12 <- coef_lm_comb["year12"] + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb["temp_STM"] * Beijing_2013$TAVG) + (coef_lm_comb["temp_STM:year13"] * Beijing_2013$temp_STM)
PM_RH_13 <- (coef_lm_comb["rh_STM"] * Beijing_2013$RH) + (coef_lm_comb["year13:rh_STM"] * Beijing_2013$rh_STM)
PM_wind_13 <- (coef_lm_comb["wind_STM"] * Beijing_2013$wind) + (coef_lm_comb["year13:wind_STM"] * Beijing_2013$wind_STM)
PM_prec_13 <- (coef_lm_comb["prec_STM"] * Beijing_2013$PRCP) + (coef_lm_comb["year13:prec_STM"] * Beijing_2013$prec_STM)
PM_pbl_13 <- (coef_lm_comb["pbl_STM"] * Beijing_2013$PBL) + (coef_lm_comb["year13:pbl_STM"] * Beijing_2013$pbl_STM)
PM_13 <- coef_lm_comb["year13"] + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb["temp_STM"] * Beijing_2014$TAVG) + (coef_lm_comb["temp_STM:year14"] * Beijing_2014$temp_STM)
PM_RH_14 <- (coef_lm_comb["rh_STM"] * Beijing_2014$RH) + (coef_lm_comb["year14:rh_STM"] * Beijing_2014$rh_STM)
PM_wind_14 <- (coef_lm_comb["wind_STM"] * Beijing_2014$wind) + (coef_lm_comb["year14:wind_STM"] * Beijing_2014$wind_STM)
PM_prec_14 <- (coef_lm_comb["prec_STM"] * Beijing_2014$PRCP) + (coef_lm_comb["year14:prec_STM"] * Beijing_2014$prec_STM)
PM_pbl_14 <- (coef_lm_comb["pbl_STM"] * Beijing_2014$PBL) + (coef_lm_comb["year14:pbl_STM"] * Beijing_2014$pbl_STM)
PM_14 <- coef_lm_comb["year14"] + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb["temp_STM"] * Beijing_2015$TAVG) + (coef_lm_comb["temp_STM:year15"] * Beijing_2015$temp_STM)
PM_RH_15 <- (coef_lm_comb["rh_STM"] * Beijing_2015$RH) + (coef_lm_comb["year15:rh_STM"] * Beijing_2015$rh_STM)
PM_wind_15 <- (coef_lm_comb["wind_STM"] * Beijing_2015$wind) + (coef_lm_comb["year15:wind_STM"] * Beijing_2015$wind_STM)
PM_prec_15 <- (coef_lm_comb["prec_STM"] * Beijing_2015$PRCP) + (coef_lm_comb["year15:prec_STM"] * Beijing_2015$prec_STM)
PM_pbl_15 <- (coef_lm_comb["pbl_STM"] * Beijing_2015$PBL) + (coef_lm_comb["year15:pbl_STM"] * Beijing_2015$pbl_STM)
PM_15 <- coef_lm_comb["year15"] + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb["temp_STM"] * Beijing_2016$TAVG) + (coef_lm_comb["temp_STM:year16"] * Beijing_2016$temp_STM)
PM_RH_16 <- (coef_lm_comb["rh_STM"] * Beijing_2016$RH) + (coef_lm_comb["year16:rh_STM"] * Beijing_2016$rh_STM)
PM_wind_16 <- (coef_lm_comb["wind_STM"] * Beijing_2016$wind) + (coef_lm_comb["year16:wind_STM"] * Beijing_2016$wind_STM)
PM_prec_16 <- (coef_lm_comb["prec_STM"] * Beijing_2016$PRCP) + (coef_lm_comb["year16:prec_STM"] * Beijing_2016$prec_STM)
PM_pbl_16 <- (coef_lm_comb["pbl_STM"] * Beijing_2016$PBL) + (coef_lm_comb["year16:pbl_STM"] * Beijing_2016$pbl_STM)
PM_16 <- coef_lm_comb["year16"] + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb["temp_STM"] * Beijing_2017$TAVG) + (coef_lm_comb["temp_STM:year17"] * Beijing_2017$temp_STM)
PM_RH_17 <- (coef_lm_comb["rh_STM"] * Beijing_2017$RH) + (coef_lm_comb["year17:rh_STM"] * Beijing_2017$rh_STM)
PM_wind_17 <- (coef_lm_comb["wind_STM"] * Beijing_2017$wind) + (coef_lm_comb["year17:wind_STM"] * Beijing_2017$wind_STM)
PM_prec_17 <- (coef_lm_comb["prec_STM"] * Beijing_2017$PRCP) + (coef_lm_comb["year17:prec_STM"] * Beijing_2017$prec_STM)
PM_pbl_17 <- (coef_lm_comb["pbl_STM"] * Beijing_2017$PBL) + (coef_lm_comb["year17:pbl_STM"] * Beijing_2017$pbl_STM)
PM_17 <- coef_lm_comb["year17"] + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb["temp_STM"] * Beijing_2018$TAVG) + (coef_lm_comb["temp_STM:year18"] * Beijing_2018$temp_STM)
PM_RH_18 <- (coef_lm_comb["rh_STM"] * Beijing_2018$RH) + (coef_lm_comb["year18:rh_STM"] * Beijing_2018$rh_STM)
PM_wind_18 <- (coef_lm_comb["wind_STM"] * Beijing_2018$wind) + (coef_lm_comb["year18:wind_STM"] * Beijing_2018$wind_STM)
PM_prec_18 <- (coef_lm_comb["prec_STM"] * Beijing_2018$PRCP) + (coef_lm_comb["year18:prec_STM"] * Beijing_2018$prec_STM)
PM_pbl_18 <- (coef_lm_comb["pbl_STM"] * Beijing_2018$PBL) + (coef_lm_comb["year18:pbl_STM"] * Beijing_2018$pbl_STM)
PM_18 <- coef_lm_comb["year18"] + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18

#calling library functions
library(data.table)

#plot yearly contributions separately
dt.tot <- data.table(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18)
dt.tot$days <- seq(1,366, by = 1)
dt.tot.melt <- melt(dt.tot, id.vars = c('days'))
dt.tot.melt$days <- as.numeric( dt.tot.melt$days)


theme_set(theme_bw())
ggplot( dt.tot.melt) + (aes( x = days, y = value,
                             group = variable, alpha = variable)) +
  geom_line() + 
  facet_wrap( . ~ variable)


#plot yearly contributions together
PM_met <- rbind(c(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18))
PM_met <- t(PM_met)
PM_met <- as.data.table(PM_met)
PM_met$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_met$year <- as.factor(format(PM_met$date, "%y"))

PM_met.melt <- melt(PM_met, id.vars = c('date', 'year'))
PM_met.melt$date <- as.Date( PM_met.melt$date)
PM_met.melt$year <- as.factor(format(PM_met.melt$date, "%y"))


theme_set(theme_bw())
ggplot( PM_met.melt) + (aes( x = date, y = value,
                             group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 detrended concentrations (ug/cc)") +
  ggtitle("PM2.5 detrended concentrations due to meteorological influences") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 



##plot yearly contributions using ggplot##
library(tidyverse)
library(reshape)
library(ggplot2)

#Applying same model for raw data, not detrended
lm_comb_raw <- lm(PM2.5_avg_conc ~ -1 + TAVG * year +
                    PRCP *year + RH *year +
                    wind * year + PBL*year, data = Beijing_2011_18)
summary(lm_comb_raw)

#extract coefficients
coef_lm_comb_raw <- coef(lm_comb_raw)


#Recreate yearly contributions
Beijing_2011 <- subset.data.frame(Beijing_2011_18, year == "11")
PM_temp_11 <- (coef_lm_comb_raw["TAVG"] * Beijing_2011$TAVG) 
PM_RH_11 <- (coef_lm_comb_raw["RH"] * Beijing_2011$RH) 
PM_wind_11 <- (coef_lm_comb_raw["wind"] * Beijing_2011$wind) 
PM_prec_11 <- (coef_lm_comb_raw["PRCP"] * Beijing_2011$PRCP) 
PM_pbl_11 <- (coef_lm_comb_raw["PBL"] * Beijing_2011$PBL) 
PM_11 <- coef_lm_comb_raw["year11"] + PM_temp_11 + PM_RH_11 + PM_wind_11 + PM_prec_11 + PM_pbl_11

Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb_raw["TAVG"] * Beijing_2012$TAVG) + (coef_lm_comb_raw["TAVG:year12"] * Beijing_2012$TAVG)
PM_RH_12 <- (coef_lm_comb_raw["RH"] * Beijing_2012$RH) + (coef_lm_comb_raw["year12:RH"] * Beijing_2012$RH)
PM_wind_12 <- (coef_lm_comb_raw["wind"] * Beijing_2012$wind) + (coef_lm_comb_raw["year12:wind"] * Beijing_2012$wind)
PM_prec_12 <- (coef_lm_comb_raw["PRCP"] * Beijing_2012$PRCP) + (coef_lm_comb_raw["year12:PRCP"] * Beijing_2012$PRCP)
PM_pbl_12 <- (coef_lm_comb_raw["PBL"] * Beijing_2012$PBL) + (coef_lm_comb_raw["year12:PBL"] * Beijing_2012$PBL)
PM_12 <- coef_lm_comb_raw["year12"] + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb_raw["TAVG"] * Beijing_2013$TAVG) + (coef_lm_comb_raw["TAVG:year13"] * Beijing_2013$TAVG)
PM_RH_13 <- (coef_lm_comb_raw["RH"] * Beijing_2013$RH) + (coef_lm_comb_raw["year13:RH"] * Beijing_2013$RH)
PM_wind_13 <- (coef_lm_comb_raw["wind"] * Beijing_2013$wind) + (coef_lm_comb_raw["year13:wind"] * Beijing_2013$wind)
PM_prec_13 <- (coef_lm_comb_raw["PRCP"] * Beijing_2013$PRCP) + (coef_lm_comb_raw["year13:PRCP"] * Beijing_2013$PRCP)
PM_pbl_13 <- (coef_lm_comb_raw["PBL"] * Beijing_2013$PBL) + (coef_lm_comb_raw["year13:PBL"] * Beijing_2013$PBL)
PM_13 <- coef_lm_comb_raw["year13"] + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb_raw["TAVG"] * Beijing_2014$TAVG) + (coef_lm_comb_raw["TAVG:year14"] * Beijing_2014$TAVG)
PM_RH_14 <- (coef_lm_comb_raw["RH"] * Beijing_2014$RH) + (coef_lm_comb_raw["year14:RH"] * Beijing_2014$RH)
PM_wind_14 <- (coef_lm_comb_raw["wind"] * Beijing_2014$wind) + (coef_lm_comb_raw["year14:wind"] * Beijing_2014$wind)
PM_prec_14 <- (coef_lm_comb_raw["PRCP"] * Beijing_2014$PRCP) + (coef_lm_comb_raw["year14:PRCP"] * Beijing_2014$PRCP)
PM_pbl_14 <- (coef_lm_comb_raw["PBL"] * Beijing_2014$PBL) + (coef_lm_comb_raw["year14:PBL"] * Beijing_2014$PBL)
PM_14 <- coef_lm_comb_raw["year14"] + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb_raw["TAVG"] * Beijing_2015$TAVG) + (coef_lm_comb_raw["TAVG:year15"] * Beijing_2015$TAVG)
PM_RH_15 <- (coef_lm_comb_raw["RH"] * Beijing_2015$RH) + (coef_lm_comb_raw["year15:RH"] * Beijing_2015$RH)
PM_wind_15 <- (coef_lm_comb_raw["wind"] * Beijing_2015$wind) + (coef_lm_comb_raw["year15:wind"] * Beijing_2015$wind)
PM_prec_15 <- (coef_lm_comb_raw["PRCP"] * Beijing_2015$PRCP) + (coef_lm_comb_raw["year15:PRCP"] * Beijing_2015$PRCP)
PM_pbl_15 <- (coef_lm_comb_raw["PBL"] * Beijing_2015$PBL) + (coef_lm_comb_raw["year15:PBL"] * Beijing_2015$PBL)
PM_15 <- coef_lm_comb_raw["year15"] + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb_raw["TAVG"] * Beijing_2016$TAVG) + (coef_lm_comb_raw["TAVG:year16"] * Beijing_2016$TAVG)
PM_RH_16 <- (coef_lm_comb_raw["RH"] * Beijing_2016$RH) + (coef_lm_comb_raw["year16:RH"] * Beijing_2016$RH)
PM_wind_16 <- (coef_lm_comb_raw["wind"] * Beijing_2016$wind) + (coef_lm_comb_raw["year16:wind"] * Beijing_2016$wind)
PM_prec_16 <- (coef_lm_comb_raw["PRCP"] * Beijing_2016$PRCP) + (coef_lm_comb_raw["year16:PRCP"] * Beijing_2016$PRCP)
PM_pbl_16 <- (coef_lm_comb_raw["PBL"] * Beijing_2016$PBL) + (coef_lm_comb_raw["year16:PBL"] * Beijing_2016$PBL)
PM_16 <- coef_lm_comb_raw["year16"] + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb_raw["TAVG"] * Beijing_2017$TAVG) + (coef_lm_comb_raw["TAVG:year17"] * Beijing_2017$TAVG)
PM_RH_17 <- (coef_lm_comb_raw["RH"] * Beijing_2017$RH) + (coef_lm_comb_raw["year17:RH"] * Beijing_2017$RH)
PM_wind_17 <- (coef_lm_comb_raw["wind"] * Beijing_2017$wind) + (coef_lm_comb_raw["year17:wind"] * Beijing_2017$wind)
PM_prec_17 <- (coef_lm_comb_raw["PRCP"] * Beijing_2017$PRCP) + (coef_lm_comb_raw["year17:PRCP"] * Beijing_2017$PRCP)
PM_pbl_17 <- (coef_lm_comb_raw["PBL"] * Beijing_2017$PBL) + (coef_lm_comb_raw["year17:PBL"] * Beijing_2017$PBL)
PM_17 <- coef_lm_comb_raw["year17"] + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb_raw["TAVG"] * Beijing_2018$TAVG) + (coef_lm_comb_raw["TAVG:year18"] * Beijing_2018$TAVG)
PM_RH_18 <- (coef_lm_comb_raw["RH"] * Beijing_2018$RH) + (coef_lm_comb_raw["year18:RH"] * Beijing_2018$RH)
PM_wind_18 <- (coef_lm_comb_raw["wind"] * Beijing_2018$wind) + (coef_lm_comb_raw["year18:wind"] * Beijing_2018$wind)
PM_prec_18 <- (coef_lm_comb_raw["PRCP"] * Beijing_2018$PRCP) + (coef_lm_comb_raw["year18:PRCP"] * Beijing_2018$PRCP)
PM_pbl_18 <- (coef_lm_comb_raw["PBL"] * Beijing_2018$PBL) + (coef_lm_comb_raw["year18:PBL"] * Beijing_2018$PBL)
PM_18 <- coef_lm_comb_raw["year18"] + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18


#plot yearly contributions separately
dt.tot.raw <- data.table(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18)
dt.tot.raw$days <- seq(1,366, by = 1)
dt.tot.raw.melt <- melt(dt.tot.raw, id.vars = c('days'))
dt.tot.raw.melt$days <- as.numeric( dt.tot.raw.melt$days)


theme_set(theme_bw())
ggplot( dt.tot.raw.melt) + (aes( x = days, y = value,
                                 group = variable, alpha = variable)) +
  geom_line() + 
  facet_wrap( . ~ variable)

#plot yearly contributions together
PM_met_raw <- rbind(c(PM_11, PM_12, PM_13, PM_14, PM_15, PM_16, PM_17, PM_18))
PM_met_raw <- t(PM_met_raw)
PM_met_raw <- as.data.table(PM_met_raw)
PM_met_raw$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_met_raw$year <- format(PM_met_raw$date, "%Y")

PM_met_raw.melt <- melt(PM_met_raw, id.vars = c('date', 'year'))
PM_met_raw.melt$date <- as.Date( PM_met_raw.melt$date)


theme_set(theme_bw())
ggplot( PM_met_raw.melt) + (aes( x = date, y = value,
                                 group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 raw concentrations (ug/cc)") +
  ggtitle("PM2.5 raw concentrations due to meteorological influences") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 





theme_set(theme_bw())
ggplot( PM_res.melt) + (aes( x = date, y = value,
                             group = variable, color = variable)) +
  geom_line() +
  xlab("time(years)") +
  ylab("PM2.5 residual concentrations (ug/cc)") +
  ggtitle("PM2.5 residual concentrations due to emission changes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  theme(legend.title = element_blank(), legend.position = "none") 


#Everything together
#Daily
ggplot() + 
  geom_line(data = Beijing_raw_2.melt, aes(x = date, y = value, color = "grey")) +
  geom_line(data = PM_res.melt, aes(x = date, y = value), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "STM removed" = "black"))
#Annual
#Everything by year, calculate yearly mean
#PM_raw_year <-aggregate.data.frame(PM_met_raw.melt$value, list(PM_met_raw.melt$year), 
                                   #FUN = mean)
#PM_raw_year$year <- as.numeric(PM_raw_year$Group.1)
#PM_raw_year$PM <- PM_raw_year$x

#raw data
#build a subset with only the raw pollutant concentration, no model run
Beijing_raw_2 <- subset(Beijing_2011_18, select = c(PM2.5_avg_conc, year, date))
Beijing_raw_2$date <- as.Date( Beijing_raw_2$date)
Beijing_raw_2$year <- format(Beijing_raw_2$date, "%Y")
Beijing_raw_2$year <- as.numeric(Beijing_raw_2$year)
PM_raw_year <-aggregate.data.frame(Beijing_raw_2$PM2.5_avg_conc, list(Beijing_raw_2$year), 
                                   FUN = mean)
PM_raw_year$year <- as.numeric(PM_raw_year$Group.1)
PM_raw_year$PM <- PM_raw_year$x

#residual
#calculating the residuals
PM_res <- Beijing_raw_2$PM2.5_avg_conc - PM_met.melt$value
PM_res <- as.data.table(PM_res)
PM_res$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
PM_res$year <- format(PM_res$date, "%Y")

PM_res.melt <- melt(PM_res, id.vars = c('date', 'year'))
PM_res.melt$date <- as.Date( PM_res.melt$date)
PM_res_year <- aggregate.data.frame(PM_res.melt$value, list(PM_res.melt$year), 
                                    FUN = mean)
PM_res_year$year <- as.numeric(PM_res_year$Group.1)
PM_res_year$PM <- PM_res_year$x


#plotting
ggplot() + 
  geom_line(data = PM_raw_year, aes(x = year, y = PM, color = "grey")) +
  geom_line(data = PM_res_year, aes(x = year, y = PM), color ="black") +
  ggtitle("Raw & Meteorologically detrended PM2.5 concentrations") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab('PM2.5 concentration(ug/cc)') +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "STM removed" = "black"))

#Poster plot (1)
#Plotting the methods: what is kz filter/detrending
#build a subset with only the variables required
Beijing_plot_2 <- subset(Beijing_2011_18, select = c(PM2.5_avg_conc, kz_PM_annual, 
                                                     kz_PM_seasonal, PM_STM,
                                                     year, date))

Beijing_plot_2 <- rename(Beijing_plot_2, c("PM2.5_avg_conc" = "Average Concentration of pollutant",
                                           "kz_PM_annual" = "Pollutant concentration, annual influences removed",
                                           "kz_PM_seasonal" = "Pollutant concentration, seasonal influences removed",
                                           "PM_STM" = "Pollutant concentration, short-term influences removed"))

#use melt function to define the x-axis, against what you're plotting
Beijing_plot_2.melt <- melt( Beijing_plot_2, id.vars = c( 'year', 'date'))
Beijing_plot_2.melt$date <- as.Date( Beijing_plot_2.melt$date)



#set background theme
theme_set(theme_bw())
#plot
ggplot( Beijing_plot_2.melt) + (aes( x = date, y = value,
                                     group = variable)) +
  geom_line(color = "black") + 
  ggtitle("Method: What is kz-filter?") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  xlab('time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/cc)"))) +
  facet_wrap( . ~ variable)



#Poster Plot(2)
#Results of Detrending

install.packages("patchwork")
library(patchwork)
install.packages("ggpubr")
library(ggpubr)

p1 <- ggplot() + 
  geom_point(data = Beijing_raw_2.melt, aes(x = date, y = value, color = "grey")) +
  geom_point(data = PM_res.melt, aes(x = date, y = value), color ="black") +
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/cc)"))) +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "STM removed" = "black"))

p2 <- ggplot() + 
  geom_line(data = PM_raw_year, aes(x = year, y = PM, color = "grey")) +
  geom_line(data = PM_res_year, aes(x = year, y = PM), color ="black") +
  labs(title=(expression(paste("Annually averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/cc)"))) +
  scale_color_manual(name = "Trend", values = c("Observed" = "grey", 
                                                "STM removed" = "black"))
#p1 + p2 + plot_layout(ncol=1)
ggarrange(p1, p2, ncol = 1, common.legend = TRUE, legend = "bottom")


#Plot(3)
#Result: Effect of meteorology on STM scale on PM2.5 concentration

#build a subset with only the STM variables, detrended pollutant concemtration,
#the detrended met coefficients, & temporal variables
Beijing_plot_STM <- subset(Beijing_2011_18, select = c(PM_STM, temp_STM, prec_STM, 
                                                       wind_STM, rh_STM, pbl_STM,
                                                       year, date))

Beijing_plot_STM <- rename(Beijing_plot_STM, c("PM_STM" = "Average Concentration of PM2.5",
                                               "temp_STM" = "Temperature",
                                               "prec_STM" = "Precipitation",
                                               "wind_STM" = "Wind",
                                               "rh_STM" = "Relative Humidity",
                                               "pbl_STM" = "Height of PBL"))

#use melt function to define the x-axis, against what you're plotting
Beijing_plot_STM.melt <- melt( Beijing_plot_STM, id.vars = c( 'year', 'date'))
Beijing_plot_STM.melt$date <- as.Date( Beijing_plot_STM.melt$date)

#set background theme
theme_set(theme_bw())
#plot_STM
ggplot( Beijing_plot_STM.melt) + (aes( x = date, y = value,
                                       group = variable)) +
  theme(axis.title = element_blank()) +
  geom_line(color="black") +
  labs(title=(expression(paste("Effect of meteorology on " , 
                                   PM[2.5] ,
                                  " concentration (ug/cc) at short-term scale")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  #scale_x_continuous(breaks = unique(Beijing_plot_STM.melt$year)) +
  facet_wrap( . ~ variable, scales = "free")

