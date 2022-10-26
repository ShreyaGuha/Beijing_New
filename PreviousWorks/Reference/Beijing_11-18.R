####Meteorological detrending for PM2.5 and ozone for Beijing 2011-18####
###Goal: To check relations between different meteorological signals and atmospheric pollutants###


###Preparing the Data###
##using specific library functions##
install.packages("data.table") #installing package
library(data.table) #calling library function

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

##storing minimum and maximum values for denormalizing in future
minvec <- sapply(Beijing_2011_18,min)
maxvec <- sapply(Beijing_2011_18,max)

##create new column for date##
#installing specific library function for date
install.packages("lubridate")
library(lubridate)
Beijing_2011_18$date <- seq(ymd('2011-01-01'), ymd('2018-12-31'), by = "1 day")
Beijing_2011_18$year <- as.factor(format(Beijing_2011_18$date, "%y"))
Beijing_2011_18$month <- as.numeric(format(Beijing_2011_18$date, "%m"))
Beijing_2011_18$dayno <- seq(1,2922, by = 1) #dummy variable for days


##scaling data##
#define normalize function
normalize <- function(x) {
   return ((x - min(x)) / (max(x) - min(x)))
}
Beijing_2011_18$PM2.5_avg_conc <- normalize(Beijing_2011_18$PM2.5_avg_conc)
Beijing_2011_18$TCO <- normalize(Beijing_2011_18$TCO)
Beijing_2011_18$Ozone <- normalize(Beijing_2011_18$Ozone)
Beijing_2011_18$TAVG <- normalize(Beijing_2011_18$TAVG)
Beijing_2011_18$PRCP <- normalize(Beijing_2011_18$PRCP)
Beijing_2011_18$RH <- normalize(Beijing_2011_18$RH)
Beijing_2011_18$wind <- normalize(Beijing_2011_18$wind)
Beijing_2011_18$PBL <- normalize(Beijing_2011_18$PBL)


#checking distributions of the parameters involved
hist(Beijing_2011_18$PM2.5_avg_conc, breaks = 50)
hist(Beijing_2011_18$TCO)
hist(Beijing_2011_18$PRCP, breaks = 50)
hist(Beijing_2011_18$TAVG)
hist(Beijing_2011_18$RH)
hist(Beijing_2011_18$PBL)
hist(Beijing_2011_18$wind, breaks = 30)

##PM2.5 and PRCP have logarithmic distributions, linear regression models assume normal distributions
#Transforming into normal distribution
Beijing_2011_18$PM2.5_avg_conc <- log10(Beijing_2011_18$PM2.5_avg_conc + 1)
Beijing_2011_18$PRCP <- log10(Beijing_2011_18$PRCP + 1)
#checking summary and adding 1 inside log10 to avoid infinity in summary
summary(Beijing_2011_18$PM2.5_avg_conc)
summary(Beijing_2011_18$PRCP)
#check distribution again
hist(Beijing_2011_18$PM2.5_avg_conc)
hist(Beijing_2011_18$PRCP, breaks = 50)

#distribution of precipitation is still skewed
#changing it to categorical variable
Beijing_2011_18$PRCPBool [Beijing_2011_18$PRCP != 0.0000] <- 1
Beijing_2011_18$PRCPBool [Beijing_2011_18$PRCP == 0.0000] <- 0
Beijing_2011_18$PRCPBool <- as.factor(Beijing_2011_18$PRCPBool)
#check number of rainy days
summary(Beijing_2011_18$PRCPBool)



###Raw Data###
##checking basic correlation for whole time period, raw data##
Beijing_numeric <- subset(Beijing_2011_18, select = -c(X, date_new, date, days, year, u10, v10, PRCPBool)) #subseting to remove non-numeric column
cor(Beijing_numeric)
plot(Beijing_numeric)

##correlation models for raw data##
#For PM2.5
lm_PM2.5_raw <- lm(Beijing_2011_18$PM2.5_avg_conc ~ -1 + Beijing_2011_18$TAVG + Beijing_2011_18$PRCP + 
                           Beijing_2011_18$RH + Beijing_2011_18$wind + Beijing_2011_18$PBL)
summary(lm_PM2.5_raw)
plot(lm_PM2.5_raw)

lm_comb_raw <- lm(PM2.5_avg_conc ~ -1 + TAVG * year +
                     PRCP *year + RH *year +
                     wind * year + PBL*year, data = Beijing_2011_18)
summary(lm_comb_raw)

#extract coefficients
coef_lm_comb_raw <- coef(lm_comb_raw)



#To check Variance Inflation Factor (VIF)
install.packages("car")
library(car)

vif(lm_PM2.5_raw)
#all are below 2


lm_PM2.5_raw_year <- lm(Beijing_2011_18$PM2.5_avg_conc ~ -1 + Beijing_2011_18$TAVG * Beijing_2011_18$year + 
                        Beijing_2011_18$PRCP * Beijing_2011_18$year + Beijing_2011_18$RH * Beijing_2011_18$year + 
                        Beijing_2011_18$wind * Beijing_2011_18$year + Beijing_2011_18$PBL * Beijing_2011_18$year)
summary(lm_PM2.5_raw_year)
plot(lm_PM2.5_raw_year)

#extract coefficients
coef_lm_raw <- coef(lm_PM2.5_raw_year)
#convert it into data frame
coef_lm_raw <- data.frame(as.list(coef_lm_raw))


Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_raw$Beijing_2011_18.TAVG.Beijing_2011_18.year18 * Beijing_2018$TAVG)
PM_RH_18 <- (coef_lm_raw$Beijing_2011_18.year18.Beijing_2011_18.RH * Beijing_2018$RH) 
PM_wind_18 <- (coef_lm_raw$Beijing_2011_18.year18.Beijing_2011_18.wind * Beijing_2018$wind) 
PM_prec_18 <- (coef_lm_raw$Beijing_2011_18.year18.Beijing_2011_18.PRCP * Beijing_2018$PRCP)
PM_pbl_18 <- (coef_lm_raw$Beijing_2011_18.year18.Beijing_2011_18.PBL * Beijing_2018$PBL)
PM_18 <- coef_lm_raw$Beijing_2011_18.year18 + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18

par(mfrow = c(1,1))


plot(PM_18, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     xlim=c(0, 365),
     ylim = c(-0.1, 0.1))
lines(PM_temp_18, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_18, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_18, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_18, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_18, type = 'l',
      pch = 18, col = "yellow", las = 1)


#add legend
legend(x = "bottomright",
       inset = c(0, -0.25),
       xpd = TRUE,
       horiz = TRUE,
       legend=c("PM2.5", "Temp", "RH", "Wind", "Precipitation", "PBL" ),
       col=c("darkslateblue", "orange", "limegreen", "gray", "purple", "yellow"), 
       lty=1:2, cex=0.75)







#For TCO
lm_TCO_raw <- lm(Beijing_2011_18$TCO ~ -1 + Beijing_2011_18$TAVG + Beijing_2011_18$PRCP + 
                         Beijing_2011_18$RH + Beijing_2011_18$wind + Beijing_2011_18$PBL)
summary(lm_TCO_raw)
plot(lm_TCO_raw)

#For Ozone
lm_ozone_raw <- lm(Beijing_2011_18$Ozone ~ -1 + Beijing_2011_18$TAVG + Beijing_2011_18$PRCP + 
                    Beijing_2011_18$RH + Beijing_2011_18$wind + Beijing_2011_18$PBL)
summary(lm_ozone_raw)

##General Additive Models##
library(mgcv) #calling library function

gam1_raw <- gam(Beijing_2011_18$PM2.5_avg_conc ~ s(Beijing_2011_18$wind) + s(Beijing_2011_18$PRCP) + 
                    s(Beijing_2011_18$RH) + Beijing_2011_18$TAVG, method = "REML" )
summary(gam1_raw)

#plot model
plot(gam1_raw, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#show residuals
plot(gam1_raw, rug = TRUE, pages = 1, all.terms = TRUE, 
     residuals = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#check model
gam.check(gam1_raw)
concurvity(gam1_raw, full = TRUE)
concurvity(gam1_raw, full = FALSE)

#GAM smoothing temperature
gam1a_raw <- gam(Beijing_2011_18$PM2.5_avg_conc ~ s(Beijing_2011_18$wind) + s(Beijing_2011_18$PRCP) + 
                         s(Beijing_2011_18$RH) + s(Beijing_2011_18$TAVG), method = "REML" )
summary(gam1a_raw)

#plot model
plot(gam1a_raw, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#show residuals
plot(gam1a_raw, rug = TRUE, pages = 1, all.terms = TRUE, 
     residuals = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#check model
gam.check(gam1a_raw)
concurvity(gam1a_raw, full = TRUE)
concurvity(gam1a_raw, full = FALSE)

#by the concurvity test, gam1a_raw is better than gam1a, gam1a_raw has less concurvity#

#GAM with cyclic cubic spline (cc), default dimension k = 10
gamcc <- gam(Beijing_2011_18$PM2.5_avg_conc ~ s(Beijing_2011_18$month, bs = "cc", k = 4))
summary(gamcc)
plot(gamcc, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

#check model
gam.check(gamcc)
concurvity(gamcc, full = TRUE)
concurvity(gamcc, full = FALSE)


###Introducing Detrending by KZ filter###
###Using KZ filter###
install.packages("kza")
library("kza")


###Long-term###
Beijing_2011_18$kz_PM_annual <- kz(Beijing_2011_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2011_18$kz_TCO_annual <- kz(Beijing_2011_18$TCO, m = 365, k = 3)
Beijing_2011_18$kz_ozone_annual <- kz(Beijing_2011_18$Ozone, m = 365, k = 3)
Beijing_2011_18$kz_prec_annual <- kz(Beijing_2011_18$PRCP, m = 365, k = 3)
Beijing_2011_18$kz_temp_annual <- kz(Beijing_2011_18$TAVG, m = 365, k = 3)
Beijing_2011_18$kz_RH_annual <- kz(Beijing_2011_18$RH, m = 365, k = 3)
Beijing_2011_18$kz_wind_annual <- kz(Beijing_2011_18$wind, m = 365, k =3)
Beijing_2011_18$kz_pbl_annual <- kz(Beijing_2011_18$PBL, m = 365, k =3)

##Annual_correlations##
Beijing_annual <- subset(Beijing_2011_18, select = c(kz_PM_annual, kz_prec_annual, kz_temp_annual, 
                                                     kz_RH_annual, kz_TCO_annual, kz_wind_annual,
                                                     kz_pbl_annual, days))
cor(Beijing_annual)
plot(Beijing_annual)

##correlation models for annual data, detrended##
#For PM2.5
lm_PM2.5_annual <- lm(Beijing_2011_18$kz_PM_annual ~ -1 + Beijing_2011_18$kz_temp_annual + Beijing_2011_18$kz_prec_annual + 
                              Beijing_2011_18$kz_RH_annual + Beijing_2011_18$kz_wind_annual + 
                              Beijing_2011_18$kz_pbl_annual)
summary(lm_PM2.5_annual) 
plot(lm_PM2.5_annual)

lm_PM2.5_annual_year <- lm(Beijing_2011_18$kz_PM_annual ~ Beijing_2011_18$kz_temp_annual * Beijing_2011_18$year + Beijing_2011_18$kz_prec_annual * Beijing_2011_18$year + 
                                   Beijing_2011_18$kz_RH_annual * Beijing_2011_18$year + Beijing_2011_18$kz_wind_annual * Beijing_2011_18$year)
summary(lm_PM2.5_annual_year) 
plot(lm_PM2.5_annual_year)


#For TCO
lm_TCO_annual <- lm(Beijing_2011_18$kz_TCO_annual ~ - 1 + Beijing_2011_18$kz_temp_annual + Beijing_2011_18$kz_prec_annual + 
                            Beijing_2011_18$kz_RH_annual + Beijing_2011_18$kz_wind_annual + Beijing_2011_18$kz_pbl_annual)
summary(lm_TCO_annual) 
plot(lm_TCO_annual)

#For Ozone
lm_ozone_annual <- lm(Beijing_2011_18$kz_ozone_annual ~ - 1 + Beijing_2011_18$kz_temp_annual + Beijing_2011_18$kz_prec_annual + 
                       Beijing_2011_18$kz_RH_annual + Beijing_2011_18$kz_wind_annual + Beijing_2011_18$kz_pbl_annual)
summary(lm_ozone_annual) 

#GAM with cubic cyclic splines
gamccann <- gam(Beijing_2011_18$kz_PM_annual ~ s(Beijing_2011_18$month, bs = "cc", k = 4))
summary(gamccann)
plot(gamccann, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)



###Seasonal###
##Detrending calculations for seasonal##
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


##Seasonal_correlations##
Beijing_seasonal <- subset(Beijing_2011_18, select = c(kz_PM_seasonal, kz_prec_seasonal, kz_temp_seasonal, 
                                                       kz_rh_seasonal, kz_wind_seasonal, kz_TCO_seasonal,
                                                       kz_pbl_seasonal, days))
cor(Beijing_seasonal)
plot(Beijing_seasonal)

##correlation models for seasonal data, detrended##
#For PM2.5
lm_PM2.5_seasonal <- lm(Beijing_2011_18$kz_PM_seasonal ~ -1 + Beijing_2011_18$kz_temp_seasonal + Beijing_2011_18$kz_prec_seasonal + 
                        Beijing_2011_18$kz_rh_seasonal + Beijing_2011_18$kz_wind_seasonal + 
                        Beijing_2011_18$kz_pbl_seasonal)
summary(lm_PM2.5_seasonal) 
plot(lm_PM2.5_seasonal)

lm_PM2.5_seasonal_year <- lm(Beijing_2011_18$kz_PM_seasonal ~ -1 + Beijing_2011_18$kz_temp_seasonal *Beijing_2011_18$year + Beijing_2011_18$kz_prec_seasonal * Beijing_2011_18$year + 
                                     Beijing_2011_18$kz_rh_seasonal * Beijing_2011_18$year + Beijing_2011_18$kz_wind_seasonal * Beijing_2011_18$year)
summary(lm_PM2.5_seasonal_year) 
plot(lm_PM2.5_seasonal_year)


#For TCO
lm_TCO_seasonal <- lm(Beijing_2011_18$kz_TCO_seasonal ~ -1 +Beijing_2011_18$kz_temp_seasonal + Beijing_2011_18$kz_prec_seasonal + 
                              Beijing_2011_18$kz_rh_seasonal + Beijing_2011_18$kz_wind_seasonal + Beijing_2011_18$kz_pbl_seasonal)
summary(lm_TCO_seasonal) 
plot(lm_TCO_seasonal)

#For Ozone
lm_ozone_seasonal <- lm(Beijing_2011_18$kz_Ozone_seasonal ~ -1 +Beijing_2011_18$kz_temp_seasonal + Beijing_2011_18$kz_prec_seasonal + 
                         Beijing_2011_18$kz_rh_seasonal + Beijing_2011_18$kz_wind_seasonal + Beijing_2011_18$kz_pbl_seasonal)
summary(lm_ozone_seasonal)

#GAM with cubic cyclic splines
gamccseas <- gam(Beijing_2011_18$kz_PM_seasonal ~ s(Beijing_2011_18$month, bs = "cc", k = 4))
summary(gamccseas)
plot(gamccseas, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


###Short-term###
##Detrending##
Beijing_2011_18$PM_STM <- Beijing_2011_18$PM_LT_rem - Beijing_2011_18$kz_PM_seasonal
Beijing_2011_18$TCO_STM <- Beijing_2011_18$TCO_LT_rem - Beijing_2011_18$kz_TCO_seasonal
Beijing_2011_18$Ozone_STM <- Beijing_2011_18$Ozone_LT_rem - Beijing_2011_18$kz_Ozone_seasonal
Beijing_2011_18$prec_STM <- Beijing_2011_18$prec_LT_rem - Beijing_2011_18$kz_prec_seasonal
Beijing_2011_18$temp_STM <- Beijing_2011_18$temp_LT_rem - Beijing_2011_18$kz_temp_seasonal
Beijing_2011_18$rh_STM <- Beijing_2011_18$rh_LT_rem - Beijing_2011_18$kz_rh_seasonal
Beijing_2011_18$wind_STM <- Beijing_2011_18$wind_LT_rem - Beijing_2011_18$kz_wind_annual
Beijing_2011_18$pbl_STM <- Beijing_2011_18$pbl_LT_rem - Beijing_2011_18$kz_pbl_annual


##Short-term_correlations##
Beijing_STM <- subset(Beijing_2011_18, select = c(PM_STM, prec_STM, temp_STM, 
                                                  rh_STM, wind_STM, TCO_STM, pbl_STM, days))
cor(Beijing_STM)
plot(Beijing_STM)

##correlation_models for short-term data, detrended##
#For PM2.5
lm_PM2.5 <- lm(Beijing_2011_18$PM_STM ~ - 1 + Beijing_2011_18$temp_STM + Beijing_2011_18$prec_STM + 
               Beijing_2011_18$rh_STM + Beijing_2011_18$wind_STM + Beijing_2011_18$pbl_STM)
summary(lm_PM2.5) 
plot(lm_PM2.5)

#Variance Inflation Factor to check multi-collinearity
vif(lm_PM2.5)
#all are around 1

lm_PM2.5_year <- lm(Beijing_2011_18$PM_STM ~ -1 + Beijing_2011_18$temp_STM  *Beijing_2011_18$year +
                       Beijing_2011_18$prec_STM *Beijing_2011_18$year + Beijing_2011_18$rh_STM *Beijing_2011_18$year +
                       Beijing_2011_18$wind_STM * Beijing_2011_18$year + Beijing_2011_18$pbl_STM*Beijing_2011_18$year)
summary(lm_PM2.5_year) 
plot(lm_PM2.5_year)


#same model in crisp way
lm_comb <- lm(PM_STM ~ -1 + temp_STM * year +
      prec_STM *year + rh_STM *year +
      wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb)
plot(lm_comb)

#extract coefficients
coef_lm_comb <- coef(lm_comb)
#convert it into data frame
coef_lm_comb <- data.frame(as.list(coef_lm_comb))


#crisp model for STM
lm_comb_2 <- lm(PM_STM ~ -1 + temp_STM +
                 prec_STM + rh_STM +
                 wind_STM + pbl_STM , data = Beijing_2011_18)
summary(lm_comb_2)
plot(lm_comb_2)

#Generalized Linear Model
glm_comb_2 <- glm(PM_STM ~ -1 + temp_STM +
                   prec_STM + rh_STM +
                   wind_STM + pbl_STM , data = Beijing_2011_18, family = "gaussian")
summary(glm_comb_2)
#Gaussian family gives same result as linear family
#model did not run for other families


#For TCO
lm_TCO <- lm(TCO_STM ~ -1 + temp_STM +
                prec_STM + rh_STM +
                wind_STM + pbl_STM , data = Beijing_2011_18 )
summary(lm_TCO) 
plot(lm_TCO)


#For Ozone
lm_Ozone <- lm(Ozone_STM ~ -1 + temp_STM +
                prec_STM + rh_STM +
                wind_STM + pbl_STM , data = Beijing_2011_18 )
summary(lm_Ozone) 


#Recreate yearly contributions
Beijing_2012 <- subset.data.frame(Beijing_2011_18, year == "12")
PM_temp_12 <- (coef_lm_comb$temp_STM * Beijing_2012$TAVG) + (coef_lm_comb$temp_STM.year12 * Beijing_2012$temp_STM)
PM_RH_12 <- (coef_lm_comb$rh_STM * Beijing_2012$RH) + (coef_lm_comb$year12.rh_STM * Beijing_2012$rh_STM)
PM_wind_12 <- (coef_lm_comb$wind_STM * Beijing_2012$wind) + (coef_lm_comb$year12.wind_STM * Beijing_2012$wind_STM)
PM_prec_12 <- (coef_lm_comb$prec_STM * Beijing_2012$PRCP) + (coef_lm_comb$year12.prec_STM * Beijing_2012$prec_STM)
PM_pbl_12 <- (coef_lm_comb$pbl_STM * Beijing_2012$PBL) + (coef_lm_comb$year12.pbl_STM * Beijing_2012$pbl_STM)
PM_12 <- coef_lm_comb$year12 + PM_temp_12 + PM_RH_12 + PM_wind_12 + PM_prec_12 + PM_pbl_12

Beijing_2013 <- subset.data.frame(Beijing_2011_18, year == "13")
PM_temp_13 <- (coef_lm_comb$temp_STM * Beijing_2013$TAVG) + (coef_lm_comb$temp_STM.year13 * Beijing_2013$temp_STM)
PM_RH_13 <- (coef_lm_comb$rh_STM * Beijing_2013$RH) + (coef_lm_comb$year13.rh_STM * Beijing_2013$rh_STM)
PM_wind_13 <- (coef_lm_comb$wind_STM * Beijing_2013$wind) + (coef_lm_comb$year13.wind_STM * Beijing_2013$wind_STM)
PM_prec_13 <- (coef_lm_comb$prec_STM * Beijing_2013$PRCP) + (coef_lm_comb$year13.prec_STM * Beijing_2013$prec_STM)
PM_pbl_13 <- (coef_lm_comb$pbl_STM * Beijing_2013$PBL) + (coef_lm_comb$year13.pbl_STM * Beijing_2013$pbl_STM)
PM_13 <- coef_lm_comb$year13 + PM_temp_13 + PM_RH_13 + PM_wind_13 + PM_prec_13 + PM_pbl_13

Beijing_2014 <- subset.data.frame(Beijing_2011_18, year == "14")
PM_temp_14 <- (coef_lm_comb$temp_STM * Beijing_2014$TAVG) + (coef_lm_comb$temp_STM.year14 * Beijing_2014$temp_STM)
PM_RH_14 <- (coef_lm_comb$rh_STM * Beijing_2014$RH) + (coef_lm_comb$year14.rh_STM * Beijing_2014$rh_STM)
PM_wind_14 <- (coef_lm_comb$wind_STM * Beijing_2014$wind) + (coef_lm_comb$year14.wind_STM * Beijing_2014$wind_STM)
PM_prec_14 <- (coef_lm_comb$prec_STM * Beijing_2014$PRCP) + (coef_lm_comb$year14.prec_STM * Beijing_2014$prec_STM)
PM_pbl_14 <- (coef_lm_comb$pbl_STM * Beijing_2014$PBL) + (coef_lm_comb$year14.pbl_STM * Beijing_2014$pbl_STM)
PM_14 <- coef_lm_comb$year14 + PM_temp_14 + PM_RH_14 + PM_wind_14 + PM_prec_14 + PM_pbl_14

Beijing_2015 <- subset.data.frame(Beijing_2011_18, year == "15")
PM_temp_15 <- (coef_lm_comb$temp_STM * Beijing_2015$TAVG) + (coef_lm_comb$temp_STM.year15 * Beijing_2015$temp_STM)
PM_RH_15 <- (coef_lm_comb$rh_STM * Beijing_2015$RH) + (coef_lm_comb$year15.rh_STM * Beijing_2015$rh_STM)
PM_wind_15 <- (coef_lm_comb$wind_STM * Beijing_2015$wind) + (coef_lm_comb$year15.wind_STM * Beijing_2015$wind_STM)
PM_prec_15 <- (coef_lm_comb$prec_STM * Beijing_2015$PRCP) + (coef_lm_comb$year15.prec_STM * Beijing_2015$prec_STM)
PM_pbl_15 <- (coef_lm_comb$pbl_STM * Beijing_2015$PBL) + (coef_lm_comb$year15.pbl_STM * Beijing_2015$pbl_STM)
PM_15 <- coef_lm_comb$year15 + PM_temp_15 + PM_RH_15 + PM_wind_15 + PM_prec_15 + PM_pbl_15

Beijing_2016 <- subset.data.frame(Beijing_2011_18, year == "16")
PM_temp_16 <- (coef_lm_comb$temp_STM * Beijing_2016$TAVG) + (coef_lm_comb$temp_STM.year16 * Beijing_2016$temp_STM)
PM_RH_16 <- (coef_lm_comb$rh_STM * Beijing_2016$RH) + (coef_lm_comb$year16.rh_STM * Beijing_2016$rh_STM)
PM_wind_16 <- (coef_lm_comb$wind_STM * Beijing_2016$wind) + (coef_lm_comb$year16.wind_STM * Beijing_2016$wind_STM)
PM_prec_16 <- (coef_lm_comb$prec_STM * Beijing_2016$PRCP) + (coef_lm_comb$year16.prec_STM * Beijing_2016$prec_STM)
PM_pbl_16 <- (coef_lm_comb$pbl_STM * Beijing_2016$PBL) + (coef_lm_comb$year16.pbl_STM * Beijing_2016$pbl_STM)
PM_16 <- coef_lm_comb$year16 + PM_temp_16 + PM_RH_16 + PM_wind_16 + PM_prec_16 + PM_pbl_16

Beijing_2017 <- subset.data.frame(Beijing_2011_18, year == "17")
PM_temp_17 <- (coef_lm_comb$temp_STM * Beijing_2017$TAVG) + (coef_lm_comb$temp_STM.year17 * Beijing_2017$temp_STM)
PM_RH_17 <- (coef_lm_comb$rh_STM * Beijing_2017$RH) + (coef_lm_comb$year17.rh_STM * Beijing_2017$rh_STM)
PM_wind_17 <- (coef_lm_comb$wind_STM * Beijing_2017$wind) + (coef_lm_comb$year17.wind_STM * Beijing_2017$wind_STM)
PM_prec_17 <- (coef_lm_comb$prec_STM * Beijing_2017$PRCP) + (coef_lm_comb$year17.prec_STM * Beijing_2017$prec_STM)
PM_pbl_17 <- (coef_lm_comb$pbl_STM * Beijing_2017$PBL) + (coef_lm_comb$year17.pbl_STM * Beijing_2017$pbl_STM)
PM_17 <- coef_lm_comb$year17 + PM_temp_17 + PM_RH_17 + PM_wind_17 + PM_prec_17 + PM_pbl_17

Beijing_2018 <- subset.data.frame(Beijing_2011_18, year == "18")
PM_temp_18 <- (coef_lm_comb$temp_STM * Beijing_2018$TAVG) + (coef_lm_comb$temp_STM.year18 * Beijing_2018$temp_STM)
PM_RH_18 <- (coef_lm_comb$rh_STM * Beijing_2018$RH) + (coef_lm_comb$year18.rh_STM * Beijing_2018$rh_STM)
PM_wind_18 <- (coef_lm_comb$wind_STM * Beijing_2018$wind) + (coef_lm_comb$year18.wind_STM * Beijing_2018$wind_STM)
PM_prec_18 <- (coef_lm_comb$prec_STM * Beijing_2018$PRCP) + (coef_lm_comb$year18.prec_STM * Beijing_2018$prec_STM)
PM_pbl_18 <- (coef_lm_comb$pbl_STM * Beijing_2018$PBL) + (coef_lm_comb$year18.pbl_STM * Beijing_2018$pbl_STM)
PM_18 <- coef_lm_comb$year18 + PM_temp_18 + PM_RH_18 + PM_wind_18 + PM_prec_18 + PM_pbl_18





#plot yearly contributions
library(tidyverse)
library(reshape)
library(ggplot2)
Beijing_plot <- subset(Beijing_2011_18, select = c(PM_STM, Ozone_STM, prec_STM, temp_STM, 
                                                   rh_STM, wind_STM, TCO_STM, pbl_STM,
                                                   year, date))
Beijing_plot.melt <- melt( Beijing_plot, id.vars = c( 'year', 'date'))
Beijing_plot.melt$date <- as.Date( Beijing_plot.melt$date)

# Beijing_plot.melt$variable = factor(Beijing_plot.melt$variable, 
#             levels = c('year','PM_STM', 'temp_STM', 'rh_STM',
#                                           'prec_STM', 'wind_STM','pbl_STM'))
summary(Beijing_plot.melt)


theme_set(theme_bw())
                              
ggplot( Beijing_plot.melt) + (aes( x = year, y = value,
                                                group = variable, color = variable)) +
   geom_line() + 
   facet_wrap( . ~ variable)
   #scale_color_manual(values = c("black", "blue","red", "green", "yellow", violet", "brown" ))
   
ggplot( Beijing_plot.melt) + (aes( x = date, y = value,
                                   group = variable, color = variable)) +
   geom_line()
   






#recreated plots
plot(PM_12, type = 'l',
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_12, type = 'l',
     pch = 18, col = "orange", las = 1)
lines(PM_RH_12, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_12, type = 'l',
     pch = 18, col = "gray", las = 1)
lines(PM_prec_12, type = 'l',
     pch = 18, col = "purple", las = 1)
lines(PM_pbl_12, type = 'l',
      pch = 18, col = "yellow", las = 1)

par(mfrow = c(2, 2))

plot(PM_13, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_13, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_13, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_13, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_13, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_13, type = 'l',
      pch = 18, col = "yellow", las = 1)

legend(x = "topright",
       inset = c(0, 0.1),
       legend=c("PM2.5", "Temp", "RH", "Wind", "Precipitation", "PBL" ),
       col=c("darkslateblue", "orange", "limegreen", "gray", "purple", "yellow"), 
       lty=1:2, cex=0.2)


plot(PM_14, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_14, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_14, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_14, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_14, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_14, type = 'l',
      pch = 18, col = "yellow", las = 1)


plot(PM_15, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_15, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_15, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_15, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_15, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_15, type = 'l',
      pch = 18, col = "yellow", las = 1)

plot(PM_16, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_16, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_16, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_16, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_16, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_16, type = 'l',
      pch = 18, col = "yellow", las = 1)


plot(PM_17, type = 'l',
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_17, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_17, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_17, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_17, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_17, type = 'l',
      pch = 18, col = "yellow", las = 1)

plot(PM_18, type = 'l',
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_18, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_18, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_18, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_18, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_18, type = 'l',
      pch = 18, col = "yellow", las = 1)

#Comparing 2012 vs 2018
par(mfrow=c(3,1))

plot(PM_12, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_12, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_12, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_12, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_12, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_12, type = 'l',
      pch = 18, col = "yellow", las = 1)

#add legend
legend(x = "bottomright",
       inset = c(0, -0.2),
       xpd = TRUE,
       horiz = TRUE,
       legend=c("PM2.5", "Temp", "RH", "Wind", "Precipitation", "PBL" ),
       col=c("darkslateblue", "orange", "limegreen", "gray", "purple", "yellow"), 
       lty=1:2, cex=0.5)

plot(PM_15, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_15, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_15, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_15, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_15, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_15, type = 'l',
      pch = 18, col = "yellow", las = 1)

#add legend
legend(x = "bottomright",
       inset = c(0, -0.5),
       xpd = TRUE,
       horiz = TRUE,
       legend=c("PM2.5", "Temp", "RH", "Wind", "Precipitation", "PBL" ),
       col=c("darkslateblue", "orange", "limegreen", "gray", "purple", "yellow"), 
       lty=1:2, cex=0.5)


plot(PM_18, type = 'l', xlab = "",
     pch = 18, col = "darkslateblue", las = 1,
     ylim=c(-0.1, 0.1))
lines(PM_temp_18, type = 'l',
      pch = 18, col = "orange", las = 1)
lines(PM_RH_18, type = 'l',
      pch = 18, col = "limegreen", las = 1)
lines(PM_wind_18, type = 'l',
      pch = 18, col = "gray", las = 1)
lines(PM_prec_18, type = 'l',
      pch = 18, col = "purple", las = 1)
lines(PM_pbl_18, type = 'l',
      pch = 18, col = "yellow", las = 1)


#add legend
legend(x = "bottomright",
       inset = c(0, -0.25),
       xpd = TRUE,
       horiz = TRUE,
       legend=c("PM2.5", "Temp", "RH", "Wind", "Precipitation", "PBL" ),
       col=c("darkslateblue", "orange", "limegreen", "gray", "purple", "yellow"), 
       lty=1:2, cex=0.75)







#checking for year 2016
plot(x = Beijing_2011_18$date, y = Beijing_2011_18$PBL, type = "l")


#GAM
gam1 <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$wind_STM, sp = 13) + s(Beijing_2011_18$prec_STM, sp = 9) + 
                    s(Beijing_2011_18$rh_STM, sp = 5) + Beijing_2011_18$temp_STM, method = "REML" )
summary(gam1)

#plot model
plot(gam1, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#show residuals
plot(gam1, rug = TRUE, pages = 1, all.terms = TRUE, 
     residuals = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#check model
gam.check(gam1)
concurvity(gam1, full = TRUE)
concurvity(gam1, full = FALSE)

#GAM with smoothing temperature
gam1a <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$wind_STM) + s(Beijing_2011_18$prec_STM) + 
                     s(Beijing_2011_18$rh_STM) + s(Beijing_2011_18$temp_STM), method = "REML" )
summary(gam1a)

#plot model
plot(gam1a, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#show residuals
plot(gam1a, rug = TRUE, pages = 1, all.terms = TRUE, 
     residuals = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#check model
gam.check(gam1a)
concurvity(gam1a, full = TRUE)
concurvity(gam1a, full = FALSE)

#anova test to compare gam1 and gam1a
anova.gam(gam1, gam1a, test = "F")

#GAM for each year as factor
gam2 <- gam(Beijing_2011_18$PM_STM ~ -1 + s(Beijing_2011_18$wind_STM *Beijing_2011_18$year) + 
                    s(Beijing_2011_18$prec_STM *Beijing_2011_18$year) + 
                    s(Beijing_2011_18$rh_STM *Beijing_2011_18$year) + 
                    (Beijing_2011_18$temp_STM *Beijing_2011_18$year), method = "REML")
summary(gam2)

#GAM for day of week
gam3 <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$PM_STM, by = Beijing_2011_18$days))
summary(gam3)

#plot model
plot(gam3, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#show residuals
plot(gam3, rug = TRUE, pages = 1, all.terms = TRUE, 
     residuals = TRUE, shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)

#check model
gam.check(gam3)

#GAM for 2D smooths, mixing interactions
gam4 <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM, Beijing_2011_18$wind_STM) + 
               Beijing_2011_18$temp_STM, method = "REML")
summary(gam4)

#plot model
plot(gam4, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 0.45 , cey = 0.3)

#check model
gam.check(gam4)
concurvity(gam4, full = TRUE)
concurvity(gam4, full = FALSE)

#GAM mixing interactions, smoothing temperature
gam4a <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM, 
                                        Beijing_2011_18$wind_STM, Beijing_2011_18$temp_STM),
             method = "REML")
summary(gam4a)
#p-value too high


#Mixing interactions, smoothing temperature separately
gam4b <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM, 
                                        Beijing_2011_18$wind_STM) + s(Beijing_2011_18$temp_STM),
             method = "REML")
summary(gam4b)
#p-value same as gam4

#anova test to compare gam4 and gam4b
anova.gam(gam4, gam4b, test = "F")
boxplot(gam4$residuals, gam4b$residuals, names = c("gam4", "gam4b"))


#GAM to build 2D surface
#given a form of quadratic equation
#(not sure about the formation)
gam4c <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$rh_STM) + s(Beijing_2011_18$prec_STM) + 
                s(Beijing_2011_18$temp_STM) + s(Beijing_2011_18$wind_STM) + 
                s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM) +
                s(Beijing_2011_18$prec_STM, Beijing_2011_18$wind_STM) +
                s(Beijing_2011_18$wind_STM, Beijing_2011_18$temp_STM) + 
                s(Beijing_2011_18$temp_STM, Beijing_2011_18$rh_STM) + 
                s(Beijing_2011_18$rh_STM,Beijing_2011_18$wind_STM) +
                s(Beijing_2011_18$prec_STM, Beijing_2011_18$temp_STM) +
                s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM,Beijing_2011_18$wind_STM)+
                s(Beijing_2011_18$prec_STM, Beijing_2011_18$wind_STM, Beijing_2011_18$temp_STM)+
                s(Beijing_2011_18$wind_STM, Beijing_2011_18$temp_STM,Beijing_2011_18$rh_STM) +
                s(Beijing_2011_18$temp_STM, Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM) +
                s(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM,Beijing_2011_18$wind_STM, Beijing_2011_18$temp_STM), 
             method = "REML")
summary(gam4c)
#summary takes too long for R to run
#only the basis function having wind, rh and prec has p-value = 0.00715, rest have higher p-values
#results similar to gam4

#GAM using tensor smooths
gam5 <- gam(Beijing_2011_18$PM_STM ~ te(Beijing_2011_18$rh_STM, Beijing_2011_18$prec_STM, Beijing_2011_18$wind_STM) + 
               Beijing_2011_18$temp_STM, method = "REML")
summary(gam5)
plot(gam5)

#GAM for tensor interaction
gamtitemp <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$temp_STM) + 
                    ti(Beijing_2011_18$PM_STM, Beijing_2011_18$temp_STM))
summary(gamtitemp)
plot(gamtitemp)

#GAM with cubic cyclic splines
gamccstm <- gam(Beijing_2011_18$PM_STM ~ s(Beijing_2011_18$month, bs = "cc", k = 4))
summary(gamccstm)
plot(gamccstm, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)



###GAMs recreated from papers###

##GAM_Beijing without optimized inputs##
##From Hua & Zhang et al (2021)##

#*****Important considerations*****
#Here, offset for log transform = 1, compared to 3 in study #
#For temporal categorical values, here days = 1, 2,.., 7 
#compared to weekdays = 1, weekends = 2, holidays = 3 to 5
#There is no hourly factor here
#Meteorological components with highest fitting performance for the PARTICULAR study included in the model
#All meteorological components = T2M, RH, Precipitation, Wind, BLH, SP, Dew Point Temperature

gam_bj <- gam(Beijing_2011_18$PM2.5_avg_conc ~ Beijing_2011_18$year + s(Beijing_2011_18$month) 
              + s(Beijing_2011_18$PBL) + s(Beijing_2011_18$wind))

summary(gam_bj)

#plot model
plot(gam_bj, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

#show residuals
plot(gam_bj, pages = 1, all.terms = TRUE,
     rug = TRUE, residuals = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


library(mgcViz)
b <- gam_bj
b <- getViz(b)
o <- plot(b, allTerms = T)
o <- o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
   l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
   l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
print(o, pages = 1)





##GAM_Wind_Precipitation##
## From Zhang, Jiao, Xu, Zhao, Tang, Zhou & Chen (2018) ##

#*****Important considerations*****
#Wind Direction not considered#
#No information about ?? in paper#
#Here, Jenks natural classification method not followed, only Boolean 1,0 followed for precipitation#
#cubic splines, non-linearly correlated used in the study; hence cubic regression splines used#

gam_wsprec <- gam(Beijing_2011_18$PM2.5_avg_conc ~ s(Beijing_2011_18$wind, bs = "cr") + 
            s(Beijing_2011_18$dayno, bs = "cr") +  Beijing_2011_18$PRCPBool, method = "REML")
summary(gam_wsprec)


#plot model
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#show residuals
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, residuals = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

#anova_test
anova.gam(gam_wsprec, test="chisq")
anova.gam(gam_bj, test="chisq")

#comparing the two recreated models
anova.gam(gam_bj, gam_wsprec, test = "F")
boxplot(gam_bj$residuals, gam_wsprec$residuals, names = c("gam_bj", "gam_wsprec"))


###***Holdout Analysis***###

#install necessary library functions
install.packages("caret")
install.packages("mlbench")
install.packages("ISLR")
library(caret)
library(mlbench)
library(ISLR)

##partition the data##
#create the partition in 4:1 ratio for training and testing data
set.seed(1)
in_train <- createDataPartition(Beijing_2011_18$PM2.5_avg_conc, p = 4/5, list = FALSE)

#name the data
training <- Beijing_2011_18[in_train, ]
testing <- Beijing_2011_18[-in_train, ]

#check the row numbers to ensure proper partitioning
nrow(Beijing_2011_18)
nrow(training)
nrow(testing)

##Training the data with GAM
#*Here, the GAM is GAM_WSPREC
#*GAM_WSPREC is a recreated model from another study, with minor changes
gam.wsprec = gam(PM2.5_avg_conc ~ s(wind, bs = "cr") + s(dayno, bs = "cr") +  PRCPBool, 
                 method = "REML", data = training)

#predicting
pred.wsprec = predict(gam.wsprec, newdata = testing)


##calculating predictive performance
install.packages("Metrics")
install.packages("tdr")
library(Metrics)
library(tdr)

#Mean squared error
mse_wsprec = mean((pred.wsprec - testing$PM2.5_avg_conc)^2)
c(mse_wsprec)
#value = 0.1517133

#calculating again with package
mse(testing$PM2.5_avg_conc, pred.wsprec)
#same value

#Root Mean Squared Error
rmse(testing$PM2.5_avg_conc, pred.wsprec)
#0.389504

#Mean Bias
bias(testing$PM2.5_avg_conc, pred.wsprec)
#0.01301326

#Mean Absolute Error
mae(testing$PM2.5_avg_conc, pred.wsprec)
#0.3044212

#Mean Absolute Percentage Error
mape(testing$PM2.5_avg_conc, pred.wsprec)
#Inf

#Normalized Mean Bias
tdStats(pred.wsprec, testing$PM2.5_avg_conc, functions = "nmbe")
#-0.004635958

#Normalized Mean Error
tdStats(pred.wsprec, testing$PM2.5_avg_conc, functions = "nmae")
#0.1084497



#*Training the data with GAM_BJ
#*GAM_BJ is a recreated model from another study, with minor changes
gam.bj = gam(PM2.5_avg_conc ~ year + s(month) + days + s(PBL) + s(wind), data = training)

#predicting
pred.bj = predict(gam.bj, newdata = testing)


##calculating predictive performance
#calculating MSE
mse_bj = mean((pred.bj - testing$PM2.5_avg_conc)^2)
c(mse_bj)
#value = 0.1136069

#Mean Squared Error
mse(testing$PM2.5_avg_conc, pred.bj)

#Root Mean Squared Error
rmse(testing$PM2.5_avg_conc, pred.bj)

#Mean Bias
bias(testing$PM2.5_avg_conc, pred.bj)

#Mean Absolute Error
mae(testing$PM2.5_avg_conc, pred.bj)

#Mean Absolute Percentage Error
mape(testing$PM2.5_avg_conc, pred.bj)

#Normalized Mean Bias
tdStats(pred.bj, testing$PM2.5_avg_conc, functions = "nmbe")

#Normalized Mean Error
tdStats(pred.bj, testing$PM2.5_avg_conc, functions = "nmae")


#plots for model performances
boxplot(pred.bj~testing$PM2.5_avg_conc)
boxplot(pred.wsprec~testing$PM2.5_avg_conc)





###k-fold cross validation###

#installing libraries
installed.packages("caret")
install.packages("dplyr")
library(caret)
library(dplyr)

set.seed(10)
#Define training control
train.control <- trainControl(method = "repeatedcv", number = 30, savePredictions=TRUE)

##partition the data##
#create the partition in 4:1 ratio for training and testing data
in_train <- createDataPartition(Beijing_2011_18$PM2.5_avg_conc, p = 4/5, list = FALSE)

#name the data
training <- Beijing_2011_18[in_train, ]
testing <- Beijing_2011_18[-in_train, ]

#check the row numbers to ensure proper partitioning
nrow(Beijing_2011_18)
nrow(training)
nrow(testing)

#train the model
gam.wsprec = gam(PM2.5_avg_conc ~ s(wind, bs = "cr") + s(dayno, bs = "cr") +  PRCPBool, 
                 method = "REML", data = training, trControl = train.control)

#predicting
pred.wsprec = predict(gam.wsprec, newdata = testing)

summary(pred.wsprec)

#Mean Square Error
mse(testing$PM2.5_avg_conc, pred.wsprec)
#*value is 0.1732102 compared to  0.1517133
#*cannot show individual runs 
#*hence cannot create histogram of outputs

#*Folds are created to visually represent each run in the k-fold
#*i.e., to un-fold the simulation
#Folds are created on the basis of target variable
folds <- createFolds(testing$PM2.5_avg_conc, k = 30, list = TRUE)
summary(folds$Fold01)



#Plots
#PM2.5 plots
par(mfrow = c(1,1))
plot(x = Beijing_2011_18$date, y = Beijing_2011_18$PM2.5_avg_conc,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$Ozone,
     xlab = "Time (years)", ylab = "Ozone Avg daily conc (ppb)",
     main = "Yearly concentration of Ozone over Beijing (2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkgreen", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$TAVG,
     xlab = "Time (years)", ylab = "Avg daily temperature (degree C)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "orange", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_PM_annual,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18) after applying long-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_PM_seasonal,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$PM_STM,
     xlab = "Time (years)", ylab = "PM2.5 Avg daily conc (ug/m3)",
     main = "Yearly concentration of PM2.5 over Beijing (2011-18) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)


#TCO plots
plot(x = Beijing_2011_18$date, y = Beijing_2011_18$TCO,
     xlab = "Time (years)", ylab = "TCO Avg daily conc (Dobson units)",
     main = "Yearly concentration of TCO over Beijing (2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_TCO_annual,
     xlab = "Time (years)", ylab = "TCO Avg daily conc (Dobson units)",
     main = "Yearly concentration of TCO over Beijing (2011-18) after applying long-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$kz_TCO_seasonal,
     xlab = "Time (years)", ylab = "TCO Avg daily conc (Dobson units)",
     main = "Yearly concentration of TCO over Beijing (2011-18)after applying seasonal KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumorchid4", las = 1)

plot(x = Beijing_2011_18$date, y = Beijing_2011_18$TCO_STM,
     xlab = "Time (years)", ylab = "TCO Avg daily conc (Dobson units)",
     main = "Yearly concentration of TCO over Beijing (2011-18) after applying short-term KZ filter",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50", type = 'l',
     pch = 18, col = "mediumpurple4", las = 1)




#correlation plots
plot(x = Beijing_2011_18$date , y = Beijing_2011_18$PM2.5_avg_conc,
     ylab = "PM2.5 Avg daily conc (ug/m3)",  xlab = "Time(years)",
     #main = "Correlation between concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50",
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Beijing_2011_18$kz_PM_seasonal, y = Beijing_2011_18$kz_temp_seasonal,
     xlab = "PM2.5 Avg daily conc (ug/m3)",  ylab = "Temperature(degree C)",
     main = "Correlation between seasonal concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50",
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Beijing_2011_18$PM_STM, y = Beijing_2011_18$temp_STM,
     xlab = "PM2.5 Avg daily conc (ug/m3)",  ylab = "Temperature(degree C)",
     main = "Correlation between short term concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50",
     pch = 18, col = "darkslateblue", las = 1)




