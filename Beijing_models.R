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
library(tdr)

#subseting to remove non-numeric column
Beijing_numeric <- subset(Beijing_2011_18, select = -c(X, date_new, date, year, month,
                                                       u10, v10, dayno)) 
##storing minimum and maximum values for un-scaling in future
meanvec <- sapply(Beijing_numeric,mean)
sdvec <- sapply(Beijing_numeric,sd)

##scaling data##
#define scaling function8
scaledf <- function(x) {
  return ((x - mean(x)) / (sd(x)))
}
#not intending to scale PM2.5 or ozone, hence excluded
#only scaling columns having meteorological variables
lapply(Beijing_2011_18[c(4,5,6,10,12)], scaledf)

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

plot(Beijing_2011_18$PM_res)

###Linear regression models for short-term data, detrended###
##Model 1##
##Simplest model##
#For PM2.5
lm_comb_1 <- lm(PM_res ~ -1 + temp_STM +
                  prec_STM + rh_STM +
                  wind_STM + pbl_STM , data = Beijing_2011_18)
summary(lm_comb_1)
#wind and pbl significant

#For Ozone
lm_comb_ozone_1 <- lm(ozone_res ~ -1 + temp_STM +
                        prec_STM + rh_STM +
                        wind_STM + pbl_STM , data = Beijing_2011_18)
summary(lm_comb_ozone_1)
#wind & pbl significant

#For Total Column Ozone, reanalysis data
lm_comb_tco_1 <- lm(TCO_res ~ -1 + temp_STM +
                  prec_STM + rh_STM +
                  wind_STM + pbl_STM , data = Beijing_2011_18)
summary(lm_comb_tco_1)
#wind, pbl and rh significant

##Model 2##
##Another model, temporal variable included, yearwise variation can be seen##
#For PM2.5
lm_comb_2 <- lm(PM_res ~ -1 + temp_STM * year +
                prec_STM *year + rh_STM *year +
                wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb_2)
#pbl and ALL years except 2015 significant, wind no longer shows significance

#For Ozone
lm_comb_ozone_2 <- lm(ozone_res ~ -1 + temp_STM * year +
                  prec_STM *year + rh_STM *year +
                  wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb_ozone_2)
#Some years seem significant, 
#pbl and wind show yearwise fluctuating significance
#this model more useful for ozone data

#For TCO
lm_comb_TCO_2 <- lm(TCO_res ~ -1 + temp_STM * year +
                        prec_STM *year + rh_STM *year +
                        wind_STM * year + pbl_STM*year, data = Beijing_2011_18)
summary(lm_comb_TCO_2)
#All years significant, 
#wind and pbl show significance

#For Model 2, we have not extracted coefficients (yet) for that step is required
#for plotting: to be done later

#Moving to slightly complex models
###Generalized Linear Models (GLMs)###
##For PM2.5
#check distribution of pollutant to determine family
hist(Beijing_2011_18$PM_res, breaks = 50)
#almost gaussian distribution
glm_PM_1 <- glm(PM_res ~ -1 + temp_STM +
                    prec_STM + rh_STM +
                    wind_STM + pbl_STM , data = Beijing_2011_18, family = "gaussian")
summary(glm_PM_1)
#wind and pbl significant, same result as linear model

##For Ozone
hist(Beijing_2011_18$ozone_res, breaks = 50)
#almost lognormal or poisson distribution
glm_ozone_1 <- glm(ozone_res ~ -1 + temp_STM +
                  prec_STM + rh_STM +
                  wind_STM + pbl_STM , data = Beijing_2011_18, family = poisson())
summary(glm_ozone_1)
#negative values of met variables at STM obtained from applying KZ-filter,
#not allowed by Poisson family

#this fit is hypothetical
glm_ozone_2 <- glm(ozone_res ~ -1 + temp_STM +
                     prec_STM + rh_STM +
                     wind_STM + pbl_STM , data = Beijing_2011_18, family = "gaussian")
summary(glm_ozone_2)
#wind and pbl show are significant

##For TCO
hist(Beijing_2011_18$TCO_res, breaks = 50)
#doubt regarding type of distribution, almost continuous uniform distribution
#this fit is hypothetical
glm_tco_1 <- glm(TCO_res ~ -1 + temp_STM +
                     prec_STM + rh_STM +
                     wind_STM + pbl_STM , data = Beijing_2011_18, family = "gaussian")
summary(glm_tco_1)
#wind, pbl and rh are significant
#resullts tally with linear regression models
##Results from LMs and GLMs seem to give consistent results


##A bit more complex model
###General Additive Models(GAMs)###
##Midway between LM and AI##
#calling library function
library(mgcv) 

#For PM2.5
gam1 <- gam(Beijing_2011_18$PM_res ~ s(Beijing_2011_18$wind_STM, sp = 3) + 
              s(Beijing_2011_18$prec_STM, sp = 3) + s(Beijing_2011_18$rh_STM, sp = 5) + 
              s(Beijing_2011_18$pbl_STM, sp = 5) + Beijing_2011_18$temp_STM, 
            method = "REML" )
summary(gam1)
#wind, pbl, rh significant
#Number of smoothing parameters for each of the meteorological variable 
#has been adjusted multiple times
#sp very high => lm, should be beware of overfitting data
#Reducing sp value led to better fitting of wind data
#Changing sp value had no change on temp data, hence it's not smoothened

#slightly complex GAM, accounting for each year as factor
#temperature not smoothened based on results from GAM1
gam2 <- gam(Beijing_2011_18$PM_res ~ -1 + 
              s(Beijing_2011_18$wind_STM *Beijing_2011_18$year, sp = 3) +
              s(Beijing_2011_18$pbl_STM * Beijing_2011_18$year) +
              s(Beijing_2011_18$prec_STM *Beijing_2011_18$year) + 
              s(Beijing_2011_18$rh_STM *Beijing_2011_18$year) + 
              (Beijing_2011_18$temp_STM *Beijing_2011_18$year), method = "REML")
summary(gam2)
#All years significant, pbl, rh, prec significant
#Wind not significant, even after specifying sp value, although p-value reduces for wind
#Temp as usual insignificant

##GAM accounting for day of week: weekdays/weekends variation##
#Assigning numbers 1-7 to the seven days of the week
Beijing_2011_18$days <- wday(Beijing_2011_18$date)
#This GAM will treat each day as a "factor"
gam3 <- gam(Beijing_2011_18$PM_res ~ s(Beijing_2011_18$PM_res, by = Beijing_2011_18$days))
summary(gam3)
#This factor seems significant
#Visualizing the results for clarity
#plot model
plot(gam3, rug = TRUE, pages = 1, all.terms = TRUE, 
     shade = TRUE, shade.col = "lightblue", 
     pch = 1, cex = 1)
#something is wrong with the plot
#need to recreate GAMs for ozone as well


###GAMs recreated from papers###
#**GAM from paper:1**#

##GAM_Beijing without optimized inputs##
##From Hua & Zhang et al (2021)##

#*****Important considerations*****
#For temporal categorical values, here days = 1, 2,.., 7 
#compared to weekdays = 1, weekends = 2, holidays = 3 to 5 in the paper
#There is no hourly factor here
#Meteorological components with highest fitting performance for the PARTICULAR study included in the model
#All meteorological components = T2M, RH, Precipitation, Wind, PBLH, SP, Dew Point Temperature

#Here, offset for log transform = 3#
Beijing_2011_18$PM2.5_avg_conc <- log10(Beijing_2011_18$PM2.5_avg_conc + 3)
#No Detrending involved

gam_bj <- gam(Beijing_2011_18$PM2.5_avg_conc ~ Beijing_2011_18$year + 
                s(Beijing_2011_18$month) + s(Beijing_2011_18$PBL) + s(Beijing_2011_18$wind))

summary(gam_bj)
#month, pbl, year significant
#wind not as significant

#Visualization
#plot model
plot(gam_bj, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

#Tweaking gam_bj with different sp values for fitting wind data not successful

##Recreating same model, but with detrended values this time
#Detrend and calculate residuals again with log-transformed data
Beijing_2011_18$kz_PM_annual <- kz(Beijing_2011_18$PM2.5_avg_conc, m = 365, k = 3)
Beijing_2011_18$PM_LT_rem <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$kz_PM_annual
Beijing_2011_18$kz_PM_seasonal <- kz(Beijing_2011_18$PM_LT_rem, m = 15, k = 5)
Beijing_2011_18$PM_STM <- Beijing_2011_18$PM_LT_rem - Beijing_2011_18$kz_PM_seasonal
Beijing_2011_18$PM_res <- Beijing_2011_18$PM2.5_avg_conc - Beijing_2011_18$PM_STM

#Detrended gam_bj
gam_bj_d <- gam(Beijing_2011_18$PM_res ~ Beijing_2011_18$year + 
                s(Beijing_2011_18$month) + s(Beijing_2011_18$PBL) + 
                  s(Beijing_2011_18$wind))

summary(gam_bj_d)
#Everything significant except wind, changing sp values does not bring about much change
#Result visualization
#plot model
plot(gam_bj_d, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#**GAM from paper:2**#
##GAM_Wind_Precipitation##
## From Zhang, Jiao, Xu, Zhao, Tang, Zhou & Chen (2018) ##

#*****Important considerations*****
#Wind Direction not considered#
#No information about ?? in paper#
#Here, Jenks natural classification method not followed

#only Boolean 1,0 followed for precipitation#
#changing PRCP to categorical variable
Beijing_2011_18$PRCPBool [Beijing_2011_18$PRCP != 0.0000] <- 1
Beijing_2011_18$PRCPBool [Beijing_2011_18$PRCP == 0.0000] <- 0
Beijing_2011_18$PRCPBool <- as.factor(Beijing_2011_18$PRCPBool)
#check number of rainy days
summary(Beijing_2011_18$PRCPBool)
#518 rainy days in 8 years, about 17.7% no of days
#quite low for a city which experiences monsoon climate
#possibilities of erroneous data or errors in data manipulation

##more about model
#cubic splines, non-linearly correlated used in the original study 
#hence cubic regression splines used here
#log-transformed PM concentration, offset value=3, like previous model(s)
gam_wsprec <- gam(Beijing_2011_18$PM2.5_avg_conc ~ s(Beijing_2011_18$wind, bs = "cr") + 
                    s(Beijing_2011_18$dayno, bs = "cr") +  Beijing_2011_18$PRCPBool, 
                  method = "REML")
summary(gam_wsprec)
#only dayno seems significant
#interpretation for intercept significance to be done
#plot model
plot(gam_wsprec, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

##Repeating same study with residual data##
gam_wsprec_d <- gam(Beijing_2011_18$PM_res ~ s(Beijing_2011_18$wind, bs = "cr") + 
                    s(Beijing_2011_18$dayno, bs = "cr") +  Beijing_2011_18$PRCPBool, 
                  method = "REML")
summary(gam_wsprec_d)
#prec and dayno are significant, along with intercept
#For wind, changing k-value or sp value does not add significance
#plot model
plot(gam_wsprec_d, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)



##To be done next
#More plots, if results are in the right direction
#Need to recreate similar models with different meteorological variables
#Recreating GAMs for ozone from papers to be done


