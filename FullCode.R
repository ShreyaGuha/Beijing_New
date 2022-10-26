#####................................######
#####*****Beijing Detrending Code*****#####
#####................................######


#####................................######
###***Preparation***###
#####................................######

#Remove unnecessary files to create memory
rm( list = ls())

#install necessary library functions
library(raster)
library(lubridate)
library(data.table)
library(dplyr)
library(tools)
library(ggplot2)
library(data.table)
library(tidyverse)
library(tools)
library(ggplot2)
library(sp)
library(raster)
library(sf)
library(ggplot2)
library(kza)
library(fst)
library(carData)
library(car)
library(pryr)
library(corrplot)
library(reshape2)
library(vip)
library(mgcv)
library(party)
library(randomForest)
library(ggRandomForests)
library(lattice)
library(caret)
library(mlbench)
library(ISLR)
library(Rcpp)
library(vctrs)
library(Metrics)
library(tdr)
library(hydroGOF)

#####................................######



#####................................######
###***Preparing observed data***###
#####................................######

##Step 1: Preparing Hourly Observed Data for PM2.5##
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


##...................................##


##Step 2: Preparing Hourly Observed Data for Ozone##
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


##..........................................##


##Step 3: Merge the pollutant data##
#merge data
pol_daily <- merge(pm_final, oz_final, by = "Date")

#save data
fwrite(pol_daily, file = "daily_pm_ozone.csv")

#open saved data
pol_daily <- read.csv("daily_pm_ozone.csv")


##.....................................##


##Step 4: Preparing Hourly Observed Data##
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

#read saved data
Bej_tog <- fread(file = "Beijing_final_data.csv")

#####................................######




#####................................######
###***Preparing modelled data***###
#####................................######


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


#####................................######


#####................................######
###***Detrending Data***###
#####................................######


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

##Introducing Detrending by KZ filter##

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
write.fst( Bej_data_stm, "Bej.fst")


#####................................######





#####................................######
###***Building models***###
#####................................######



#setting specific directory
setwd("D:/Professional/Projects/Beijing")

#let's take a faster route here instead
Bej_data <- read.fst("Bej.fst")

object.size(Bej_data)
#450kb


##Step1: plot the data##
#subset
Bej_plota <- subset(Bej_data, select = c(Date, PM_STM, ozone_STM, Temp_STM, Td_STM, Ws_STM, 
                                         RH_STM, PBL_STM, Pres_STM, Albedo_STM, SR_STM, 
                                         SM_STM))

#first, melt the data
Bej_data.m <- melt(Bej_plota, id.vars="Date")

# Separate plots
ggplot(Bej_data.m, aes(Date,value)) + 
  geom_point() + 
  stat_smooth() +
  facet_wrap(~variable)



##................................##



##Step 2: correlation plots for checking collinearity##
#For PM2.5
Bej_plot_1 <- subset(Bej_data, select = c(PM_STM, Temp_STM, Td_STM, Ws_STM, RH_STM,
                                          PBL_STM, Pres_STM, Albedo_STM, SR_STM, 
                                          SM_STM))
Bej_plot_1n <- na.omit(Bej_plot_1)
Bej_cor_1 <- cor(Bej_plot_1n)
corrplot(Bej_cor_1, type="upper")


#For Ozone
Bej_plot_2 <- subset(Bej_data, select = c(ozone_STM, Temp_STM, Td_STM, Ws_STM, RH_STM,
                                          PBL_STM, Pres_STM, Albedo_STM, SR_STM, 
                                          SM_STM))
Bej_plot_2n <- na.omit(Bej_plot_2)
Bej_cor_2 <- cor(Bej_plot_2n)
corrplot(Bej_cor_2, type="upper")


#Check memory
memory.limit(size = 1200000000)


###................................###


##Step 3: Linear Model##
#For PM2.5
lm_full <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                SR_STM + SM_STM + as.factor(dayf),
              data = Bej_data)
summary(lm_full)
backward <- step(lm_full, direction = c("backward"), k=1)

#variable importance
vi_backward <- vi(backward)

#plot
ggplot(vi_backward, aes( x = Importance, y = Variable, fill = Sign)) +
  geom_col() 

#choosing backward as we are fine tuning some prior selection of variables
#and not on a fishing expedition
avPlots(lm_full, layout= c(4,4), col="Red", col.lines="green", pch=14, lwd=2)


#For Ozone
lm_full_2 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM +  Pres_STM + Albedo_STM +
                  SR_STM + SM_STM + as.factor(dayf),
                data = Bej_data)
summary(lm_full_2)
b2 <- step(lm_full_2, direction = c("backward"), k=1)

#variable imp
vi_b2 <- vi(b2)

#plot
ggplot(vi_b2, aes( x = Importance, y = Variable, fill = Sign)) +
  geom_col() 

#added variable plots
avPlots(lm_full_2, layout= c(4,4), col="Red", col.lines="green", pch=14, lwd=2)


###................................###


##Step 4: General Additive Model (GAM)##
#For PM2.5
gam_full <- gam(PM_STM ~ s(Temp_STM) + s(Td_STM) +
                  s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                  s(Albedo_STM) + s(SR_STM) + s(SM_STM),
                data = Bej_data)
summary(gam_full)

#variable importance
b<-bm_VariablesImportance(gam_full,Bej_data,method="full_rand",nb_rand=1)
plot(b)
names(b)
b_r <- data.table(names = rownames(b), values = b)
#plot
ggplot(b_r , aes( x = values.rand1, y = names)) +
  geom_col() +
  xlab('importance') +
  ylab("variables")


#partial plots
plot(gam_full, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)


#For Ozone
gam_full_2 <- gam(ozone_STM ~ -1 + s(Temp_STM) +
                    Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                    as.factor(dayf),
                  data = Bej_data)
summary(gam_full_2)

#variable importance
bx<-bm_VariablesImportance(gam_full_2,Bej_data,method="full_rand",nb_rand=1)
plot(bx)
names(bx)
b_rx <- data.table(names = rownames(bx), values = bx)
#plot
ggplot(b_rx , aes( x = values.rand1, y = names)) +
  geom_col() +
  xlab('importance') +
  ylab("variables")

#partial plots
plot(gam_full_2, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)



###................................###


##Step 5: Random Forest(RF)##
# For PM2.5
rf1 = randomForest(PM_STM ~ -1 + Temp_STM + Td_STM +
                                  Ws_STM + 
                                  as.factor(Wdfac) + as.factor(PRCPBool) + 
                                  RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                                  SR_STM + SM_STM,
                                as.factor(dayf),
                                data = Bej_daily, importance=TRUE, na.action = na.omit)

summary(rf1) #summary
print(rf1) #view forest results
plot(rf1)

#getting values for a particular tree
rf1_tree23 <- getTree(rf1, k=23, labelVar=TRUE)
plot(rf1_tree23)

#Variable Importance
plot(gg_vimp(rf1), relative=TRUE)

#no of trees= 1000
rf2 = randomForest(PM_STM ~ -1 + Temp_STM + Td_STM +
                     Ws_STM + 
                     as.factor(Wdfac) + as.factor(PRCPBool) + 
                     RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                     SR_STM + SM_STM,
                   as.factor(dayf), data = Bej_daily, importance=TRUE, 
                   na.action = na.omit, ntree=1000)

summary(rf2) #summary
print(rf2) #view forest results
plot(rf2)

#For ozone
rf1o = randomForest(ozone_STM ~ -1 + Temp_STM + Td_STM +
                     Ws_STM + 
                     as.factor(Wdfac) + as.factor(PRCPBool) + 
                     RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                     SR_STM + SM_STM,
                   as.factor(dayf),
                   data = Bej_daily, importance=TRUE, na.action = na.omit)

summary(rf1o) #summary
print(rf1o) #view forest results
plot(rf1o)

#Variable Importance
plot(gg_vimp(rf1o), relative=TRUE)


#####................................######




#####................................######
###***Holdout Analysis***###
#####................................######



#setting specific directory
setwd("D:/Professional/Projects/Beijing")

#read data
Bej_data <- read.fst("Bej.fst")

##partition the data##
set.seed(1)

#start a vector to save the evaluation statistics 
eval <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$PM_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  lm_full <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                  SR_STM + SM_STM + as.factor(dayf),
                data = training)
  
  #predicting
  pred.lm = predict(lm_full, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$PM_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$PM_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$PM_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "LM",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval <- 
    rbind( evaluation,
           eval)
  
}


#start a vector to save the evaluation statistic
eval_2 <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$PM_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  gam_full <- gam(PM_STM ~ -1 + s(Temp_STM) + s(Td_STM) +
                    as.factor(Wdfac) + as.factor(PRCPBool) + 
                    s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                    s(Albedo_STM) + s(SR_STM) + s(SM_STM) + 
                    as.factor(dayf), data = training)
  
  #predicting
  pred.lm = predict(gam_full, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$PM_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$PM_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$PM_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "GAM",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval_2 <- 
    rbind( evaluation,
           eval_2)
  
}


#start a vector to save the evaluation statistics 
eval_3 <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$PM_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  rf1 = randomForest(PM_STM ~ -1 + Temp_STM + Td_STM +
                       Ws_STM + 
                       #as.factor(Wdfac) + as.factor(PRCPBool) + 
                       RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                       SR_STM + SM_STM,
                     #as.factor(dayf),
                     data = training, importance=TRUE, na.action = na.omit)
  
  #predicting
  pred.lm = predict(rf1, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$PM_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$PM_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$PM_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$PM_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "RF",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval_3 <- 
    rbind( evaluation,
           eval_3)
  
}

#put all the results together
evalu <- rbind(eval, eval_2, eval_3)

#save data
fwrite(eval, file = "eval.csv")
fwrite(eval_2, file = "eval_2.csv")
fwrite(eval_3, file = "eval_3.csv")
fwrite(evalu, file = "evalu.csv")

#final evaluation plots
#read data
eval_final <- fread("eval_final.csv")
evalu <- fread("evalu.csv")

#plotting
eval_out.m <- 
  melt( eval_final,
        id.vars = c( 'model'))
#plot the evaluation
ggplot( eval_out.m,
        aes( x = model,
             y = value,
             color = model,
             group = variable)) +       
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ variable, ncol = 1,
              scales = 'free_y') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'right')

#version2
ggplot( eval_out.m,
        aes( x = variable,
             y = value,
             color = model,
             group = model)) +       
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ model, ncol = 1,
              scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none')


#histogram
eval_out <- 
  melt( evalu,
        id.vars = c( 'model', 'run'))

ggplot(eval_out, aes(x = value, fill = variable, color = model)) +
  geom_histogram(colour = "black") +
  scale_fill_brewer(palette = 'Greys') +
  facet_grid( model ~ variable, scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none')+
  theme(axis.text.x = element_text(face="bold", color="#993333", size=7),
        axis.text.y = element_text(face="bold", color="#993333", size=10)) +
  theme(strip.text = element_text(size = 14))


#add line for means
#create new data frame for mean

ggplot(eval_out, aes(x = value, fill = variable, color = model)) +
  geom_histogram() +
  geom_vline(xintercept = mean(x),        
             col = "red",
             lwd = 3) +
  scale_fill_brewer(palette = "Pastel2") +
  facet_grid( model ~ variable, scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none')



#####................................######
###***Repeat for Ozone***###
#####................................######


##partition the data##
set.seed(1)

##Step 1: LM##
#start a vector to save the evaluation statistics 
eval <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$ozone_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  lm_full <- lm(ozone_STM ~ Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                  SR_STM + SM_STM + as.factor(dayf),
                data = training)
  
  #predicting
  pred.lm = predict(lm_full, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$ozone_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$ozone_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "LM",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval <- 
    rbind( evaluation,
           eval)
  
}

##.............................##


##Step 2: GAM##
#start a vector to save the evaluation statistics
eval_2 <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$ozone_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  gam_full <- gam(ozone_STM ~  s(Temp_STM) + s(Td_STM) +
                    as.factor(Wdfac) + as.factor(PRCPBool) + 
                    s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                    s(Albedo_STM) + s(SR_STM) + s(SM_STM) + 
                    as.factor(dayf), data = training)
  
  #predicting
  pred.lm = predict(gam_full, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$ozone_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$ozone_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "GAM",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval_2 <- 
    rbind( evaluation,
           eval_2)
  
}

##.............................##


##Step 3: RF##
#start a vector to save the evaluation statistics 
eval_3 <- data.table()

for (i in 1:30) {
  print( i)
  
  #train the data
  in_train <- createDataPartition(!(is.na(Bej_data$ozone_STM)), p = 4/5, list = FALSE)
  
  #name the data
  training <- Bej_data[in_train, ]
  testing <- Bej_data[-in_train, ]
  
  #check the row numbers to ensure proper partitioning
  nrow(Bej_data)
  nrow(training)
  nrow(testing)
  
  
  ##Training the data with LM
  rf1 = randomForest(ozone_STM ~ Temp_STM + Td_STM +
                       Ws_STM + 
                       #as.factor(Wdfac) + as.factor(PRCPBool) + 
                       RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                       SR_STM + SM_STM,
                     #as.factor(dayf),
                     data = training, importance=TRUE, na.action = na.omit)
  
  #predicting
  pred.lm = predict(rf1, newdata = testing)
  pred.lm.num <- as.numeric(pred.lm)
  
  
  #evaluation statistics
  #name eval_stats
  
  #mean absolute error
  eval_mae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mae")
  
  #mean bias error
  eval_mbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "mbe")
  
  #normalized mean bias error
  eval_nmbe <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmbe")
  
  #normalized mean absolute error
  eval_nmae <- tdStats(pred.lm.num, testing$ozone_STM, functions = "nmae")
  
  #correlation
  eval_cor <- cor(pred.lm.num, testing$ozone_STM, use = "pairwise.complete.obs")
  
  #root mean square error
  eval_rmse <- tdStats(pred.lm.num, testing$ozone_STM, functions = "rmse")
  
  
  # create a data table of the results
  evaluation <- 
    data.table( run = i,
                model = "RF",
                mae = eval_mae,
                mbe = eval_mbe,
                nmbe = eval_nmbe,
                nmae = eval_nmae,
                r2 = eval_cor,
                rmse = eval_rmse )
  
  # rbind with existing data table
  eval_3 <- 
    rbind( evaluation,
           eval_3)
  
}


##.............................##


##Step 4: put all the results together##
evalu <- rbind(eval, eval_2, eval_3)

#save data
fwrite(eval, file = "eval_oz.csv")
fwrite(eval_2, file = "eval_2_oz.csv")
fwrite(eval_3, file = "eval_3_oz.csv")
fwrite(evalu, file = "evalu_oz.csv")

#final evaluation plots
#read data
eval_final <- fread("eval_final_oz.csv")
evalu <- fread("evalu_oz.csv")



##.............................##


##Step 5: plotting##
eval_out.m <- 
  melt( eval_final,
        id.vars = c( 'model'))
#plot the evaluation
ggplot( eval_out.m,
        aes( x = model,
             y = value,
             color = model,
             group = variable)) +       
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ variable, ncol = 1,
              scales = 'free_y') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'right')

#version2
ggplot( eval_out.m,
        aes( x = variable,
             y = value,
             color = model,
             group = model)) +       
  geom_hline( yintercept = 0) +
  geom_line() + 
  scale_color_brewer( palette = 'Dark2') +
  facet_wrap( . ~ model, ncol = 1,
              scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none')


#histogram
eval_out <- 
  melt( evalu,
        id.vars = c( 'model', 'run'))

ggplot(eval_out, aes(x = value, fill = variable, color = model)) +
  geom_histogram(colour = "black") +
  scale_fill_brewer( palette = 'Greys') +
  facet_grid( model ~ variable, scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none') +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=7),
        axis.text.y = element_text(face="bold", color="#993333", size=10)) +
  theme(strip.text = element_text(size = 14))


#add line for means
#create new data frame for mean

ggplot(eval_out, aes(x = value, fill = variable, color = model)) +
  geom_histogram() +
  geom_vline(xintercept = mean(x),        
             col = "red",
             lwd = 3) +
  scale_fill_brewer(palette = "Pastel2") +
  facet_grid( model ~ variable, scales = 'free_x') +
  theme_minimal() + 
  theme( axis.title = element_blank(),
         legend.background = element_rect( fill = 'white', color = NA),
         legend.position = 'none')


#lh plots
#train the data
#name the data
training <- Bej_data_3

testing <- copy( Bej_data_3)[, c("Temp_STM",
                                 "Td_STM", "Pres_STM", "Ws_STM",
                                 "Albedo_STM", "RH_STM", "PBL_STM",
                                 "SR_STM", "SM_STM") := 0]



test <- subset(testing, select =c("PM_STM", "ozone_STM", "Temp_STM",
                                  "Td_STM", "Pres_STM", "Ws_STM",
                                  "Albedo_STM", "RH_STM", "PBL_STM",
                                  "SR_STM", "SM_STM", "Date"))



#check the row numbers to ensure proper partitioning
nrow(Bej_data_3)
nrow(training)
nrow(testing)
summary(Bej_data_3)
summary(training)
summary(testing)


##Training the data with LM
summary( training)



# since we're not using any factor variables, we do want to keep the
# intercept in the model
lm_full <- lm(ozone_STM ~ Temp_STM + Td_STM +
                Ws_STM +
                RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                SR_STM + SM_STM,
              data = training)



summary( lm_full)



# which are not NA in train?
not_na_ozone <- which( !is.na( test$ozone_STM))



#predicting
pred.lm <- predict(lm_full, newdata = test)[not_na_ozone] + residuals( lm_full)
summary( residuals( lm_full))
summary( test$ozone_STM)
pred.lm.num <- as.numeric(pred.lm)
pred <- as.data.table(pred.lm.num)




#plot using ggplot2
# create dataframe with actual and predicted values
plot_data <- data.frame(STM_no_met = pred.lm.num,  
                        STM_from_met_lm = test$ozone_STM[not_na_ozone] - pred.lm.num,
                        Time = training$Date[not_na_ozone] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_lm, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



plot_data_det <- rename(plot_data, c( "Date" = "Time"))
Bej_plot_det <- cbind(Beijing_plot_det, plot_data_det, by = "Date")

#calculate detrended data
Bej_plot_det$det_oz <- Bej_plot_det$kz_ozone_annual + Bej_plot_det$kz_ozone_seasonal + 
  Bej_plot_det$STM_no_met

#new subset with only relevant values
Bej_det_oz <- subset(Bej_plot_det, select =c("Date", "ozone", "det_oz"))

#aggregate values by year
Bej_det_oz$year <- year(Bej_det_oz$Date)
Bej_det_oz$month <- month(Bej_det_oz$Date)
Bej_det_year_oz <- aggregate(Bej_det_oz$ozone, by = list(Bej_det_oz$year, Bej_det_oz$month), 
                             FUN=mean, na.rm=TRUE)
Bej_det_year_det_oz <- aggregate(Bej_det_oz$det_oz, by = list(Bej_det_oz$year, Bej_det_oz$month), 
                                 FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_oz)[1] <- 'year'
names(Bej_det_year_oz)[2] <- 'month'
names(Bej_det_year_oz)[3] <- 'ozone'
names(Bej_det_year_det_oz)[1] <- 'year'
names(Bej_det_year_det_oz)[2] <- 'month'
names(Bej_det_year_det_oz)[3] <- 'ozone_detrended'

#combine data
Bej_det_mean <- merge(Bej_det_year_oz, Bej_det_year_det_oz, 
                      by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean$difference <- Bej_det_mean$ozone - Bej_det_mean$ozone_detrended

#introduce date column
#take first day of each month
Bej_det_mean$day <- 1
Bej_det_mean$date <- as.Date(with(Bej_det_mean, paste(year, month, day, sep="-")),
                             "%Y-%m-%d") 


#plot the data
#plot the difference
ggplot() +
  geom_line(data = Bej_det_mean, aes(y = difference, x = date, color = "black")) +
  scale_color_manual(name = "Trend", values = c("Difference" = "black")) +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


#melt the data for better plotting
Bej_det_mean.m <- melt( Bej_det_mean,
                        id.vars = c('date', 'year', 'month', 'day'))


#plot the data
ggplot(Bej_det_mean.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line() +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


#presentation plot
ggplot(Bej_det_mean.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(face ="bold", size = 18)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab('') +
  ylab("ozone concentration(ppb)") +
  theme(legend.position = "none")



#plotting for gam and rf
gam_full <- gam(ozone_STM ~ s(Temp_STM) + s(Td_STM) +
                  s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                  s(Albedo_STM) + s(SR_STM) + s(SM_STM), data = training)

pred.gam <- predict(gam_full, newdata = test)[not_na_ozone] + residuals( gam_full)
pred.gam.num <- as.numeric(pred.gam)

#plot using ggplot2
# create dataframe with actual and predicted values
plot_data <- data.frame(STM_no_met = pred.gam.num,  
                        STM_from_met_gam = test$ozone_STM[not_na_ozone] - pred.gam.num,
                        Time = training$Date[not_na_ozone] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_gam, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



#read data
Bej_data_whole <- fread("Bej_data.stm.csv")

#subset data
Beijing_plot_det <- subset(Bej_data_whole, select =c("Date", "pm", "kz_PM_annual", "kz_PM_seasonal",
                                                     "ozone", "kz_ozone_annual", "kz_ozone_seasonal"))

plot_data_det_gam <- rename(plot_data, c( "Date" = "Time"))
Bej_plot_det_gam <- cbind(Beijing_plot_det, plot_data_det_gam, by = "Date")

#calculate detrended data
Bej_plot_det_gam$det_oz <- Bej_plot_det_gam$kz_ozone_annual + 
  Bej_plot_det_gam$kz_ozone_seasonal + 
  Bej_plot_det_gam$STM_no_met

#new subset with only relevant values
Bej_det_oz_gam <- subset(Bej_plot_det_gam, select =c("Date", "ozone", "det_oz"))

#aggregate values by year
Bej_det_oz_gam$year <- year(Bej_det_oz_gam$Date)
Bej_det_oz_gam$month <- month(Bej_det_oz_gam$Date)
Bej_det_year_oz_gam <- aggregate(Bej_det_oz_gam$ozone, by = list(Bej_det_oz_gam$year, Bej_det_oz_gam$month), 
                                 FUN=mean, na.rm=TRUE)
Bej_det_year_det_oz_gam <- aggregate(Bej_det_oz_gam$det_oz, by = list(Bej_det_oz_gam$year, Bej_det_oz_gam$month), 
                                     FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_oz_gam)[1] <- 'year'
names(Bej_det_year_oz_gam)[2] <- 'month'
names(Bej_det_year_oz_gam)[3] <- 'ozone'
names(Bej_det_year_det_oz_gam)[1] <- 'year'
names(Bej_det_year_det_oz_gam)[2] <- 'month'
names(Bej_det_year_det_oz_gam)[3] <- 'ozone_detrended'

#combine data
Bej_det_mean_gam <- merge(Bej_det_year_oz_gam, Bej_det_year_det_oz_gam, 
                          by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean_gam$difference <- Bej_det_mean_gam$ozone - Bej_det_mean_gam$ozone_detrended

#introduce date column
#take first day of each month
Bej_det_mean_gam$day <- 1
Bej_det_mean_gam$date <- as.Date(with(Bej_det_mean_gam, paste(year, month, day, sep="-")),
                                 "%Y-%m-%d") 

#plot the data
#plot the difference
ggplot() +
  geom_line(data = Bej_det_mean_gam, aes(y = difference, x = date, color = "black")) +
  scale_color_manual(name = "Trend", values = c("Difference" = "black")) +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


summary(Bej_det_mean_gam$difference)
summary(Bej_det_mean$difference)

#melt the data for better plotting
Bej_det_mean_gam.m <- melt( Bej_det_mean_gam,
                            id.vars = c('date', 'year', 'month', 'day'))


#plot the data
ggplot(Bej_det_mean_gam.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line() +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


#presentation plot
ggplot(Bej_det_mean_gam.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(face ="bold", size = 18)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab('') +
  ylab("ozone concentration(ppb)") +
  theme(legend.position = "none")



#random forest
##Training the data with rf
rf1 = randomForest(ozone_STM ~ Temp_STM + Td_STM +
                     Ws_STM + 
                     #as.factor(Wdfac) + as.factor(PRCPBool) + 
                     RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                     SR_STM + SM_STM,
                   #as.factor(dayf),
                   data = training, importance=TRUE,
                   na.action = na.omit)

summary( rf1)


#predicting again without residuals
pred.rf <- predict(rf1, newdata = test)[not_na_ozone] 
pred.rf.num <- as.numeric(pred.rf)

#predicted values of PM
summary( test$ozone_STM)
oz_rf <- as.numeric(test$ozone_STM)
oz_rf.t <- as.data.table(oz_rf)


#plot using ggplot2
# create dataframe with actual and predicted values
plot_data <- data.frame(STM_no_met = pred.rf.num,  
                        STM_from_met_rf = test$ozone_STM[not_na_ozone] - pred.rf.num,
                        Time = training$Date[not_na_ozone] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_rf, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



plot_data_det <- rename(plot_data, c( "Date" = "Time"))
Bej_plot_det_rf <- cbind(Beijing_plot_det, plot_data_det, by = "Date")


#calculate detrended data
Bej_plot_det_rf$det_oz <- Bej_plot_det_rf$kz_ozone_annual + 
  Bej_plot_det_rf$kz_ozone_seasonal + 
  Bej_plot_det_rf$STM_no_met

#new subset with only relevant values
Bej_det_oz_rf <- subset(Bej_plot_det_rf, select =c("Date", "ozone", "det_oz"))

#aggregate values by year
Bej_det_oz_rf$year <- year(Bej_det_oz_rf$Date)
Bej_det_oz_rf$month <- month(Bej_det_oz_rf$Date)
Bej_det_year_oz_rf <- aggregate(Bej_det_oz_rf$ozone, by = list(Bej_det_oz_rf$year, 
                                                               Bej_det_oz_rf$month), 
                                FUN=mean, na.rm=TRUE)
Bej_det_year_det_oz_rf <- aggregate(Bej_det_oz_rf$det_oz, 
                                    by = list(Bej_det_oz_rf$year, Bej_det_oz_rf$month), 
                                    FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_oz_rf)[1] <- 'year'
names(Bej_det_year_oz_rf)[2] <- 'month'
names(Bej_det_year_oz_rf)[3] <- 'ozone'
names(Bej_det_year_det_oz_rf)[1] <- 'year'
names(Bej_det_year_det_oz_rf)[2] <- 'month'
names(Bej_det_year_det_oz_rf)[3] <- 'ozone_detrended'

#combine data
Bej_det_mean_rf <- merge(Bej_det_year_oz_rf, Bej_det_year_det_oz_rf, 
                         by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean_rf$difference <- Bej_det_mean_rf$ozone - Bej_det_mean_rf$ozone_detrended

#introduce date column
#take first day of each month
Bej_det_mean_rf$day <- 1
Bej_det_mean_rf$date <- as.Date(with(Bej_det_mean_rf, paste(year, month, day, sep="-")),
                                "%Y-%m-%d") 

#plot the data
#plot the difference
ggplot() +
  geom_line(data = Bej_det_mean_rf, aes(y = difference, x = date, color = "black")) +
  scale_color_manual(name = "Trend", values = c("Difference" = "black")) +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


summary(Bej_det_mean_rf$difference)
summary(Bej_det_mean$difference)

#melt the data for better plotting
Bej_det_mean_rf.m <- melt( Bej_det_mean_rf,
                           id.vars = c('date', 'year', 'month', 'day'))


#plot the data
ggplot(Bej_det_mean_rf.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line() +
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab("ozone concentration(ppb)")


#presentation plot
ggplot(Bej_det_mean_rf.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title="Daily averaged Raw & Meteorologically detrended ozone concentration") +
  theme(plot.title = element_text(face ="bold", size = 18)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab('') +
  ylab("ozone concentration(ppb)") +
  theme(legend.position = "none")


fwrite(Bej_det_mean, file = "oz_lm.csv")
fwrite(Bej_det_mean_gam, file = "oz_gam.csv")
fwrite(Bej_det_mean_rf, file = "oz_rf.csv")



#####.....................................#####




#####.....................................#####
###***Repeat PLOTS for GAM and RF***###
#####.....................................#####


#plotting for gam and rf
gam_full <- gam(PM_STM ~ s(Temp_STM) + s(Td_STM) +
                  s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                  s(Albedo_STM) + s(SR_STM) + s(SM_STM), data = training)

pred.gam <- predict(gam_full, newdata = test)[not_na_pm] + residuals( gam_full)
summary( residuals( gam_full))
summary( test$PM_STM)
pred.gam.num <- as.numeric(pred.gam)
pred <- as.data.table(pred.gam.num)

#plot using ggplot2
# create dataframe with actual and predicted values
plot_data_2 <- data.frame(STM_no_met = pred.gam.num,  
                          STM_from_met_gam = test$PM_STM[not_na_pm] - pred.gam.num,
                          Time = training$Date[not_na_pm] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")


# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_gam, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")


#proper plot
plot_data_det_2 <- rename(plot_data_2, c( "Date" = "Time"))
Bej_plot_det_2 <- cbind(Beijing_plot_det, plot_data_det_2, by = "Date")


#calculate detrended data
Bej_plot_det_2$det_pm <- Bej_plot_det_2$kz_PM_annual + Bej_plot_det_2$kz_PM_seasonal + Bej_plot_det_2$STM_no_met

#new subset with only relevant values
Bej_det_pm_2 <- subset(Bej_plot_det_2, select =c("Date", "pm", "det_pm"))

#aggregate values by year
Bej_det_pm_2$year <- year(Bej_det_pm_2$Date)
Bej_det_pm_2$month <- month(Bej_det_pm_2$Date)
Bej_det_year_pm_2 <- aggregate(Bej_det_pm_2$pm, by = list(Bej_det_pm_2$year, 
                                                          Bej_det_pm_2$month), 
                               FUN=mean, na.rm=TRUE)
Bej_det_year_det_pm_2 <- aggregate(Bej_det_pm_2$det_pm, by = list(Bej_det_pm_2$year, 
                                                                  Bej_det_pm_2$month), 
                                   FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_pm_2)[1] <- 'year'
names(Bej_det_year_pm_2)[2] <- 'month'
names(Bej_det_year_pm_2)[3] <- 'pm'
names(Bej_det_year_det_pm_2)[1] <- 'year'
names(Bej_det_year_det_pm_2)[2] <- 'month'
names(Bej_det_year_det_pm_2)[3] <- 'pm_detrended'

#combine data
Bej_det_mean_2 <- merge(Bej_det_year_pm_2, Bej_det_year_det_pm_2, 
                        by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean_2$difference <- Bej_det_mean_2$pm - Bej_det_mean_2$pm_detrended

#introduce date column
#take first day of each month
Bej_det_mean_2$day <- 1
Bej_det_mean_2$date <- as.Date(with(Bej_det_mean, paste(year, month, day, sep="-")),
                               "%Y-%m-%d") 


#melt the data for better plotting
Bej_det_mean.m2 <- melt( Bej_det_mean_2,
                         id.vars = c('date', 'year', 'month', 'day'))


#presentation plot
p2 <- ggplot(Bej_det_mean.m2, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(face ="bold", size = 20)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab("")+
  ylab(expression(paste(PM[2.5] , " concentration, ug ", m^{'-3'})))

p2.n <- p2 + theme(legend.position = "none")





#random forest
##Training the data with rf
rf1 = randomForest(PM_STM ~ Temp_STM + Td_STM +
                     Ws_STM + 
                     #as.factor(Wdfac) + as.factor(PRCPBool) + 
                     RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                     SR_STM + SM_STM,
                   #as.factor(dayf),
                   data = training, importance=TRUE,
                   na.action = na.omit)

summary( rf1)


pred.rf <- predict(rf1, newdata = test)[not_na_pm] + residuals(rf1)

#residuals
summary( residuals( rf1))
#showing length = 0
#increasing no of trees to 1000, also shows residuals to be length = 0

#checking for residuals
explain_apart_rf <- explain(model = rf1, 
                            data    = test, 
                            y       = test$PM_STM, 
                            label   = "Random Forest")
mr_rf <- model_performance(explain_apart_rf)
#residuals are NA
#so we don't need residuals

#predicting again without residuals
pred.rf <- predict(rf1, newdata = test)[not_na_pm] 
pred.rf.num <- as.numeric(pred.rf)
pred <- as.data.table(pred.rf.num)

#predicted values of PM
summary( test$PM_STM)
pm_rf <- as.numeric(test$PM_STM)
pm_rf.t <- as.data.table(pm_rf)


#finding residuals the conventional way
residual <- pm_rf.t$pm_rf - pred$pred.rf.num
res <- as.numeric(residual)
res.t <- as.data.table(res)


#add residuals to pred.rf (prediction without residuals) for plotting
#but thoretically that should give us the predicted values as that's how we calculated
#residuals are not generated by rf model

pred1 <- pred$pred.rf.num + res.t$res
pred1.num <- as.numeric(pred1)
pred1.dt <- as.data.table(pred1.num)

#pred1.dt is essentially the same data table as pm_rf.t = test$PM_STM
#this is the predicted value of PM by the model
#we are plotting this

#plot using ggplot2
# create dataframe with actual and predicted values
plot_data_3 <- data.frame(STM_no_met = pred.rf.num,  
                          STM_from_met_rf = test$PM_STM[not_na_pm] - pred.rf.num,
                          Time = training$Date[not_na_pm] )



# plot predicted values and actual values
ggplot(plot_data, aes(y = STM_no_met, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")



# plot portion attributable to met
ggplot(plot_data, aes(y = STM_from_met_rf, x = Time)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "green")

#proper plot
plot_data_det_3 <- rename(plot_data_3, c( "Date" = "Time"))
Bej_plot_det_3 <- cbind(Beijing_plot_det, plot_data_det_3, by = "Date")


#calculate detrended data
Bej_plot_det_3$det_pm <- Bej_plot_det_3$kz_PM_annual + Bej_plot_det_3$kz_PM_seasonal + Bej_plot_det_3$STM_no_met

#new subset with only relevant values
Bej_det_pm_3 <- subset(Bej_plot_det_3, select =c("Date", "pm", "det_pm"))

#aggregate values by year
Bej_det_pm_3$year <- year(Bej_det_pm_3$Date)
Bej_det_pm_3$month <- month(Bej_det_pm_3$Date)
Bej_det_year_pm_3 <- aggregate(Bej_det_pm_3$pm, by = list(Bej_det_pm_3$year, 
                                                          Bej_det_pm_3$month), 
                               FUN=mean, na.rm=TRUE)
Bej_det_year_det_pm_3 <- aggregate(Bej_det_pm_3$det_pm, by = list(Bej_det_pm_3$year, 
                                                                  Bej_det_pm_3$month), 
                                   FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_pm_3)[1] <- 'year'
names(Bej_det_year_pm_3)[2] <- 'month'
names(Bej_det_year_pm_3)[3] <- 'pm'
names(Bej_det_year_det_pm_3)[1] <- 'year'
names(Bej_det_year_det_pm_3)[2] <- 'month'
names(Bej_det_year_det_pm_3)[3] <- 'pm_detrended'

#combine data
Bej_det_mean_3 <- merge(Bej_det_year_pm_3, Bej_det_year_det_pm_3, 
                        by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean_3$difference <- Bej_det_mean_3$pm - Bej_det_mean_3$pm_detrended

#introduce date column
#take first day of each month
Bej_det_mean_3$day <- 1
Bej_det_mean_3$date <- as.Date(with(Bej_det_mean_3, paste(year, month, day, sep="-")),
                               "%Y-%m-%d") 


#melt the data for better plotting
Bej_det_mean.m3 <- melt( Bej_det_mean_3,
                         id.vars = c('date', 'year', 'month', 'day'))


#presentation plot
p3 <- ggplot(Bej_det_mean.m3, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(face ="bold", size = 20)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab("")+
  ylab(expression(paste(PM[2.5] , " concentration, ug ", m^{'-3'})))

p3.n <- p3 + theme(legend.position = "none")




#FACET
names(Bej_det_mean.m)[6] <- 'LM'
names(Bej_det_mean.m2)[6] <- 'GAM'
names(Bej_det_mean.m3)[6] <- 'RF'

#combine data
Bej_det_mean_all <- merge(Bej_det_mean.m, Bej_det_mean.m2,
                          by = c("date", "year", "month", "day", "variable"))
names(Bej_det_mean_all)[6] <- 'LM'
names(Bej_det_mean_all)[7] <- 'GAM'

Bej_det_mean_all <- merge(Bej_det_mean_all, Bej_det_mean.m3,
                          by = c("date", "year", "month", "day", "variable"))

names(Bej_det_mean_all)[8] <- 'RF'

Bej_det_mean_all.m <-  melt( Bej_det_mean_all,
                             id.vars = c('date', 'year', 'month', 'day'))
Beijing_plot_2.melt$date <- as.Date( Beijing_plot_2.melt$date)




#####.....................................#####




#####.....................................#####
###***Plotting the methods: what is kz filter/detrending***###
#####.....................................#####


#build a subset with only the variables required
Beijing_plot_2 <- subset(Bej_data_whole, select = c(pm, kz_PM_annual,
                                                    kz_PM_seasonal, PM_STM, Date))


# create dataset with all dates
alldates <- data.table( Date = seq.Date( as.Date('2011-01-01'),
                                         max( Beijing_plot_2$Date),
                                         by = 'day'))

#PLOT
Beijing_plot_3 <-
  merge( alldates,
         Beijing_plot_2, by = 'Date', all = TRUE)
dim( Beijing_plot_3)
dim( Beijing_plot_2)



# assign year
Beijing_plot_3$year <- year(Beijing_plot_3$Date)




Beijing_plot_3 <- rename(Beijing_plot_3,
                         c("Raw Pollutant average concentration" = "pm",
                           "Long-term variation" ="kz_PM_annual",
                           "Seasonal variation" ="kz_PM_seasonal",
                           "Short-term variation"="PM_STM" ))



#use melt function to define the x-axis, against what you're plotting
Beijing_plot_2.melt <- melt( Beijing_plot_3, id.vars = c( 'year', 'Date'))
Beijing_plot_2.melt$Date <- as.Date( Beijing_plot_2.melt$Date)




#set background theme
theme_set(theme_bw())
#plot
gg_out <-
  ggplot( Beijing_plot_2.melt) +
  (aes( x = Date, y = value,
        group = variable)) +
  geom_hline( yintercept = 0, size = .25) +
  geom_line(color = "black") +
  ggtitle("Method: What is kz-filter?") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.title = element_text(face = "bold", size = 16)) +
  xlab('time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration, ug ", m^{'-3'}))) +
  facet_wrap( . ~ variable) +
  theme(axis.text = element_text( size = 16),
        axis.title = element_text( size = 20),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 14),
        strip.background = element_blank())


#SAVE
ggsave( '~/Desktop/kz_filter_definition.png',
        width = 10, height = 6)



#####.....................................#####




#####.....................................#####
###***Other plots***###
#####.....................................#####

#read data
Bej_data_whole <- fread("Bej_data.stm.csv")

#subset data
Beijing_plot_det <- subset(Bej_data_whole, select =c("Date", "pm", "kz_PM_annual", "kz_PM_seasonal",
                                                     "ozone", "kz_ozone_annual", "kz_ozone_seasonal"))

plot_data_det <- rename(plot_data, c( "Date" = "Time"))
Bej_plot_det <- cbind(Beijing_plot_det, plot_data_det, by = "Date")

#calculate detrended data
Bej_plot_det$det_pm <- Bej_plot_det$kz_PM_annual + Bej_plot_det$kz_PM_seasonal + Bej_plot_det$STM_no_met

#new subset with only relevant values
Bej_det_pm <- subset(Bej_plot_det, select =c("Date", "pm", "det_pm"))

#aggregate values by year
Bej_det_pm$year <- year(Bej_det_pm$Date)
Bej_det_pm$month <- month(Bej_det_pm$Date)
Bej_det_year_pm <- aggregate(Bej_det_pm$pm, by = list(Bej_det_pm$year, Bej_det_pm$month), 
                             FUN=mean, na.rm=TRUE)
Bej_det_year_det_pm <- aggregate(Bej_det_pm$det_pm, by = list(Bej_det_pm$year, Bej_det_pm$month), 
                                 FUN=mean, na.rm=TRUE)

#rename columns
#renaming columns
names(Bej_det_year_pm)[1] <- 'year'
names(Bej_det_year_pm)[2] <- 'month'
names(Bej_det_year_pm)[3] <- 'pm'
names(Bej_det_year_det_pm)[1] <- 'year'
names(Bej_det_year_det_pm)[2] <- 'month'
names(Bej_det_year_det_pm)[3] <- 'pm_detrended'

#combine data
Bej_det_mean <- merge(Bej_det_year_pm, Bej_det_year_det_pm, 
                      by = c("year", "month"))

#calculate difference between raw and observed
Bej_det_mean$difference <- Bej_det_mean$pm - Bej_det_mean$pm_detrended

#introduce date column
#take first day of each month
Bej_det_mean$day <- 1
Bej_det_mean$date <- as.Date(with(Bej_det_mean, paste(year, month, day, sep="-")),
                             "%Y-%m-%d") 

#save data
write.fst( Bej_det_mean, "Bej_det_plot.fst")

#plot the data
ggplot() +
  geom_line(data = Bej_det_mean, aes(y = pm, x = date, color = "yellow")) +
  geom_line(data = Bej_det_mean, aes(y = pm_detrended, x = date, color = "black")) +
  scale_color_manual(name = "Trend", values = c("Observed" = "yellow", 
                                                "Detrended" = "black")) +
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/cc)")))


#plot the difference
ggplot() +
  geom_line(data = Bej_det_mean, aes(y = difference, x = date, color = "black")) +
  scale_color_manual(name = "Trend", values = c("Difference" = "black")) +
  labs(title=(expression(paste("Difference between daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/m3)")))


#melt the data for better plotting
Bej_det_mean.m <- melt( Bej_det_mean,
                        id.vars = c('date', 'year', 'month', 'day'))


#plot the data
ggplot(Bej_det_mean.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line() +
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  scale_color_brewer(palette = "Dark2") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
  xlab('Time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/m3)")))


#presentation plot
p1 <- ggplot(Bej_det_mean.m, aes(x = date, y = value, fill = value, color = variable)) +
  geom_line(size = 1.2)+
  labs(title=(expression(paste("Daily averaged Raw & Meteorologically detrended " , 
                               PM[2.5] ,
                               " concentration")))) +
  theme(plot.title = element_text(face ="bold", size = 20)) +
  scale_color_brewer(palette = "Dark2", name = "Trend") +
  theme(axis.title = element_text(face ="bold", size = 16)) +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=18),
        axis.text.y = element_text(face="bold", color="#993333", size=18)) +
  theme(legend.title = element_text(face="bold", size=18)) +
  theme(legend.text = element_text(size=16)) +
  xlab("")+
  ylab(expression(paste(PM[2.5] , " concentration, ug ", m^{'-3'})))

p1.n <- p1 + theme(legend.position = "none")


#Poster plot (1)
#Plotting the methods: what is kz filter/detrending
#build a subset with only the variables required
Beijing_plot_2 <- subset(Bej_data_whole, select = c(pm, kz_PM_annual, 
                                                    kz_PM_seasonal, PM_STM, Date))

Beijing_plot_2$year <- year(Beijing_plot_2$Date)


Beijing_plot_2 <- rename(Beijing_plot_2, 
                         c("Pollutant average concentration" = "pm",
                           "Long-term concentration" ="kz_PM_annual",
                           "Seasonal concentration" ="kz_PM_seasonal",
                           "Short-term concentration"="PM_STM" ))

#use melt function to define the x-axis, against what you're plotting
Beijing_plot_2.melt <- melt( Beijing_plot_2, id.vars = c( 'year', 'Date'))
Beijing_plot_2.melt$Date <- as.Date( Beijing_plot_2.melt$Date)



#set background theme
theme_set(theme_bw())
#plot
ggplot( Beijing_plot_2.melt) + (aes( x = Date, y = value,
                                     group = variable)) +
  geom_line(color = "black") + 
  ggtitle("Method: What is kz-filter?") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.title = element_text(face = "bold", size = 16)) +
  xlab('time(years)') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/m3)"))) +
  facet_wrap( . ~ variable) +
  theme(strip.text = element_text(size = 14))


#LH diagnosis
Beijing_plot_2.melt.dt <- as.data.table(Beijing_plot_2.melt)
Bej_plot.m <- Beijing_plot_2.melt.dt[year %in% 2011 & month( Date) == 1 & variable == "Long-term concentration"]

ggplot( Bej_plot.m) + (aes( x = Date, y = value,
                            group = variable)) +
  geom_line(color = "black") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.title = element_text(face = "bold", size = 16)) +
  xlab('time') +
  ylab(expression(paste(PM[2.5] , " concentration(ug/m3)"))) +
  facet_wrap( . ~ variable) +
  theme(strip.text = element_text(size = 14))


#####.....................................#####




#####.....................................#####
###***Simple time plots***###
#####.....................................#####

plot(x = Bej_daily$Date , y =  Bej_daily$pm ,
     ylab = "PM2.5 Avg daily conc (ugm-3)",  xlab = "Time(days)",
     #main = "Correlation between concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.lab = "1", cex.axis ="0.75", type="l",
     pch = 18, col = "darkslateblue", las = 1)

plot(x = Bej_daily$Date , y =  Bej_daily$ozone ,
     ylab = "Ozone Avg daily conc (ppb)",  xlab = "Time(days)",
     #main = "Correlation between concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.lab = "1", cex.axis ="0.75", type="l",
     pch = 18, col = "darkslateblue", las = 1)


#####.....................................#####
#####.....................................#####

