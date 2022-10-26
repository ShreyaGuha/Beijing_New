library(mgcv)

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
library(party)
library(randomForest)
library(DALEX)

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

