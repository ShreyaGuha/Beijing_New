#ozone

##partition the data##
set.seed(1)

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


#start a vector to save the evaluation statistics
library(mgcv)

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

library(party)
library(randomForest)
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

#put all the results together
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

