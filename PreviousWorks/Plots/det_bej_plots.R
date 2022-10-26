#Plot(3)
#Result: Effect of meteorology on STM scale on PM2.5 concentration

Bej_data$year <- as.factor(format(Bej_data$Date, "%y"))

#build a subset with only the STM variables, detrended pollutant concemtration,
#the detrended met coefficients, & temporal variables
Beijing_plot_STM <- subset(Bej_data, select = c(PM_STM, ozone_STM,
                                                Temp_STM, Td_STM, 
                                                       Ws_STM, RH_STM, PBL_STM,
                                                       year, Date))

Beijing_plot_STM <- rename(Beijing_plot_STM, c("Average Concentration of PM2.5" = "PM_STM",
                                               "Average Concentration of Ozone" = "ozone_STM",
                                               "Temperature" = "Temp_STM",
                                               "Dew Point Temperature" = "Td_STM",
                                               "Wind Speed" = "Ws_STM",
                                               "Relative Humidity" = "RH_STM",
                                               "Height of PBL" = "PBL_STM"))

#data table
Beijing_plot_STM <- as.data.table(Beijing_plot_STM)

#use melt function to define the x-axis, against what you're plotting
Beijing_plot_STM.melt <- melt( Beijing_plot_STM, id.vars = c( 'year', 'Date'))
Beijing_plot_STM.melt$Date <- as.Date( Beijing_plot_STM.melt$Date)


#set background theme
theme_set(theme_bw())
#plot_STM
ggplot( Beijing_plot_STM.melt) + (aes( x = Date, y = value,
                                       group = variable)) +
  theme(axis.title = element_blank()) +
  geom_line(color="black") +
  labs(title=(expression(paste("Effect of meteorology on " , 
                               PM[2.5] ,
                               " concentration (ug/cc) and ozone (ppb) at short-term scale")))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15)) +
  #scale_x_continuous(breaks = unique(Beijing_plot_STM.melt$year)) +
  facet_wrap( . ~ variable, scales = "free")





















#from previous file
Beijing_2011_18 <- read.csv("Bej_data.stm.csv")

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








par(mfrow=c(2,1))
plot(x = Bej_daily$Date, y = Bej_daily$ozone, type="l",
     ylab = "Ozone Avg daily conc (ppb)",  xlab = "Time(years)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50",
     pch = 18, col = "darkslateblue", las = 1)


plot(x = Bej_data$Date, y = Bej_data$ozone_STM, type="l",
     ylab = "STM Ozone (ppb)",  xlab = "Time(years)",
     cex.main = "0.75", cex.lab = "0.65", cex.axis ="0.50",
     pch = 18, col = "darkslateblue", las = 1)


