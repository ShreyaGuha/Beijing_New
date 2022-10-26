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


