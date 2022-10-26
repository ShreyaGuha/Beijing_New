library( data.table)
library( ggplot2)
library( dplyr)

#Plotting the methods: what is kz filter/detrending
#build a subset with only the variables required
Beijing_plot_2 <- subset(Bej_data_whole, select = c(pm, kz_PM_annual,
                                                    kz_PM_seasonal, PM_STM, Date))



# create dataset with all dates
alldates <- data.table( Date = seq.Date( as.Date('2011-01-01'),
                                         max( Beijing_plot_2$Date),
                                         by = 'day'))



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



ggsave( '~/Desktop/kz_filter_definition.png',
        width = 10, height = 6)



#plot pm2.5 conc
plot(x = Beijing_plot_2$Date , y =  Beijing_plot_2$pm ,
     ylab = "PM2.5 Avg daily conc (ugm-3)",  xlab = "Time(years)",
     #main = "Correlation between concentration of PM2.5 over Beijing and temperature(2011-18)",
     cex.lab = "1", cex.axis ="0.75", type="l",
     pch = 18, col = "darkslateblue", las = 1)