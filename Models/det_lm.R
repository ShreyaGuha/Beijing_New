rm( list = ls())

library(data.table)
library(tidyverse)
library(carData)
library(car)
library(pryr)
library(fst)
library(corrplot)
library(ggplot2)
library(reshape2)


#setting specific directory
setwd("D:/Professional/Projects/Beijing")

#let's take a faster route here instead
Bej_data <- read.fst("Bej.fst")

object.size(Bej_data)
#450kb


#plot the data
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


#correlation plots
Bej_plot_1 <- subset(Bej_data, select = c(PM_STM, Temp_STM, Td_STM, Ws_STM, RH_STM,
                                          PBL_STM, Pres_STM, Albedo_STM, SR_STM, 
                                          SM_STM))
Bej_plot_1n <- na.omit(Bej_plot_1)
Bej_cor_1 <- cor(Bej_plot_1n)
corrplot(Bej_cor_1, type="upper")

Bej_plot_2 <- subset(Bej_data, select = c(ozone_STM, Temp_STM, Td_STM, Ws_STM, RH_STM,
                                          PBL_STM, Pres_STM, Albedo_STM, SR_STM, 
                                          SM_STM))
Bej_plot_2n <- na.omit(Bej_plot_2)
Bej_cor_2 <- cor(Bej_plot_2n)
corrplot(Bej_cor_2, type="upper")

memory.limit(size = 1200000000)

lm_full <- lm(PM_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                RH_STM + PBL_STM + Pres_STM + Albedo_STM +
                SR_STM + SM_STM + as.factor(dayf),
              data = Bej_data)
summary(lm_full)
backward <- step(lm_full, direction = c("backward"), k=1)

#variable importance
install.packages("vip")
library(vip)
vi_backward <- vi(backward)

#plot
ggplot(vi_backward, aes( x = Importance, y = Variable, fill = Sign)) +
  geom_col() 

#choosing backward as we are fine tuning some prior selection of variables
#and not on a fishing expedition
avPlots(lm_full, layout= c(4,4), col="Red", col.lines="green", pch=14, lwd=2)


lm_full_2 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                RH_STM + PBL_STM +  Pres_STM + Albedo_STM +
                  SR_STM + SM_STM + as.factor(dayf),
              data = Bej_data)
summary(lm_full_2)
b2 <- step(lm_full_2, direction = c("backward"), k=1)

vi_b2 <- vi(b2)

#plot
ggplot(vi_b2, aes( x = Importance, y = Variable, fill = Sign)) +
  geom_col() 


avPlots(lm_full_2, layout= c(4,4), col="Red", col.lines="green", pch=14, lwd=2)


#GLM

glm_full <- glm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM +  Pres_STM + Albedo_STM +
                  SR_STM + SM_STM + as.factor(dayf),
                data = Bej_data, family = "gaussian")
summary(glm_full)


glm_full_2 <- glm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                    Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                    RH_STM + PBL_STM +  Pres_STM + Albedo_STM +
                    SR_STM + SM_STM + as.factor(dayf),
                  data = Bej_data, family = "gaussian")
summary(glm_full_2)


#GAM
library(mgcv)

gam_full <- gam(PM_STM ~ s(Temp_STM) + s(Td_STM) +
                              s(Ws_STM) + s(RH_STM) + s(PBL_STM) + s(Pres_STM) + 
                              s(Albedo_STM) + s(SR_STM) + s(SM_STM),
                data = Bej_data)
summary(gam_full)

b<-bm_VariablesImportance(gam_full,Bej_data,method="full_rand",nb_rand=1)
plot(b)
names(b)
b_r <- data.table(names = rownames(b), values = b)
#plot
ggplot(b_r , aes( x = values.rand1, y = names)) +
  geom_col() +
  xlab('importance') +
  ylab("variables")

plot(gam_full, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)

gam_full_2 <- gam(ozone_STM ~ -1 + s(Temp_STM) +
                    Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                    as.factor(dayf),
                  data = Bej_data)
summary(gam_full_2)


bx<-bm_VariablesImportance(gam_full_2,Bej_data,method="full_rand",nb_rand=1)
plot(bx)
names(bx)
b_rx <- data.table(names = rownames(bx), values = bx)
#plot
ggplot(b_rx , aes( x = values.rand1, y = names)) +
  geom_col() +
  xlab('importance') +
  ylab("variables")

plot(gam_full_2, pages = 1, all.terms = TRUE,
     rug = TRUE, shade = TRUE, shade.col = "lightgreen", 
     pch = 1, cex = 1)




###***Holdout Analysis***###

#install necessary library functions
library(lattice)
library(caret)
library(mlbench)
library(ISLR)
library(Rcpp)
library(vctrs)
library(Metrics)
library(tdr)
library(hydroGOF)

##partition the data##
#create the partition in 4:1 ratio for training and testing data
set.seed(1)

#start a vector to save the evaluation statistics 

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
                RH_STM + PBL_STM + as.factor(dayf),
              data = training)

lm_full_2 <- lm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM + as.factor(dayf),
                data = training)

#predicting
pred.lm = predict(lm_full, newdata = testing)
pred.lm.num <- as.numeric(pred.lm)

pred.lm.2 = predict(lm_full_2, newdata = testing)
pred.lm.num.2 <- as.numeric(pred.lm.2)


#evaluation statistics
#name eval_stats
gof(pred.lm.num, testing$PM_STM, na.rm = TRUE)
tdStats(pred.lm.num, testing$PM_STM, functions = "nmbe")
tdStats(pred.lm.num, testing$PM_STM, functions = "nmae")

gof(pred.lm.num.2, testing$ozone_STM, na.rm = TRUE)
tdStats(pred.lm.num.2, testing$ozone_STM, functions = "nmbe")
tdStats(pred.lm.num.2, testing$ozone_STM, functions = "nmae")



##Training the data with GLM
glm_full <- glm(PM_STM ~ -1 + Temp_STM + Td_STM +
                  Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                  RH_STM + PBL_STM + as.factor(dayf),
                data = training, family = "gaussian")

glm_full_2 <- glm(ozone_STM ~ -1 + Temp_STM + Td_STM +
                    Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                    RH_STM + PBL_STM + as.factor(dayf),
                  data = training, family = "gaussian")

#predicting
pred.glm = predict(glm_full, newdata = testing)
pred.glm.num <- as.numeric(pred.glm)

pred.glm.2 = predict(glm_full_2, newdata = testing)
pred.glm.num.2 <- as.numeric(pred.glm)

#evaluation statistics
gof(pred.glm.num, testing$PM_STM, na.rm = TRUE)
tdStats(pred.glm.num, testing$PM_STM, functions = "nmbe")
tdStats(pred.glm.num, testing$PM_STM, functions = "nmae")

gof(pred.glm.num.2, testing$ozone_STM, na.rm = TRUE)
tdStats(pred.glm.num.2, testing$ozone_STM, functions = "nmbe")
tdStats(pred.glm.num.2, testing$ozone_STM, functions = "nmae")


##Training the data with GAM
gam_full <- gam(PM_STM ~ -1 + s(Temp_STM) + s(Td_STM) +
                  as.factor(Wdfac) + as.factor(PRCPBool),
                data = training)

gam_full_2 <- gam(ozone_STM ~ -1 + s(Temp_STM) +
                    Ws_STM + as.factor(Wdfac) + as.factor(PRCPBool) + 
                    as.factor(dayf),
                  data = training)



#predicting
pred.gam = predict(gam_full, newdata = testing)
pred.gam.num <- as.numeric(pred.gam)

pred.gam_2 = predict(gam_full_2, newdata = testing)
pred.gam.num.2 <- as.numeric(pred.gam_2)


#evaluation statistics
gof(pred.gam.num, testing$PM_STM, na.rm = TRUE)

#Normalized Mean Bias
tdStats(pred.gam, testing$PM_STM, functions = "nmbe")
#0.01602949 

#Normalized Mean Error
tdStats(pred.gam, testing$PM_STM, functions = "nmae")
#0.0599894 


#For ozone
gof(pred.gam.num.2, testing$ozone_STM, na.rm = TRUE)

#Normalized Mean Bias
tdStats(pred.gam_2, testing$ozone_STM, functions = "nmbe")
#0.01593435 

#Normalized Mean Error
tdStats(pred.gam_2, testing$ozone_STM, functions = "nmae")
#0.08553398  




#Mean Absolute Percentage Error
mape(testing$PM_STM, pred.gam)
#NA







#plots for model performances
boxplot(pred.gam.num~testing$PM_STM, data = Bej_data)






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



