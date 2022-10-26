rm( list = ls())

###***Holdout Analysis***###

#install necessary library functions
library(data.table)
library(tidyverse)
library(carData)
library(car)
library(pryr)
library(fst)
library(corrplot)
library(ggplot2)
library(reshape2)
library(lattice)
library(caret)
library(mlbench)
library(ISLR)
library(Rcpp)
library(vctrs)
library(Metrics)
library(tdr)
library(hydroGOF)


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


#start a vector to save the evaluation statistics
library(mgcv)

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

library(party)
library(randomForest)
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

