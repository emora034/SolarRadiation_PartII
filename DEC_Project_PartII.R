# delete everything
rm(list=ls()) 

library(skimr)
library(dplyr)
library(reshape2) #melt function
library(ggplot2)
library(tidyverse)
library(lubridate)
library(caret)
library(gsubfn)
library(MASS)
library(tidyr)
library(psych) #pairs.panels function
library(corrplot)
library(plyr) #revalue function
library(devtools)
library(ggpubr) #ggarrange function
library(doParallel)


set.seed(13)
#load the dataset
radiation <- read.csv("C:/Users/emora/OneDrive/Desktop/Adv Reg/Project Part I/SolarRadiation.csv", header=TRUE)

###############################
###############################
###### Data Engineering #######

#check for missing values
skim(radiation) #no missing values

#change the timestamp to date/time type
radiation$UNIXTime<-as.POSIXct(radiation$UNIXTime, origin="1970-01-01")
radiation<-radiation[,-2]
#note it's returned in CEST which is incorrect
#it takes 23 as 11am. so the radiation would be inaccurate

#Use the time column to get the correct hour
radiation$Hour<-factor(hour(as.POSIXct(radiation$Time, format="%H:%M:%S")))
#get ride of the time column
radiation<-radiation[,-2]
radiation<-radiation[,c(1,10,2:9)]

#we will not use sunrise and sunset times
radiation<-radiation[,c(-9,-10)]

#obtain the day of the week and the month from the POSIXct
radiation$Month<-(month(radiation$UNIXTime))
#eliminate measurements from the first month of the year
#there are only 132
radiation=radiation%>%filter(Month!=1)
radiation$Month<-factor(radiation$Month)

radiation$Day<-factor(weekdays(radiation$UNIXTime))
radiation$Month<-revalue(radiation$Month, c("9"="September", "10"="October",
                                            "11"="November", "12"="December"))


#radiation<-radiation[,-1]

#Basic plot of radiation by month
ggplot(radiation, aes(x=UNIXTime, y=Radiation))+geom_line()

#density
ggplot(radiation, aes(x=Radiation))+geom_density(fill="yellow")+
  ylab("")+xlab("Radiation")

#monthly Variations?

radiation %>%
  ggplot(aes(Month, Radiation)) + geom_boxplot(fill="yellow")  +
  xlab("Months") + ylab("Radiation") +
  theme_bw()  + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") 
#significantly less in December

# Any variation with day of the week?

radiation %>%
  ggplot(aes(Day, Radiation)) + geom_boxplot(fill="blue")  +
  xlab("Day of the Week") + ylab("Radiation") +
  theme_bw()  + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") 
#not much variation

#Hourly Variation
radiation%>%
  ggplot(aes(Hour, Radiation)) + geom_boxplot(fill="blue")   +
  xlab("Hour of the Day") + ylab("Radiation") +
  theme_bw()  + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") 
#high radiation between 0700-1700 hrs

#DECEMBER ONLY
radiationD<-radiation%>%filter(Month=="December")

radiationD%>%
  ggplot(aes(Hour, Radiation)) + geom_boxplot(fill="lightgreen")   +
  xlab("Hour of the Day") + ylab("Radiation") +
  theme_bw()  + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") 

# Temp relation:
ggplot(radiation, aes(Temperature, Radiation)) + geom_point()  +
  xlab("Temperature") + ylab("Radiation") +
  theme_bw()  + theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "horizontal") +
  facet_wrap(~ Month, scales = 'free_y', ncol = 2)


########################
########################
#### Adv Reg Models ####
radiation$DayofYear<-as.numeric(strftime(radiation$UNIXTime, format="%j"))
##############################

radiationD2<-radiationD[,c(-1,-5,-9-10)]
#divide data in 2 parts
in_train <- createDataPartition(radiationD2$Radiation, p = 0.75, list = FALSE)  # 75% for training
training <- radiationD2[ in_train,]
testing <- radiationD2[-in_train,]
nrow(training)
nrow(testing)
testing<-testing[,c(-7,-8)]

training<-training[,c(-7,-8)]
##############
#### SVR #####

fitall<-Radiation~Hour+Temperature+Humidity+WindDirection.Degrees.+Speed
#attemtp 1 with linear Kernel 
ctrl <- trainControl(method = "repeatedcv", number =20, repeats = 1)

svr_tune<- train(fitall, 
                 data = training,
                 method = "svmLinear",
                 #   method = "svmPoly",
                 #   method = "svmRadial",
                 preProc=c('scale','center'),
                 #na.action = na.omit,
                 trControl = ctrl,
                 #  tuneGrid = data.frame(C=c(0.1,.5,1)),
                 tuneLength = 20,
                 importance = TRUE,
                 verbose = T)

svr_tune
svr_tune$bestTune

#plot(svr_tune)

predictions = testing$Radiation
predictions[which(complete.cases(testing))] <- predict(svr_tune, newdata=testing[which(complete.cases(testing)), ])

df_fit <- data.frame(Hour=testing$Hour,Temperature=testing$Temperature, Humidity=testing$Humidity, 
                     WindDirection.Degrees=testing$WindDirection.Degrees., Speed=testing$Speed,
                     observed = testing$Radiation, prSVR = predictions, resSVR=testing$Radiation-predictions)

cor(df_fit$observed, df_fit$prSVR, use="complete")^2

ggplot(df_fit, aes(x = Temperature)) +
  geom_point(aes(y=observed), alpha = .5) + scale_colour_gradient(low = "white", high = "black") +
  geom_point(aes(y=prSVR), color="blue", alpha=0.5) +
  ggtitle("Out-of-sample fit - Linear Kernel")

#################################
#attemtp 2 with Polynomial Kernel 
ctrl <- trainControl(method = "repeatedcv", number =4, repeats = 1)

svr_tune <- train(fitall, 
                  data = training,
                  #   method = "svmLinear",
                  method = "svmPoly",
                  #   method = "svmRadial",
                  preProc=c('scale','center'),
                  #na.action = na.omit,
                  trControl = ctrl,
                  #  tuneGrid = data.frame(C=c(0.1,.5,1)),
                  tuneLength = 4,
                  importance = TRUE,
                  verbose = T)
svr_tune
svr_tune$bestTune

plot(svr_tune)

predictions = testing$Radiation
predictions[which(complete.cases(testing))] <- predict(svr_tune, newdata=testing[which(complete.cases(testing)), ])

df_fit <- data.frame(Hour=testing$Hour,Temperature=testing$Temperature, Humidity=testing$Humidity, 
                     WindDirection.Degrees=testing$WindDirection.Degrees., Speed=testing$Speed,
                     observed = testing$Radiation, prSVR = predictions, resSVR=testing$Radiation-predictions)

cor(df_fit$observed, df_fit$prSVR, use="complete")^2

ggplot(df_fit, aes(x = Temperature)) +
  geom_point(aes(y=observed), alpha = .5) + scale_colour_gradient(low = "white", high = "black") +
  geom_point(aes(y=prSVR), color="yellow", alpha=0.5) +
  ggtitle("Out-of-sample fit - Polynomial Kernel")

###############################
#attemtp 3 with Gaussian Kernel 
ctrl <- trainControl(method = "repeatedcv", number =10, repeats = 1)
#fitall2<-Radiation ~ Temperature + Humidity + WindDirection.Degrees. + Speed
svr_tune <- train(fitall, 
                  data = training,
                  #   method = "svmLinear",
                  #   method = "svmPoly",
                  method = "svmRadial",
                  preProc=c('scale','center'),
                  #na.action = na.omit,
                  trControl = ctrl,
                  #  tuneGrid = data.frame(C=c(0.1,.5,1)),
                  tuneLength = 10,
                  importance = TRUE,
                  verbose = T)

svr_tune
svr_tune$bestTune

plot(svr_tune)

predictions = testing$Radiation
predictions[which(complete.cases(testing))] <- predict(svr_tune, newdata=testing[which(complete.cases(testing)), ])

df_fit <- data.frame(Hour=testing$Hour,Temperature=testing$Temperature, Humidity=testing$Humidity, 
                     WindDirection.Degrees=testing$WindDirection.Degrees., Speed=testing$Speed,
                     observed = testing$Radiation, prSVR = predictions, resSVR=testing$Radiation-predictions)

cor(df_fit$observed, df_fit$prSVR, use="complete")^2

ggplot(df_fit, aes(x = Temperature)) +
  geom_point(aes(y=observed), alpha = .5) + scale_colour_gradient(low = "white", high = "black") +
  geom_point(aes(y=prSVR), color="red", alpha=0.5) +
  ggtitle("Out-of-sample fit - Gaussian Kernel")

########################
#### Random Forest #####

ctrl <- trainControl(method = "repeatedcv", number =4, repeats = 1)

rf_tune <- train(fitall, 
                 data = training,
                 method = "rf",
                 preProc=c('scale','center'),
                 #na.action = na.omit,
                 trControl = ctrl,
                 ntree = 300,
                 #tuneGrid = data.frame(mtry=c(5,10,15)),
                 tuneLength = 4,
                 importance = TRUE,
                 verbose = T)

plot(varImp(rf_tune, scale = F), scales = list(y = list(cex = .95)))

predictions = testing$Radiation
predictions[which(complete.cases(testing))] <- predict(rf_tune, newdata=testing[which(complete.cases(testing)), ])

df_fit <- data.frame(Hour=testing$Hour,Temperature=testing$Temperature, Humidity=testing$Humidity, 
                     WindDirection.Degrees=testing$WindDirection.Degrees., Speed=testing$Speed,
                     observed = testing$Radiation, prRf = predictions, resRf=testing$Radiation-predictions)

cor(df_fit$observed, df_fit$prRf, use="complete")^2

ggplot(df_fit, aes(x = Temperature)) +
  geom_point(aes(y=observed), alpha = .5) + scale_colour_gradient(low = "white", high = "black") +
  geom_point(aes(y=prRf), color="pink", alpha=0.5) +
  ggtitle("Out-of-sample fit - Random Forest")



#############################
#############################
#####Gradient Boosting ######

ctrl <- trainControl(method = "repeatedcv", number =10, repeats = 1)

rf_tune <- train(fitall, 
                 data = training,
                 method = "gbm",
                 metric="RMSE",
                 preProc=c('scale','center'),
                 #na.action = na.omit,
                 trControl = ctrl,
                 #ntree = 100,
                 #tuneGrid = data.frame(mtry=c(5,10,15)),
                 tuneLength = 10,
                 #importance = TRUE,
                 verbose = T)

predictions = testing$Radiation
predictions[which(complete.cases(testing))] <- predict(rf_tune, newdata=testing[which(complete.cases(testing)), ])

df_fit <- data.frame(Hour=testing$Hour,Temperature=testing$Temperature, Humidity=testing$Humidity, 
                     WindDirection.Degrees=testing$WindDirection.Degrees., Speed=testing$Speed,
                     observed = testing$Radiation, prRf = predictions, resRf=testing$Radiation-predictions)

cor(df_fit$observed, df_fit$prRf, use="complete")^2

ggplot(df_fit, aes(x = Temperature)) +
  geom_point(aes(y=observed), alpha = .5) + scale_colour_gradient(low = "white", high = "black") +
  geom_point(aes(y=prRf), color="orange", alpha=0.5) +
  ggtitle("Out-of-sample fit - Gradient Boosting")


###############################
###############################
###############################
####### Neural Networks #######

#ctrl <- trainControl(method = "LOOCV", 
#                     number = 5, repeats = 10)
ctrl <- trainControl(method = "cv", number = 5)


cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

nnetFit = train(fitall,"nnet", data=training, metric = "RMSE", 
                tuneGrid = expand.grid(size=c(5,10), 
                decay=c(0.01,0.02,0.03,0.5)), 
                preProcess = c("center", "scale"), 
                trControl = ctrl)
nnetFit
neuralnetFit = train(fitall,"neuralnet", 
                     data=training, metric = "RMSE", 
                     tuneGrid = expand.grid(layer1=c(2,3,4),layer2=c(2,3),layer3=0), 
                     preProcess = c("center", "scale"), 
                     trControl = ctrl)
neuralnetFit
stopCluster(cl)
on.exit(stopCluster(cl))
