######################################################################
######################################################################
######################################################################
## Student: Alejandro Diaz Salgado
## Student ID: X17104050
## Project: Final Project for Data Analysis
## Topic: Passenger utilization on railway network using Timeseries Forecasting
######################################################################
######################################################################
######################################################################


#///////LIST OF PACKAGES USED/////////#
library(readr)
library(xlsx)
library(tidyverse)
library(readxl)
library(plotly)
library(ggthemes)
library(forecast)
library(autoTS)
library(ggpubr)




###########################################################
###########################################################

## Dataset upload, preparation, filtering and plotting

###########################################################
###########################################################


##the code below is to avoid scientific notation appearing for big values
options(scipen=999)

## finding the dataset on the file system
file.choose()



#reading the file using read_excel function from the readXL Library 

trainArg <- read_excel("C:\\Users\\alexd\\OneDrive\\Escritorio\\SUMMER\\Datasets finales\\arg1.xlsx")
summary(trainArg)



### FILTERING DATA TO OBTAIN RESULT FROM RELEVANT STATIONS ###


#Filter Data by Station
RetTrain <- trainArg %>% filter(estacion == "Retiro" )
HaedoTrain <- trainArg %>% filter(estacion == "Haedo" | estacion == "HAEDO")
FloridaTrain <- trainArg %>% filter(estacion == "Florida" | estacion == "FLORIDA")

#Applying another filter for date range since AutoTS does not work well with ts() timeseries objects
RetTrain3x <- trainArg %>% filter(estacion == "Retiro" ) %>% filter(mes >= as.Date("2017-05-01") & mes <= as.Date("2020-02-01"))
HaedoTrain3x <- trainArg %>% filter(estacion == "Haedo" | estacion == "HAEDO")%>% filter(mes >= as.Date("2017-05-01") & mes <= as.Date("2020-02-01"))
FloridaTrain3x <- trainArg %>% filter(estacion == "Florida" | estacion == "FLORIDA")%>% filter(mes >= as.Date("2017-05-01") & mes <= as.Date("2020-02-01"))




############ creating timeseries objects. ####################################
###########  We leave the last datapoint 2020-03 out of the analysis #########



RetiroTS<- ts(RetTrain[,4], start=c(2017,5), end=c(2020,2), frequency = 12) 
HaedoTS<- ts(HaedoTrain[,4], start=c(2017,5), end=c(2020,2), frequency = 12) 
FloridaTS<- ts(FloridaTrain[,4], start=c(2017,5), end=c(2020,2), frequency = 12) 


########### Plotting the current data we have ###########

autoplot(RetiroTS) + 
  theme_solarized() + 
  labs(title = "Passenger utilization in Retiro station", y="Number of passengers", x="Date")

autoplot(HaedoTS) + 
  theme_solarized() + 
  labs(title = "Passenger utilization in Haedo station", y="Number of passengers", x="Date")

autoplot(FloridaTS) + 
  theme_solarized() + 
  labs(title = "Passenger utilization in Florida station", y="Number of passengers", x="Date")









##########################################
##########################################
### NORMALITY > GGQQPLOTS > SHAPIRO > HIST
##########################################
##########################################






###RETIRO NORMALITY USING QUANTILE-QUANTILE, SHARPIRO-TEST, HISTORGRAM

Ret_QQs = ggqqplot(RetTrain, x="cantidad", color = "estacion",
                   main = "Normality check in Retiro", ylab = "Passenger Usage") +
  facet_wrap(~estacion) + theme_solarized()
Ret_QQs

shapiro.test(RetTrain$cantidad)

hist(RetTrain$cantidad, main = "Passenger usage histogram for Retiro" , xlab = "Passenger usage")
ggplot(RetTrain, aes(y=cantidad)) + geom_histogram() + labs(x="Number of repetitions", y="passenger usage", title="Retiro") + theme_solarized()




##HAEDO NORMALITY USING QUANTILE-QUANTILE, SHARPIRO-TEST, HISTORGRAM
Haedo_QQs = ggqqplot(HaedoTrain, x="cantidad", color = "estacion",
                     main = "Normality check in Haedo", ylab = "Passenger Usage") +
  facet_wrap(~estacion) + theme_solarized()
Haedo_QQs

shapiro.test(HaedoTrain$cantidad)

hist(HaedoTrain$cantidad, main = "Passenger usage histogram for Haedo" , xlab = "Passenger usage")
ggplot(HaedoTrain, aes(y=cantidad)) + geom_histogram() + labs(x="Number of repetitions", y="passenger usage", title="Haedo") + theme_solarized()



##FLORIDA NORMALITY USING QUANTILE-QUANTILE, SHARPIRO-TEST, HISTORGRAM

Florida_QQs = ggqqplot(FloridaTrain, x="cantidad", color = "estacion",
                       main = "Normality check in Florida", ylab = "Passenger Usage") +
  facet_wrap(~estacion) + theme_solarized()
Florida_QQs

shapiro.test(FloridaTrain$cantidad)

hist(FloridaTrain$cantidad, main = "Passenger usage histogram for Florida" , xlab = "Passenger usage")
ggplot(FloridaTrain, aes(y=cantidad)) + geom_histogram() + labs(x="Number of repetitions", y="passenger usage", title="FLORIDA") + theme_solarized()





####             ####
#### SEASONALITY ####
####             ####



##Using partial autocorrelation to look for seasonality trends on the data. 

ggPacf(RetiroTS) + scale_x_continuous(name="Lag", limits=c(1, 12)) + theme_solarized()
ggPacf(HaedoTS) + scale_x_continuous(name="Lag", limits=c(1, 12)) + theme_solarized()
ggPacf(FloridaTS) + scale_x_continuous(name="Lag", limits=c(1, 12)) + theme_solarized()






#######################
##### FORECASTING #####
#######################







#################
#~~~~ ARIMA ~~~~#
#################


#Applying ARIMA having some trend & seasonality

Retiro_arima <- auto.arima(RetiroTS)
print(Retiro_arima)


Haedo_arima <- auto.arima(HaedoTS)
print(Haedo_arima)


Florida_arima <- auto.arima(FloridaTS)
print(Retiro_arima)



#forecasting using ARIMA with trend&seasonality



fcst_ar_Retiro <- forecast(Retiro_arima,h=1)

fcst_ar_Retiro

autoplot(fcst_ar_Retiro) + 
  labs(subtitle="Retiro station", y="Number of passengers", x="Date") + 
  theme_solarized()





fcst_ar_Haedo <- forecast(Haedo_arima,h=1)

fcst_ar_Haedo

ggplot(fcst_ar_Haedo) + 
  labs(subtitle="Haedo station", y="Number of passengers", x="Date") + 
  theme_solarized()





fcst_ar_Florida <- forecast(Florida_arima,h=1)

fcst_ar_Florida

autoplot(fcst_ar_Florida) + 
  labs(subtitle="Florida station", y="Number of passengers", x="Date") + 
  theme_solarized()


#Applying ARIMA with no trend and seasonality using d & D arguments 

TX1Retiro_arima <- auto.arima(RetiroTS, d=1, D=1, stepwise = FALSE, approximation = FALSE)
print(TX1Retiro_arima)

TX1Haedo_arima <- auto.arima(HaedoTS, d=1, D=1, stepwise = FALSE, approximation = FALSE)
print(TX1Haedo_arima)

TX1Florida_arima <- auto.arima(FloridaTS, d=1, D=1, stepwise = FALSE, approximation = FALSE)
print(TX1Florida_arima)


#forecasting using ARIMA with no trend & seasonality


Tx1fcstRet <- forecast(TX1Retiro_arima,h=1)
Tx1fcstRet

autoplot(Tx1fcstRet) + 
  labs(subtitle="Retiro station", y="Number of passengers", x="Date") + 
  theme_solarized()



Tx1fcstHae <- forecast(TX1Haedo_arima,h=1)
Tx1fcstHae

autoplot(Tx1fcstHae) + 
  labs(subtitle="Haedo station", y="Number of passengers", x="Date") + 
  theme_solarized()



Tx1fcstFlo <- forecast(TX1Florida_arima,h=1)
Tx1fcstFlo

autoplot(Tx1fcstFlo) + 
  labs(subtitle="Florida station", y="Number of passengers", x="Date") + 
  theme_solarized()


   





 

####              ####
#### ~~~AutoTS~~~ ####
####              ####




## Using getBestModel() function from AutoTS package


AutoTSBMRet <- getBestModel(RetTrain3x$mes, RetTrain3x$cantidad, freq = "month", complete = 1,
                            n_test = NA, graph = TRUE, 
                            algos = list("my.prophet", "my.sarima"),
                            bagged = "custom",
                            metric.error = my.rmse)
View(AutoTSBMRet)

AutoTSBMHae <- getBestModel(HaedoTrain3x$mes, HaedoTrain3x$cantidad, freq = "month", complete = 1,
                            n_test = NA, graph = TRUE, 
                            algos = list("my.prophet", "my.sarima"),
                            bagged = "custom",
                            metric.error = my.rmse) 

View(AutoTSBMHae)

AutoTSBMFlo <- getBestModel(FloridaTrain3x$mes, FloridaTrain3x$cantidad, freq = "month", complete = 1,
                            n_test = NA, graph = TRUE, 
                            algos = list("my.prophet", "my.sarima"),
                            bagged = "custom",
                            metric.error = my.rmse) 

View(AutoTSBMFlo)

















