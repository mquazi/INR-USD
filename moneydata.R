#######################################################################
########################### packages needed ###########################
#######################################################################
install.packages("TSA")
install.packages("GGally")
install.packages("forecast")
install.packages("fpp2")
install.packages("xts")
install.packages("TSPred")
install.packages("lubridate")
install.packages("astsa")
install.packages("ggplot2")
library(TSA)
library(xts)
library(lubridate)
library(GGally)
library(forecast)
library(fpp2)
library(TSPred)
library(ggplot2)
library(xts)
library(astsa)

#######################################################################
########################### input data ################################
#######################################################################
setwd("../Time_Series_Project")  #set working directory
mdata<-read.csv(file.choose(),header=T) #read maindatacsv.csv in 
tdata<-read.csv(file.choose(),header=T) #read forecastdata.csv in 
head(mdata)
attach(mdata)

###############################################################################################
########################### convert data to time series object ################################
###############################################################################################
# transformation to time series-object

mdata$ymd <- ymd(paste0(mdata$Year, " ", mdata$Month, " ", "1")) # transform to year-month-date format
df3 <- aggregate(High ~ ymd, mdata, sum ) # aggregate by monthly count
x <- ts(1, freq = 12, start = c(1994, 1), end = c(2016, 12)) # create dummy ts
df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
df_r$High <- ifelse(is.na(df_r$High), 0, df_r$High) # substitute NA's to 0's
ts_r <- ts(df_r$High, freq = 12, start = c(1994, 1), end = c(2016, 12)) # transform to ts
print(ts_r, digits = 3)

# test data 

tdata$ymd <- ymd(paste0(tdata$Year, " ", tdata$Month, " ", "1")) # transform to year-month-date format
df3 <- aggregate(High ~ ymd, tdata, sum ) # aggregate by monthly count
x <- ts(1, freq = 12, start = c(2017, 1), end = c(2018, 12)) # create dummy ts
df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
df_r$High <- ifelse(is.na(df_r$High), 0, df_r$High) # substitute NA's to 0's
ts_r_t <- ts(df_r$High, freq = 12, start = c(2017, 1), end = c(2018, 12)) # transform to ts
print(ts_r_t, digits = 3)

###############################################################################################
########################### preliminary data analysis #########################################
###############################################################################################
plot(ts_r, main="1USD VS INDIAN NATIONAL RUPEE",ylab="INR",las=1)
points(y=ts_r,x=as.vector(time(ts_r)),
       pch=as.vector(season(ts_r)))
#################trying log transformation##############
logts<-log(ts_r)
plot(logts, main="1USD VS log(INDIAN NATIONAL RUPEE)",ylab="INR",las=1)
##### Log trasnformation did not work well 


####################################################################################
########################### model building #########################################
####################################################################################
# trying regular model
model1<-lm(ts_r~time(ts_r))
summary(model1)
model3<-lm(ts_r~season(ts_r)+time(ts_r)-1)
summary(model3)

# ACF AND PACF
par(mfrow=c(1,2))
acf(as.vector(ts_r),main="ACF")
pacf(as.vector(ts_r),main="PACF")

# First differencing
plot(diff(ts_r),type='o',ylab='Diff(INR)',
     main="Time Series INR")
plot(diff(ts_r,lag=12),ylab='Seasonal & First Differences',type='l',main="Seasonal & First Differences")
points(y=diff(ts_r,lag=12),x=time(diff(ts_r,lag=12)),pch=as.vector(season(diff(ts_r,lag=12))))
acf(diff(ts_r,lag=12),lag.max=100)
pacf(diff(ts_r,lag=12),lag.max=100)
acf(diff(diff(ts_r,lag=12),lag=12),lag.max=100)
pacf(diff(diff(ts_r,lag=12),lag=12),lag.max=100)
acf(diff(ts_r),lag.max=200)
acf(diff(diff(ts_r),lag=12),lag.max=100)
pacf(diff(diff(ts_r),lag=12),lag.max=100)
model2=arima(ts_r,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
plot(diff(diff(ts_r)),type='o',ylab='Dff(Diff(INR))',
     main="Time Series INR")
subset=armasubsets(y=ts(ts_r),nar=10,nma=10,y.name='INR series',ar.method='ols')
plot(subset)
model4<-arima(ts_r,order=c(1,0,0))
model4
model5<-arima(ts_r,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12))
model5
model6=arima(ts_r,order=c(0,1,0),seasonal=list(order=c(0,1,1),period=12))
acf(model6$residuals,lag.max=100)
pacf(model6$residuals,lag.max=100)
model6

plot(diff(diff(ts_r),lag=12),ylab='Seasonal & First Differences',type='l',main="Seasonal & First Differences")
acf(as.vector(model6$residuals),lag.max=100)

eacf(model6$residuals)
BoxCox.ar(ts_r)
plot(diff((ts_r)^0.5),type="l")
acf(diff)
acf(ts_r)

####################################################################################
########################### ARIMA models ###########################################
####################################################################################
# candiate 1
modelar<-arima(ts_r,order=c(0,1,1))
modelar
acf(as.vector(modelar$residuals),lag.max=100)
pacf(as.vector(modelar$residuals),lag.max=100)
tsdiag(modelar)
hist(residuals(modelar),xlab='Residuals')
qqnorm(residuals(modelar))
qqline(residuals(modelar))
par(mfrow=c(1,2))
shapiro.test(residuals(modelar))
eacf(modelar$residuals)
checkresiduals(residuals(modelar))
Box.test(residuals(modelar),type="Ljung")

# candidate 2
modelar2<-arima(ts_r,order=c(2,1,2))
modelar2
acf(as.vector(modelar2$residuals),lag.max=100)
pacf(as.vector(modelar2$residuals),lag.max=100)
tsdiag(modelar2)
hist(residuals(modelar2),xlab='Residuals')
qqnorm(residuals(modelar2))
qqline(residuals(modelar2))
par(mfrow=c(1,1))
shapiro.test(residuals(modelar2))
eacf(modelar2$residuals)
checkresiduals(residuals(modelar2))
Box.test(residuals(modelar2),type="Ljung")

fulldata<-read.csv(file.choose(),header=T) #read maindatacsv.csv in 
fulldata$ymd <- ymd(paste0(fulldata$Year, " ", fulldata$Month, " ", "1")) # transform to year-month-date format
df3 <- aggregate(High ~ ymd, fulldata, sum ) # aggregate by monthly count
x <- ts(1, freq = 12, start = c(1994, 1), end = c(2018, 12)) # create dummy ts

df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
ts_full <- ts(df_r$High, freq = 12, start = c(1994, 1), end = c(2018, 12)) # transform to ts
print(ts_full, digits = 3)

aaPred <- forecast(modelar,h=20) # predictions
plot(aaPred,pch=19,ylab='Forecasts and Conf Limits',xlab='Year',type="o")
points(ts_r_t,pch=5)
forecasts<-aaPred$mean
accuracy(forecasts,ts_r_t)


aaPred <- forecast(modelar2,h=20)
forecasts<-aaPred$mean
accuracy(forecasts,ts_r_t)
plot(modelar2,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits")
points(ts_r_t,pch=5)

####################################################################################
########################### ETS model ##############################################
####################################################################################
etsdata<-window(ts_r,start=2005)
etsdata
etsfit<-ets(etsdata)
summary(etsfit)
autoplot(etsfit)
cbind('Residuals'=residuals(etsfit),'Forecast errors'=residuals(etsfit,type='response'))%>%autoplot(facet=TRUE)+xlab("Year")+ylab("")
etsfit %>% forecast(h=20) %>%
        autoplot() + 
        ylab("1USD against INR")
aaPred <- forecast(etsfit,h=20)
plot(aaPred,pch=19,main=,ylab='Forecasts and Conf Limits',xlab='Year',type="o")
points(ts_r_t,pch=5)
hat<-aaPred$mean
accuracy(hat,ts_r_t)

####################################################################################
########################### SARIMA models ##########################################
####################################################################################
# candidate 1
par(mfrow=c(1,2))
modelsar1=arima(ts_r,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
acf(as.vector(modelsar1$residuals),lag.max=100)
pacf(as.vector(modelsar1$residuals),lag.max=100)
modelsar1
tsdiag(modelsar1)
hist(residuals(modelsar1),xlab='Residuals')
qqnorm(residuals(modelsar1))
qqline(residuals(modelsar1))
par(mfrow=c(1,1))
shapiro.test(residuals(modelsar1))
eacf(modelsar1$residuals)

checkresiduals(residuals(modelsar1)) # Diangostics
Box.test(residuals(modelsar1),type="Ljung")
plot(diff(diff(ts_r),lag=12),ylab='Seasonal & First Differences',type='l',main="Seasonal & First Differences")

# candidate 2
modelsar2=arima(ts_r,order=c(2,1,2),seasonal=list(order=c(0,1,1),period=12))
acf(as.vector(modelsar2$residuals),lag.max=100)
pacf(as.vector(modelsar2$residuals),lag.max=100)
modelsar2
tsdiag(modelsar2)
hist(residuals(modelsar2),xlab='Residuals')
qqnorm(residuals(modelsar2))
qqline(residuals(modelsar2))
par(mfrow=c(1,1))
shapiro.test(residuals(modelsar2))
eacf(modelsar2$residuals)
checkresiduals(residuals(modelsar2)) # Diangostics
Box.test(residuals(modelsar2),type="Ljung")

# Predictions 
aaPred <- forecast(modelsar1,h=20)
forecasts<-aaPred$mean
accuracy(forecasts,ts_r_t)
plot(modelsar1,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits",type="o",col="black")
points(ts_r_t,pch=5,type="p",col="red")

aaPred <- forecast(modelsar2,h=20)
forecasts<-aaPred$mean
accuracy(forecasts,ts_r_t)
plot(modelsar2,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits",type="o",col="black")
points(ts_r_t,pch=5,type="p",col="purple")

################################################################################
########################### AIC & BIC ##########################################
################################################################################
BIC(modelsar1)
BIC(modelsar2)
BIC(modelar)
BIC(modelar2)
AIC(modelsar1)
AIC(modelsar2)
AIC(modelar)
AIC(modelar2)


################################################################################
########################### X11 Decomposition ##################################
################################################################################
install.packages("seasonal")
library(seasonal)
data(elecequip)
ts_r %>% seas(x11="") -> fit
autoplot(fit)
fit

################################################################################
########################### STL Decomposition ##################################
################################################################################

ts_r %>% stl(t.window=13, s.window="periodic", robust=TRUE) -> fit2

fit2 <- stl(ts_r, t.window=13, s.window="periodic", robust=TRUE)
fit2 %>% seasadj() %>% naive() %>% autoplot() + ylab("INR vs 1USD")
autoplot(fit2)
aaPred <- forecast(fit2,h=20)
forecasts<-aaPred$mean
accuracy(forecasts,ts_r_t)

###########################################################################
########################### Naive Method ##################################
###########################################################################

autoplot(ts_r) +
        autolayer(meanf(ts_r, h=20),
                  series="Mean", PI=FALSE) +
        autolayer(naive(ts_r, h=20),
                  series="Naïve", PI=FALSE) +
        autolayer(snaive(ts_r, h=20),
                  series="Seasonal naïve", PI=FALSE) +
        ggtitle("Forecasts for INR gainst 1USD") +
        xlab("Year") + ylab("INR") +
        guides(colour=guide_legend(title="Forecast"))

autoplot(ts_r) +
        autolayer(meanf(ts_r, h=20),
                  series="Mean", PI=FALSE) +
        autolayer(rwf(ts_r, h=20),
                  series="Naïve", PI=FALSE) +
        autolayer(rwf(ts_r, drift=TRUE, h=20),
                  series="Drift", PI=FALSE) +
        ggtitle("Forecasts for INR gainst 1USD") +
        xlab("Year") + ylab("INR)") +
        guides(colour=guide_legend(title="Forecast"))

