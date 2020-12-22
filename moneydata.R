
install.packages("TSA")
library(TSA)
install.packages("xts")
library(xts)
install.packages("GGally")
install.packages("forecast")
install.packages("fpp2")
library(GGally)
library(forecast)
library(fpp2)
install.packages("lubridate")
library(lubridate)
install.packages("TSPred")
library(TSPred)

install.packages("ggplot2")
library(ggplot2)
install.packages("xts")
library(xts)
install.packages("astsa")
library(astsa)

setwd("/Users/quazi/Desktop/PhD app/UNM COURSES/FALL 2018/ITTSA/project")
mdata<-read.csv("/Users/quazi/Desktop/PhD app/UNM COURSES/FALL 2018/ITTSA/project/maindatacsv.csv",header=T)
tdata<-read.csv("/Users/quazi/Desktop/PhD app/UNM COURSES/FALL 2018/ITTSA/project/forecast data.csv",header=T)
tdata
head(mdata)
class(mdata)
attach(mdata)
names(mdata)
str(mdata)

plot(mdata)
mdata
mdata_xts<-as.xts(mdata)

rm(mdate)
class(mdata)
class(tdata)
# transformation to ts-object
mdata$ymd <- ymd(paste0(mdata$Year, " ", mdata$Month, " ", "1")) # transform to year-month-date format

df3 <- aggregate(High ~ ymd, mdata, sum ) # aggregate by monthly count


x <- ts(1, freq = 12, start = c(1994, 1), end = c(2016, 12)) # create dummy ts
df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
as.Date(x)
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
df_r$High <- ifelse(is.na(df_r$High), 0, df_r$High) # substitute NA's to 0's
ts_r <- ts(df_r$High, freq = 12, start = c(1994, 1), end = c(2016, 12)) # transform to ts
print(ts_r, digits = 3)
class(ts_r)
ts_r


################# test data ##################

tdata$ymd <- ymd(paste0(tdata$Year, " ", tdata$Month, " ", "1")) # transform to year-month-date format
df3 <- aggregate(High ~ ymd, tdata, sum ) # aggregate by monthly count
x <- ts(1, freq = 12, start = c(2017, 1), end = c(2018, 12)) # create dummy ts
class(x)
class(df3)

df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
df3
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
df_r
rm(df_r)
df_r$High <- ifelse(is.na(df_r$High), 0, df_r$High) # substitute NA's to 0's
ts_r_t <- ts(df_r$High, freq = 12, start = c(2017, 1), end = c(2018, 12)) # transform to ts
print(ts_r_t, digits = 3)
class(ts_r_t)
ts_r_t
ts_full
actual<-window(ts_full,2017,2019,20)
actual
actual<-na.omit(actual)
actual

#################### test data #################

plot(ts_r, main="1USD VS INDIAN NATIONAL RUPEE",ylab="INR",las=1)
points(y=ts_r,x=as.vector(time(ts_r)),
       pch=as.vector(season(ts_r)))
#################trying log transformation##############
logts<-log(ts_r)
plot(logts, main="1USD VS log(INDIAN NATIONAL RUPEE)",ylab="INR",las=1)
Did not work

trying regular model
model1<-lm(ts_r~time(ts_r))
summary(model1)
model3<-lm(ts_r~season(ts_r)+time(ts_r)-1)
summary(model3)

ACF AND PACF
par(mfrow=c(1,2))
acf(as.vector(ts_r),main="ACF")
pacf(as.vector(ts_r),main="PACF")

First differencing*****************
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
model2
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
#########ARIMA MODEL######################################
candiate 1
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

candidate 2
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

auto.arima(ts_r)

fulldata<-read.csv(file.choose(),header=T)
fulldata$ymd <- ymd(paste0(fulldata$Year, " ", fulldata$Month, " ", "1")) # transform to year-month-date format
df3 <- aggregate(High ~ ymd, fulldata, sum ) # aggregate by monthly count
x <- ts(1, freq = 12, start = c(1994, 1), end = c(2018, 12)) # create dummy ts
class(x)
class(df3)

df2 <- data.frame(ymd=as.Date(x)) # convert to data.frame
df3
df_r <- merge(df2, df3,  all.x = TRUE) # merge data.frames (left join)
df_r
ts_full <- ts(df_r$High, freq = 12, start = c(1994, 1), end = c(2018, 12)) # transform to ts
print(ts_full, digits = 3)
class(ts_full)
ts_full

aaPred <- forecast(modelar,h=20)
aaPred
plot(aaPred,pch=19,ylab='Forecasts and Conf Limits',xlab='Year',type="o")
points(ts_r_t,pch=5)
forecasts<-aaPred$mean
forecasts
accuracy(forecasts,ts_r_t)


aaPred <- forecast(modelar2,h=20)
forecasts<-aaPred$mean
forecasts
accuracy(forecasts,ts_r_t)
plot(modelar2,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits")
points(ts_r_t,pch=5)


#########ARIMA MODEL######################################

#########ETS MODEL######################################
data(austourists)
austourists
aust<-window(austourists,start=2005)
aust
ts_r
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

#########ETS MODEL######################################

#########SARIMA MODEL######################################
candidate 1
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

checkresiduals(residuals(modelsar1))
Box.test(residuals(modelsar1),type="Ljung")
plot(diff(diff(ts_r),lag=12),ylab='Seasonal & First Differences',type='l',main="Seasonal & First Differences")

candidate 2
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
checkresiduals(residuals(modelsar2))
Box.test(residuals(modelsar2),type="Ljung")

aaPred <- forecast(modelsar1,h=20)
forecasts<-aaPred$mean
forecasts
accuracy(forecasts,ts_r_t)
plot(modelsar1,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits",type="o",col="black")
points(ts_r_t,pch=5,type="p",col="red")


aaPred <- forecast(modelsar2,h=20)
forecasts<-aaPred$mean
forecasts
accuracy(forecasts,ts_r_t)
plot(modelsar2,n.ahead=20,ylab='Forecasts and Conf Limits', pch=19,main="95% forecast limits",type="o",col="black")
points(ts_r_t,pch=5,type="p",col="purple")



#########SARIMA MODEL######################################
#########AIC AND BIC######################################
BIC(modelsar1)
BIC(modelsar2)
BIC(modelar)
BIC(modelar2)
AIC(modelsar1)
AIC(modelsar2)
AIC(modelar)
AIC(modelar2)

#########AIC AND BIC######################################
######### X11 DECOMP ######################################
install.packages("seasonal")
library(seasonal)
ts_r
data(elecequip)
ts_r %>% seas(x11="") -> fit
autoplot(fit)
fit


#########  STL DECOMP ######################################

ts_r %>% stl(t.window=13, s.window="periodic", robust=TRUE) -> fit2
autoplot(fit2)
fit2
fit2 <- stl(ts_r, t.window=13, s.window="periodic", robust=TRUE)
fit2 %>% seasadj() %>% naive() %>% autoplot() + ylab("INR vs 1USD")
aaPred <- forecast(fit2,h=20)
forecasts<-aaPred$mean
forecasts
accuracy(forecasts,ts_r_t)

#########  STL DECOMP ######################################
############ Naive method###########

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



gg<-2900-2700
ff<-sqrt(((20^2)/10)+((22^2)/10))
gg/ff
-0.3/sqrt(0.21/5)
