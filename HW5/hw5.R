#Prob 1
library(readr)
dataset<-read.csv("C:/Users/Raj Shah/Desktop/Studies UH/MATH 6359 Stastical Computing/Homework/Answers/HW5/dataset.csv")
attach(dataset)
dataset$sex<-as.factor(dataset$sex)
dataset$type<-as.factor(dataset$type)
k2.glm<-glm(delta~type+time+sex,family = binomial,data=dataset)
summary(k2.glm)

#ODDS for type 
k2.glm<-glm(delta~type+time+sex-1,family = binomial,data=dataset)
exp(cbind(Odds=coef(k2.glm),confint(k2.glm)))

#odds for sex
k2.glm<-glm(delta~sex+time+type-1,family = binomial,data=dataset)
exp(cbind(Odds=coef(k2.glm),confint(k2.glm)))

#Odds Ratio
k2.glm<-glm(delta~type+time+sex,family = binomial,data=dataset)
exp(coef(k2.glm))

#Prob 2
library(forecast)
tsdisplay(sunspot.year)
tsdisplay(diff(sunspot.year,12),main="Seasonal Differencing")
tsadjs<-diff(sunspot.year,12)
tsdisplay(diff(tsadjs),main="Differencing to observe Stationarity")
arima.param <- list(c(3,1,0),c(4,1,0),c(7,1,0),c(6,1,0),c(5,1,0))
arima.aic <- sapply(arima.param,function(x) Arima(diff(tsadjs),order=x)$aic)
print(arima.param[[which.min(arima.aic)]])
my.arima.fit <-Arima(diff(tsadjs),c(7,1,0))
summary(my.arima.fit)


par(mfrow=c(1,2))
plot(forecast(my.arima.fit))
plot(forecast(auto.arima(diff(tsadjs),seasonal=F)))
par(mfrow=c(1,1))

