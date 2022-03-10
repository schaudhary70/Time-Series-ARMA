#no. of users logged on to internet server each minute over a 100-minute period

#STEP 1: Visualize the time series
library(fma)
plot(internet)

#We observe no seasonality on data. trend is there and cyclicity can be seen there
#STEP 2: Make the series stationary

internetD<-diff(internet) #This is method to make it stationary
plot(internetD)
library(tseries)
adf.test(internetD) #Throgh Dickey-Fuller Test we can see the Stationarity of time-series

#In Dickey-Fuller Null Hypothesis: Series is non-stationary 
# If p value <0.05 fail to reject Reject null hypothesis
#Here p=.35>.05 so we cannot reject null hypothesis and This time-series is not-stationary


#So we do second defferencing
internetDD<-diff(internetD)
plot(internetDD)
adf.test(internetDD)#P<0.05 so times-series is stationary


#STEP 3: ACF and PACF to see if we can use ARMA model

par(mfrow=c(1,2))
acf(internetDD)
pacf(internetDD)
#Here AR(2) could be a resonable choice because in PACF it's cutting at lag 2. PACF starts from lag 1
#ACF starts from lag0, and it's cutting at lag 2.
#Best choice could be  AR(2) or MA(2) cz lag 2 is out of limit in ACF.
#But for now let's check and do it with AR(2)


#STEP 4: Fit the ARMA(p,q) model
#FIT ARMA(2,0) 

modl<-arma(internetDD, order=c(2,0,0)) #p=2 for AR(2) model, q=0 as there is not MA model
#d=0 cz we are not any differencing data, it has already been differentiated.
#If we would have put internet data here then we could have write d=2, for difeerencing twice


#STEP 5: Diagnose Test:  Analyze the residuals 
par(mfrow=c(2,2))
r1<-residuals(modl)
plot(na.omit(r1)) #Exclude all misiing values from residual,yt-yt-1 but before that value will be misisng
qqnorm(as.vector(r1), main="")
qqline(r1, distribution=qnorm)
hist(r1, main="")#It has good normal distribution shape
acf(na.omit(r1))
#In ACF autocorrelation is 0 for all lags in stationary data, White noise is there
#So its a good model






































