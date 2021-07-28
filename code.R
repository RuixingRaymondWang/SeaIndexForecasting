
if(!require(tseries)) install.packages("tseries", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")

library(tseries)
library(forecast)


# The data could be found online at: https://nsidc.org/data/seaice_index

#import the data
S12<- read.csv("./S_12_extent_v3.0.csv")


## Preprocessing
#Before we build the model, we need to reduce the data to stationary
#Step1: firstly, I set extent as x and then plot it.
x<-ts(S12$extent,start=1979,frequency=1)
plot(x,las=1)

#Step2: I find that there is a particular point, which can be seen as an outlier. 
#So I set that value to be the mean value of the data, (see that plot now).
x[10]=NA
x[10]=mean(x,na.rm = T)
plot(x,las=1)

#Step3: through “adf.test(x)” this code, I find that p-value of this data is 0.8016, which is bigger than 0.05, hence I got conclusion that this data set is not stationary.
adf.test(x)
diff(x)

#Step4: through differencing x, and then use “adf.test(diff(x))”, I can see that if this data after differencing can be stationary. The result is that p-value is 0.02163, which is less than 0.05.
adf.test(diff(x))


#Step5: then I got conclusion that diffenencing this data can make it to stationarity.
plot(diff(x),las=1)


# Methods
## ARIMA
set.seed(116101521)
Y<-arima.sim(list(order=c(1,2,1),ar=c(0.7),ma=c(-0.5)),n=500)
plot(Y,las=1,main='')
acf(Y,las=1,main='')
pacf(Y,las=1,main='')


plot(WWWusage)
aics <- matrix(, 6, 6, dimnames = list(p = 0:5, q = 0:5))


#For ARIMA, the choices of p and q are important
#Through a “for loop”, The possible p and q values are brought into the for loop to obtain several possible models. Then the aic method is used to obtain the optimal p and q values that minimize aic.

for(q in 0:1) aics[1, 1+q] <- arima(WWWusage, c(0, 1, q),
                                    optim.control = list(maxit = 500))$aic


for(p in 1:3)
        for(q in 0:1) aics[1+p, 1+q] <- arima(WWWusage, c(p, 1, q),
                                              optim.control = list(maxit = 500))$aic
(round(aics - min(aics, na.rm = TRUE), 2))


Best<-arima(WWWusage, c(3, 1, 0),optim.control = list(maxit = 500))
plot(Best)

#Use the best p and q I’ve find to establish the arima model.
#After establish that model, I plot that model and make forcast for it.
plot(forecast(Best),col = "black")

# Results
#This result shows this AR model is not stationary since the plot that the three points all inside the unit circle.
plot(Best)


#This diagram shows that in different confident interval, I can get the forecast shows in blue.
plot(forecast(Best),col = "black")

