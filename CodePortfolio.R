#Code Portfolio
#Spring '19
#by Asya Kirgiz

#Loading Data
##astsa
data(birth,package="oil")
##from local file as data frame format
oil.month = read.table(file.choose(), header=TRUE)

#Converting data to Time Series format
oil.ts = ts(oil.month,start=c(1930,1), freq=12)

#Plot Data
plot(oil.ts)
ts.plot(oil.ts)
plot(aggregate(oil.ts))
boxplot(oil ~ cycle(oil.ts))

#Investigate data
##Agregate, windows
oil.annual.ts <- aggregate(oil.ts)/12
oil.June <- window(oil.ts, start=c(1930,6), freq = TRUE)

##Decompose - from Intro TS with R by Cowperwait
oil.decom <- decompose(oil.ts, type="mult") #multiplicative model (vs. additive)
trend <- oil.decom$trend
seasonal <- oil.decom$seasonal
random <- oil.decom$random

##Correlation
cor(x,y) #values between -1 and +1, closer to 0 less correlation
acf(x)$acf[2] #autocorrelation with lag 1

#Model fitting

##Manual
trend = time(oil.ts)
fit = lm(oil.ts~ trend, na.action=NULL)

##Moving Average (MA) - from datacamp https://github.com/AsyaJ/code-portfolio
oil_change <- diff(oil.ts) #changes in oil rate
acf(oil_change, lag.mx = 12)
MA_oil_change <- arima(oil_change, order=c(0,0,1)) #1st order MA model

##Examine residuals
residuals(MA_oil_change)

##Auto Regression (AR)
AR_oil_change <- arima(oil_change, order=c(1,0,0)) #1st order AR model

##ARIMA
oil_returns = diff(log(oil))
acf2(oil_returns)
sarima(oil.ts,1,0,1) #fit 1st order MA and 1st order AR model

#Evaluate model with information criterion
AIC(MA_oil_change) #lower value means better the model
BIC(MA_oil_change)

#Forecasting
predict(MA_oil_change) #one step ahead
predict(MA_oil_change, n.ahead = 5) #5 steps ahead

##Naive Method - from TS Analysis and its Applications with R by Shumway
fc <- naive(oil.ts)
checkresiduals(fc) #displays info and plots residuals of the forecast
accuracy(fc, test) #displays forecast error for different calculation methods

##sarima
sarima.for(oil.ts,1,0,1)

