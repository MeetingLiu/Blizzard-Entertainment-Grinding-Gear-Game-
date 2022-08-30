library(TSA)
library(tseries)
library(fGarch)


datab = read.csv("game2.csv",head=T)
datab


View(data)
data = ts(datab$price)


plot(data)
acf(data)

#view
bxh = BoxCox.ar(data)

bxh$mle # the mle of the power parameter = -0.4
bxh$ci # corresponding 95% C.I.

data1 = ((data^-0.4)-1)/-0.4
data1



#stationary
plot(data1)
acf((as.vector(data1)))
adf.test(data1)
kpss.test(data1)


#1 step diff
data2 = diff(data1)
plot(data2)
acf(as.vector(data2), main="ACF of time series")#MA3
pacf(as.vector(data2), main="PACF of time series")#ar3
eacf(as.vector(data2))#ma3
adf.test(diff(data2))
kpss.test(diff(data2))




#SARIMAx(1,1,0)(1,1,0)_7,outlier
a = arima(data2,order=c(0,0,3))
a
a = arima(data2,order=c(0,0,3),fixed = c(NA,0,NA,NA))
a 
res = residuals(a) 
plot(res)
acf(as.vector(res))
acf(as.vector(res*res))

b = arima(data2,order=c(3,0,0))
b
b = arima(data2,order=c(3,0,0),fixed = c(NA,0,NA,NA))
b
res2 = residuals(b) 
plot(res2)
acf(as.vector(res2))
acf(as.vector(res2*res2))


#acf
acf(res, main="Residual ACF of MA(3)")
LB.test(a,lag=7)
#mean
hist(res, main = "Histogram of residual (MA(3))")
t.test(res,mu=0)
#distribution
qqnorm(res, main="Normal Q-Q Plot of residual (MA(3))");qqline(res, col = 2)
shapiro.test(res)
ks.test(res,mean(res),sd(res))

#acf
acf(res2, main="Residual ACF of AR(3)")
LB.test(b,lag=7)
#mean
hist(res2, main = "Histogram of residual (AR(3))")
t.test(res2,mu=0)
#distribution
qqnorm(res2, main="Normal Q-Q Plot of residual (AR(3))");qqline(res2, col = 2)
shapiro.test(res2)
ks.test(res2,mean(res2),sd(res2))
McLeod.Li.test(y=res2, main="Mc.Leod.Li Test of residual (AR(3))")


res1


dataf




res <- data.frame(res=res2, row.names = F)

write.csv(res, "C:/Users/miomi/Desktop/109/109-1/時間序列/期末報告/圖片/aaa.csv")










