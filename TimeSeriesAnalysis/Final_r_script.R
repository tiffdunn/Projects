
setwd("~/STA137F19/FinalProject")
library(astsa)
library(lmtest)
vars = c("sales", "deaths")
data = read.table("GD.dat.txt", sep = "", header = FALSE, col.names = vars)
dim(data)   #  monthly data from 1980 - 1998

sales.d = data[,1]
deaths.d = data[,2]
sales.d = ts(sales.d, start = c(1980,1), frequency = 12)
deaths.d = ts(deaths.d, start = c(1980,1), frequency = 12)

par(mfrow=c(2,1))
tsplot(sales.d, ylab = "sales per 100,000", main = "Monthly Handgun Sales", xlab = "year")
tsplot(deaths.d, ylab = "deaths per 100,000", main = "Monthly Firearm Related Deaths", xlab = "year")


## hand gun sales data analysis 

tsplot(sales.d)
acf2(sales.d)

tsplot(diff(sales.d))
acf2(diff(sales.d))

tsplot(diff(diff(sales.d)))
acf2(diff(diff(sales.d)), ylab=expression(nabla~nabla[12]~"sales"))

m1 = sarima(sales.d, p=0,d=2,q=8, P=1,D=0,Q=0,S=12) ## figure 1. 
m1

m2 = sarima(sales.d, p=0,d=2,q=11, P=1,D=0,Q=0,S=12) ## dont fucking change this 
m2

fitARIMA <- arima(sales.d, order=c(0,2,8),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA)
confint(fitARIMA)





## firearm deaths data analysis 


par(mfrow=c(3,1))
tsplot(deaths.d, col=4)
tsplot(log(deaths.d), col=4)
tsplot(diff(diff(deaths.d)), col=4)

acf2((deaths.d))

acf2(diff(diff(deaths.d)), ylab=expression(nabla~nabla[12]~"deaths"))


fm1 = sarima(deaths.d, p=0,d=1,q=2, P=2,D=1,Q=0, S=12)  ## figure 4
fm1

fm2 = sarima(deaths.d, p=0,d=0,q=2, P=2,D=2,Q=2, S=4)  
fm2

## ccf 

ccf2(sales.d, deaths.d)
ts(sales.d)

c(m1$AIC,m1$AICc,m1$BIC)
c(m2$AIC,m2$AICc,m2$BIC)

c(fm1$AIC,m1$AICc,m1$BIC)
c(fm2$AIC,m2$AICc,m2$BIC)






