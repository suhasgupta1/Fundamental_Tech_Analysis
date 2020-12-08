

# (Wiley Series in Probability and Statistics) George E. P. Box, Gwilym M. Jenkins, Gregory C. Reinsel, Greta M. Ljung - Time Series Analysis_ Forecasting and Control-Wiley (2015).pdf
# (Wiley & SAS business series) Baesens, Bart_ Roesch, Daniel_ Scheule, Harald - Credit risk analytics _ measurement techniques, applications, and examples in SAS-Wiley (2016).pdf
# [Wiley Finance] John C. Hull - Risk Management and Financial Institutions (2018, Wiley).pdf
# Dowd.pdf
# 
# Introduction_to_R_for_Quantitative_Finance_1565324071985.pdf


# install.packages("moments", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("nortest", dependencies = TRUE)
# install.packages("goftest", dependencies = TRUE)
# install.packages("tseries", dependencies = TRUE)
# install.packages("jtrans", dependencies = TRUE)
# install.packages("urca", dependencies = TRUE)
# install.packages("randtests", dependencies = TRUE)
# install.packages("lmtest", dependencies = TRUE)
# install.packages("forecast", dependencies = TRUE)
# install.packages("sos", dependencies = TRUE)
# install.packages("lmtest", dependencies = TRUE)
# install.packages("forecast", dependencies = TRUE)
# install.packages("aTSA", dependencies = TRUE)
# install.packages("rugarch", dependencies = TRUE)
# install.packages("fBasics", dependencies = TRUE)
# install.packages("gamlss", dependencies = TRUE)
# install.packages("Dowd", dependencies = TRUE)
# install.packages("SuppDists", dependencies = TRUE)
# install.packages("installr", dependencies = TRUE)
# install.packages("GAS", dependencies = TRUE)
install.packages("xlsx", dependencies = TRUE)
library(xlsx)
# library(GAS) # VaR Computation
library(SuppDists) # VaR Computation
library(Dowd) # VaR Computation
library(installr)
library(gamlss)
library(fBasics)
library(rugarch)
library(aTSA)
library(lmtest)
library(sos)
library(forecast) # arima
library(lmtest)
library(randtests)
library(urca)
library(jtrans)
library(SuppDists) # Johnson distribution
library(MASS) # fitdistr
library(tseries)
library(goftest)
library(nortest)
library(dplyr)
library(moments)
library(ggplot2)
version
ls("package:sos")
updateR()
findFn("VaRTest")

# Data Load
rm(list = ls())
src1 <- read.csv('..//Data/YESBANK.BO.csv')
# tempsrc <- read.xlsx("..//Data/YESBANK.BO.xlsx", 1)
# head(tempsrc)
# newvar <- src1
# src1 <- tempsrc


# src1<-read.csv('C:/Users/sgupta7/Documents/bits/5th semester/project/21-02-2018-TO-20-02-2020YESBANKALLN.csv')
head(src1)

#Select Required Columns
src1 = src1[,c("Date","High.Price","Low.Price","Close.Price")]
head(src1)

#Data Types
typeof(src1)
colnames(src1)
typeof(src1$Date)
class(src1$Date)

src1$Date = as.Date(src1$Date, format = "%Y-%m-%d")
head(src1)
typeof(src1$Date)
class(src1$Date)


typeof(src1$High.Price)
class(src1$High.Price)
options(digits=11)

src1$High.Price <- as.numeric(as.matrix(src1)[,2])
head(src1)
class(src1$High.Price)
typeof(src1$High.Price)

typeof(src1$Low.Price)
class(src1$Low.Price)
src1$Low.Price <- as.numeric(as.matrix(src1[,3]))
head(src1)
typeof(src1$Low.Price)
class(src1$Low.Price)


typeof(src1$Close.Price)
class(src1$Close.Price)
src1$Close.Price <- as.numeric(as.matrix(src1[,4]))
head(src1)
typeof(src1$Close.Price)
class(src1$Close.Price)
# Transforms
# Sorting
src1 = src1[order(src1$Date),]
head(src1,50)

# Create returns column
# findFn("lag")
src1$prevClose <- lag(src1$Close.Price, n = 1)
head(src1)
src1$arithreturn = (as.numeric(src1$Close.Price)-src1$prevClose)/(src1$prevClose) 
src1$ccreturn <- c(NA,diff(log(src1$Close.Price))) # Compounded Consolidted Returns

head(src1)
na.omit(src1)

head(src1)

# Removing Null NA from data & also removing -infinity values
src2 <- na.omit(src1)
src2[src2$ccreturn == -Inf,]
src2 <- src2[src2$ccreturn != -Inf,]
head(src2)
length(unique(src2$Date,incomparables = FALSE))
#Plots
plot(src2$Date, src2$Close.Price, type = "l",main = "Trend Of Yes Bank Stock", ylab = "Closing Price")
plot(src2$Date, src2$ccreturn, type = "l",main = "Trend Of Yes Bank Stock Returns", ylab = "Returns")

hist(src2$Close.Price, breaks = 30, main = "Closing Price: Yes Bank")
hist(src2$ccreturn, breaks = 30, main = "Returns: Yes Bank")

boxplot(src2$Close.Price, main = "Closing Price: Yes Bank")
boxplot(src2$ccreturn, main = "Returns: Yes Bank")

# arithematic return scatter PLot
plot(src2$Close.Price, src2$arithreturn, main="Arithematic Return Vs Closing Price: YesBank",
     xlab="Closing Price ", ylab="Returns", pch=19)
# continuous compounding return scatter PLot
plot(src2$Close.Price, src2$ccreturn, main="Continuous Compounding Return Vs Closing Price: YesBank",
     xlab="Closing Price ", ylab="Returns", pch=19)

# this graph shows that volatility is more when stock price is lesser that is 
# leverage effect.

# Measures of Central Tendency
mean(src2$Close.Price)
nrow(src1)
median(src2$Close.Price)
sd(src2$Close.Price)
var(src2$Close.Price)
skewness(src1$Close.Price)
kurtosis(src1$Close.Price)


# Checking if data is normally distributed or not
jarque.bera.test(src2$Close.Price) # tseries
lillie.test(src2$Close.Price) # goftest , 
ad.test(src2$Close.Price) # goftest 
shapiro.test(src2$Close.Price)
qqnorm(src2$Close.Price);qqline(src2$Close.Price, col = 2, lwd = 3)

hist(src2$Close.Price, breaks = 30)
plot(density(src2$Close.Price))
abline(v = 60 , col = "purple")
abline(v = 150 , col = "purple")
abline(v = 325 , col = "purple")
# looks like multimodal, probably we fit platicurtic distribution.

######################## Test for Compounded Consolidted Returns ###################

mean(src2$ccreturn)
median(src2$ccreturn)
sd(src2$ccreturn)
var(src2$ccreturn)
skewness(src2$ccreturn)
kurtosis(src2$ccreturn)


jarque.bera.test(src2$ccreturn) # tseries
lillie.test(src2$ccreturn) # goftest , 
ad.test(src2$ccreturn)
shapiro.test(src2$ccreturn)
qqnorm(src2$ccreturn);qqline(src2$ccreturn, col = 2, lwd = 3)
plot(density(src2$ccreturn))
abline(v = 0 , col = "purple")

# Fitting the distributioin.
# If Symmetric : Using Skewness we can find if value close to 0
# Symmetric : Normal, logistic, students t, cauchy's Johnson
# Right Skewed : Log Normal, Log logistic, Gumbel, Exponentail, Pareto weibull, Burr, Frechet. Skew > 0
# Left skewed: Sq Normal, Inverted Gumbel, Gompertz. Skew < 0
# multi modal : platicurtic distribution

ecdf(src2$Close.Price)
ecdf(src2$ccreturn)

plot(ecdf(src2$Close.Price))
plot(ecdf(src2$ccreturn))

# GOF tests Kolmogorov Smirnov (KS) and Anderson-Darling (AD) test
# Trying if normal distribution fits data
fitdistribution <- fitdistr(src2$ccreturn, "normal")
fitdistribution
fitdistribution$estimate[2]
hist(src2$ccreturn, breaks = 30, freq = FALSE)
curve(dnorm(x,mean = fitdistribution$estimate[1],sd = fitdistribution$estimate[2]),from = -0.08, to = 0.11, add = TRUE, col= "red")
ks.test(src2$ccreturn, "pnorm", fitdistribution$estimate[1],fitdistribution$estimate[2])
goftest::ad.test(as.vector(src2$ccreturn), "pnorm", fitdistribution$estimate[1],fitdistribution$estimate[2])
randomdata <- rnorm(1228, fitdistribution$estimate[1], fitdistribution$estimate[2])
(randomdata)
qqplot(src2$ccreturn, randomdata); qqline(src2$ccreturn, col = "orange", lwd = 3)
dim(src2)
dim(randomdata)
cor(sort(as.vector(src2$ccreturn)),sort(as.vector(randomdata)))
nrow(as.vector(randomdata))
nrow(src2)


# Try fitting students t distribution to the ccreturn
fitdistribution_t <- fitdistr(src2$ccreturn, "t", start = list(m= mean(src2$ccreturn), s = sd(src2$ccreturn), df = 3), lower = c(-1, 0.001, 1))
fitdistribution_t
hist(src2$ccreturn, breaks = 30, freq = FALSE)
curve(dt(((x-fitdistribution_t$estimate[1])/fitdistribution_t$estimate[2]), df = fitdistribution_t$estimate[3]/fitdistribution_t$estimate[2]), from = -0.08, to = 0.11, log = FALSE, add = TRUE, col = "red")
ks.test(as.vector(src2$ccreturn), "pt", df = fitdistribution_t$estimate[3], fitdistribution_t$estimate[2])
goftest::ad.test(as.vector(src2$ccreturn), "pt", df = fitdistribution_t$estimate[3], fitdistribution_t$estimate[2])
randomdata_t <- rnorm(n = 1228, fitdistribution_t$estimate[1], fitdistribution_t$estimate[2])
qqplot(src2$ccreturn, randomdata_t);qqline(src2$ccreturn, lwd = 3 , col = "red")
cor(sort(as.vector(src2$ccreturn)),sort(randomdata_t))

# Try fitting Cauchy's Distribution
set.seed(3)
fitdistribution_c <- fitdistr(src2$ccreturn, "cauchy")
fitdistribution_c
hist(src2$ccreturn, breaks = 30, freq = FALSE)
curve(dcauchy(x, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2]), from = -0.08, to = 0.11, add = TRUE, col = "darkgreen")
ks.test(as.vector(src2$ccreturn), "pcauchy", fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
goftest::ad.test(as.vector(src2$ccreturn), "pcauchy", fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
randomdata_c <- rnorm(n = 1228, fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
qqplot(src2$ccreturn, randomdata_c)
cor(sort(src2$ccreturn), sort(randomdata_c))

# Logistic Distribution
fitdistribution_l = fitdistr(src2$ccreturn, "logistic")
fitdistribution_l
hist(src2$ccreturn, breaks = 30, freq = FALSE)
curve(dlogis(x, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2]), from = -0.08, to = 0.11, add = TRUE, col = "blue")
ks.test(as.vector(src2$ccreturn), "plogis", fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
goftest::ad.test(as.vector(src2$ccreturn), "plogis", fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
randomdata_l <- rlogis(n= 1228, fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
qqplot(src2$ccreturn, randomdata_l)
cor(sort(src2$ccreturn), sort(randomdata_l))

# Johnson Distribution
fitdistribution_j <- JohnsonFit(src2$ccreturn)
fitdistribution_j
hist(src2$ccreturn, breaks = 30, freq = FALSE)
curve(dJohnson(x, fitdistribution_j), from = -0.08, to = 0.11, add = TRUE, col = "Orange")
ks.test(as.vector(src2$ccreturn), "pJohnson", fitdistribution_j)
goftest::ad.test(src2$ccreturn, "pJohnson",fitdistribution_j)
randomdata_j <- rJohnson(n = 1228, fitdistribution_j)
qqplot(src2$ccreturn, randomdata_j)
cor(sort(src2$ccreturn), sort(randomdata_j))

# Calculating the risk using different distributions
# Normal , T , Cauchy's , Logistic , Johnson
print(paste("*******************************Chances of loosing Greater than or equal to 3% in a day ***********************"))
pnorm(-0.03, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(-0.03, df = 3)
plogis(-0.03, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(-0.03, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(-0.03, fitdistribution_j)

pnorm(-0.05, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(-0.05, df = 3)
plogis(-0.05, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(-0.05, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(-0.05, fitdistribution_j)


pnorm(-0.01, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(-0.01, df = 3)
plogis(-0.01, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(-0.01, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(-0.01, fitdistribution_j)

# chances of loosing greater than or at the most equal to 1% in a day for Yes Bank stock are 38.99%, So intraday is risky
# chances of loosing greater than or at the most equal to 3% in a day for Yes Bank stock are 20.20%, So intraday is risky
# Chances of loosing greater than or at the most equal to 5% in a day for Yes Bank stock are 10.72%



pnorm(0.01, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(0.01, df = 3)
plogis(0.01, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(0.01, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(0.01, fitdistribution_j)

pnorm(0.03, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(0.03, df = 3)
plogis(0.03, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(0.03, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(0.03, fitdistribution_j)

pnorm(0.05, mean = fitdistribution$estimate[1], sd = fitdistribution$estimate[2])
pt(0.05, df = 3)
plogis(0.05, location = fitdistribution_l$estimate[1], scale = fitdistribution_l$estimate[2])
pcauchy(0.05, location = fitdistribution_c$estimate[1], scale = fitdistribution_c$estimate[2])
pJohnson(0.05, fitdistribution_j)

pJohnson(0.00, fitdistribution_j)
# 0.5212541


# Chances of gain remaining greater than 1% in a day for Yes Bank stock are 100-65.42% = 34.68%
# Chances of gain remaining greater than 3% in a day for Yes Bank stock are 100-84.19% = 15.81%
# Chances of gain remaining greater than 5% in a day for Yes Bank stock are 100-92.68% = 07.32%
# forecasting the financial timeseries
# There can be some autocorrelation among the data in that case the distribution fitted will not be reliable. So we need to separate the White Noise(Fit Distribution) and Predictable Part()

# Randomness Tests Runs Tests
?randtests::runs.test
randtests::runs.test(src2$Close.Price, alternative = "two.sided", plot = TRUE)
randtests::runs.test(src2$ccreturn, alternative = "two.sided", plot = TRUE)

# Test of Unit root
plot(src2$Close.Price, type = "l")
plot(src2$ccreturn, type = "l")

# Augmented Dickey Fuller Test - check if stationary - stats for current data chunk same as other data chunks e.g. mean, variance
summary(ur.df(src2$Close.Price, lags = 20, selectlags = "AIC"))
summary(ur.df(src2$ccreturn, lags = 20, selectlags = "AIC"))
# Here we reject the H0
summary(ur.kpss(src2$Close.Price, type = "mu", lags = "short"))
summary(ur.kpss(src2$ccreturn, type = "mu", lags = "short"))

# with both df and kpss test we are getting that ccreturn are stationary but Close.Price are not stationary.
# Test of Serial Correlation
# Autocorrelation functions
# ----------------- ---------------- Try after making it stationary-------------------- ------------------acf(src2$Close.Price)
acf(src2$ccreturn)
# looks like both Close.Price and ccreturn are whitenoise no autocorrelation found

# Partial Autocorrelation function
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ pacf(src2$Close.Price)
pacf(src2$ccreturn)
# In case of return autocorrelation found in lag 18. So, its not a white noise.

# Ljung-Box test for autocorrelation
Box.test(src2$ccreturn, lag = 20, type = "Ljung-Box")
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ Box.test(src2$Close.Price, lag = 20, type = "Ljung-Box")
# In Case of Close.Price the p-value is 2.2e-16 i.e. there exists some autocorrelation since we can reject Ho Null Hypothesis.


# Durbin Watson Test for finding autocorrelation
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ dwtest(as.vector(src2$Close.Price)~1, alternative = "two.sided")
dwtest(as.vector(src2$ccreturn)~1, alternative = "two.sided", iterations = 200)
# so once again Close.Price has autocorrelation but ccreturns don't have. So for returns we have option of plotting distribution only.
# for price we can try different models.

#ARIMA Tests
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ a_closingprice_kpss<-auto.arima(src2$Close.Price, max.p = 10, max.q = 10, max.order = 20, ic = "aic", test = "kpss")
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ a_closingprice_kpss
a_return_kpss <- auto.arima(src2$ccreturn, max.p = 10, max.q = 10, max.order = 20, ic = "aic", test = "kpss")
a_return_kpss
# from auto.arima there is no lag which will affect the tomorrow's stock price of YES Bank. We need to do augmented dickey fuller test also on returns
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ a_closingprice_adf <- auto.arima(src2$Close.Price, max.p = 10, max.q = 10, max.order = 20, ic = "aic", test = "adf")
a_return_adf <- auto.arima(src2$ccreturn, max.p = 10, max.q = 10, max.order = 20, ic = "aic", test = "adf")
a_return_adf
# Now its clear that we can predict future one day at a time can predict by using mean i.e. -0.0044 and this term is significant
# because if we divide correlation with std dev we are getting value 1.96 approx so we can't reject Ho, hence we have to consider
# that mean parameter is significant
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ a_closingprice_adf

par(mar = c(3,3,3,3))
# ----------------- ---------------- Try after making it stationary-------------------- ------------------ Box.test(a_closingprice_kpss$residuals,lag= 20,type = "Ljung-Box")
Box.test(a_return_kpss$residuals,lag= 20,type = "Ljung-Box")
# So, there is no AC now in the residuals that is our model able to capture the serial correlation well.

# tsdiag(a_closingprice_kpss)
tsdiag(a_return_kpss)

# In the diagram it is clear that there is no serial correlation left in the residuals So that is good that we are able to predict most part from the time series.
# For the remaining random part we may use distribution
a_return_kpss$coef[1]

a_arima_returns = arima(src2$ccreturn, order = c(0,0,0))
a_arima_returns
a_arima_returns$coef[1]
lines(fitted(a_arima_returns), col = "blue")
accuracy(a_arima_returns)
predict(a_arima_returns,1)$pred[1]
# forecast(a_arima_returns, n.ahead = 3)
forecast(a_arima_returns, lead = 3)
# Since there is some error in prediction we can notice using forecast method result interpretation 95% Confidance interval 
# is is giving too big a range.

# Residuals after fitting of this ARIMA model which has only mean

# result for window size
# This is for model validation where in first we find the window size which is best suited and the use it to find the residuals and 
# compare with actuals

# find_winsize <- function(x) {
# winres <- c()
# l<-NA
# m<-NA
# winres$winsize <- NA
# winres$p_val <- NA
# winres = as.data.frame(winres)
# k <- 0
# for(j in 200:350){
# l <- c(l,j)
# k = k + 1
# windowsize = j
# no_iterations <- nrow(src2)-windowsize
# no_iterations
# src3<- src2
# for (i in 1:no_iterations){
#   aa<-auto.arima(src2$ccreturn[i:i+windowsize], max.p = 20, max.q = 20, max.order = 40, ic = "aic", test = "adf")
#   src3$Forecast[i] <- predict(aa,1)$pred[1]
# }
# # winres$winsize[k] <- l
# src3$Residuals <- src3$ccreturn - src3$Forecast
# temp <- Box.test(src3$Residuals, lag = 20, type = "Ljung-Box")
# m <- c(m,temp$p.value)
# }
# winres <- cbind(l,m)
# colnames(winres) <- c('WindowSize', 'p_Value')
# return(winres)
# }

# winres <- find_winsize(x)
# winres <- na.omit(winres)
# winres[winres[,2] == max(winres[,2], na.rm=T),]


# windowsize = max(winres[,2])
windowsize = 240
no_iterations <- nrow(src2)-windowsize
src3 <- src2
for (i in 1:no_iterations){
  aa<-auto.arima(src2$ccreturn[i:i+windowsize], max.p = 20, max.q = 20, max.order = 40, ic = "aic", test = "adf")
  src3$Forecast[i] <- predict(aa,1)$pred[1]
}
aa
src3$Residuals <- src3$ccreturn-src3$Forecast

head(src3)

mean(src3$Residuals)
mean(src3$Residuals^2)
Box.test(src3$Residuals, lag = 20, type = "Ljung-Box")
Box.test(src3$Residuals^2, lag = 20, type = "Ljung-Box")
# require("sos")
# findFn("Box.test")
max(src2$Date)

tail(src3)


# Various methods to estimate volatility
# ARima was to estimate mean and GARCH is estimate of variance or volatility
# Back testing
# Since the residuals squares are not normal but having serial correlation we can' use ARIMA (mainly becoz not normal). We use GARCH models

# GARCH Model
Box.test(src3$Residuals^2,type = "Ljung-Box",lag = 20)

# Since arch.test takes only arima object as parameter and not auto.arima we will call arima now
# aa<-auto.arima(src2$ccreturn[i:i+windowsize], max.p = 20, max.q = 20, max.order = 40, ic = "aic", test = "adf")
a <- arima(x = src2$ccreturn, order = c(0,0,0))
arch.test(a)
# Portmanteau  - Q test we accept Ho because p>|t| i.e. the residuals are whitenoise
# LM test we are rejecting Ho and accepting Ha i.e. the residuals are wn.

sd(src3$ccreturn)
# this is sd that will keep on varying with time - heteroskedasticity

#Specification of an appropriate GARCH Model
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "norm")
# Fit spec to data
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit

sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')








# Spec for standard garch with t-distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "std")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')









# # Spec for exponential garch with normal distribution of residuals
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "norm")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')







# # Spec for exponential garch with T distribution of residuals
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "std")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')








# Standard Garch with Johnson Distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "jsu")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')












# Exponential Garch with Johnson Distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "jsu")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')











# Standard Garch with Generalized Error Distribution GED
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "ged")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')











# Exponential GARCH with Generalized Error Distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "ged")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')


# GJR GARCH with T Distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "std")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')



# GJR GARCH with Skewed T Distribution
YesBank_ret_spec<-ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = "sstd")
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)                   
plot(YesBank_ret_garch11_fit@fit$sigma, type = 'l')

fitresiduals <- function(){
  fitgarchtoresiduals <- data.frame()
  varmodel_distrimodel <- list()
  AIC_value <- list()
  ct<-0
  # for(varmodel in c('sGARCH', 'eGARCH', 'gjrGARCH', 'apARCH','iGARCH')){
  #   for(distrmodel in c('norm','snorm','std','sstd','ged','sged','nig','ghyp' ,'jsu','gamlss')){
  for(varmodel in c('sGARCH', 'eGARCH', 'gjrGARCH', 'apARCH')){
    for(distrmodel in c('norm','snorm','std','sstd','ged','sged','jsu')){
      ct <- ct+1
      YesBank_ret_spec<-NULL
      YesBank_ret_garch11_fit<-NULL
      print(paste(ct,". ",varmodel,"-",distrmodel))
      
      
      YesBank_ret_spec <- ugarchspec(variance.model = list(model = varmodel, garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = distrmodel)
      YesBank_ret_spec
      YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
      varmodel_distrimodel <- c(varmodel_distrimodel,paste(varmodel,'-',distrmodel))
      AIC_value <- c(AIC_value,infocriteria(YesBank_ret_garch11_fit)[1])
      # if(YesBank_ret_garch11_fit == NULL){
      # AIC_value <- c(AIC_value,9999)    }else{
      # AIC_value <- c(AIC_value,infocriteria(YesBank_ret_garch11_fit)[1])}
    }
  }
  result_AIC<-cbind(varmodel_distrimodel,AIC_value)
  results <- as.data.frame(result_AIC)
  print(paste(varmodel))
  print(paste(distrmodel))
  return(results)
}

results<-fitresiduals()

results
# gjrGARCH-norm has least AIC
YesBank_ret_spec<-ugarchspec(variance.model = list(model = 'gjrGARCH', garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0)),distribution.model = 'norm')
YesBank_ret_garch11_fit<-ugarchfit(YesBank_ret_spec,data = src3$Residuals)
YesBank_ret_garch11_fit
sqrt(uncvariance(YesBank_ret_garch11_fit))
persistence(YesBank_ret_garch11_fit)
halflife(YesBank_ret_garch11_fit)

YesBank_ret_garch11_forecast<-ugarchforecast(YesBank_ret_garch11_fit,n.ahead = 10)  
YesBank_ret_garch11_forecast

# find long term average volatility of yes bank
sqrt(uncvariance(YesBank_ret_garch11_fit))
abline(h = sqrt(uncvariance(YesBank_ret_garch11_fit)), col="blue", lwd=3, lty=1)

# when volatility goes above long term average volatility i.e. above sqrt(uncvariance(YesBank_ret_garch11_fit)) then it tries to come back towards ltav
# when volatility goes below ltav then it tries to increase and reach to ltav. Mean reverting level its called.
# We can also find persistent level i.e. how much day 1 depend on day 0 
# how much day 2 depend on day 0
# how much day 3 depend on day 0
# Say persistent level is 0.9848503 Then for day 2 on day 0 it will be 0.9848503^2


# Now we have to find value at risk with this volatility i.e. with apARCH variance model and distribution model is student t distribution


fitnorm<-fitdistr(src2$ccreturn,"normal")
fitnorm

goftest::ad.test(src2$ccreturn,"pnorm",fitnorm$estimate[1],fitnorm$estimate[2])
# Since the distribution is not normal and from above we know that ccreturns follow Johnson DistributionDistributionUtils
# So, Monte Carlo simulation is done using Johnson distribution
fitdistribution_j <- JohnsonFit(src2$ccreturn)
fitdistribution_j
# hist(src2$ccreturn, breaks = 30, freq = FALSE)
# curve(dJohnson(x, fitdistribution_j), from = -0.08, to = 0.11, add = TRUE, col = "Orange")
# ks.test(as.vector(src2$ccreturn), "pJohnson", fitdistribution_j)
# goftest::ad.test(src2$ccreturn, "pJohnson",fitdistribution_j)

rm(montecarlo_johnson)
montecarlo_johnson <- function(datalist) {
  temp <- list()
  fitdistribution_j <- JohnsonFit(datalist)
  for(i in 1:5000){
    randomdata_j <- rJohnson(n = 10000, fitdistribution_j)
    n <- length(randomdata_j)
    k <- ceiling(0.99*n)
    temp <- c(temp, sort(randomdata_j,partial = k)[k])
  }
  return(temp)
}

monte <- montecarlo_johnson(src2$ccreturn)
montedf <- t(as.data.frame(monte))
head(montedf)
colnames(montedf) <- "ccreturn"
dim(montedf)

rm(fitdistributionfunction)
fitdistributionfunction <- function(ccreturn){
  lengthofdata <- length(ccreturn)
  # rm(DistrName,TestName,temp,Coeff_Corr,pValue,TestStat)
  fitdistribution <- fitdistr(ccreturn, "normal")
  DistrName <- "Normal"
  TestName <- "ks test"
  temp<-ks.test(ccreturn, "pnorm", fitdistribution$estimate[1],fitdistribution$estimate[2])
  TestStat <- temp[1]
  pValue <- temp[2]
  DistrName <- c("Normal",DistrName)
  TestName <- c("ad test",TestName)
  temp <- goftest::ad.test(as.vector(ccreturn), "pnorm", fitdistribution$estimate[1],fitdistribution$estimate[2])
  TestStat <- c(temp[1],TestStat)
  pValue <- c(temp[2],pValue)
  randomdata <- rnorm(lengthofdata, fitdistribution$estimate[1], fitdistribution$estimate[2])
  Coeff_Corr <- cor(sort(as.vector(ccreturn)),sort(randomdata))
  Coeff_Corr <- c(cor(sort(as.vector(ccreturn)),sort(randomdata)), Coeff_Corr)
  
  
  # Try fitting students t distribution to the ccreturn
  fitdistribution_t <- fitdistr(ccreturn, "t", start = list(m= mean(ccreturn), s = sd(ccreturn), df = 3), lower = c(-1, 0.001, 1))
  DistrName <- c("t-Distr", DistrName)
  TestName <- c("ks test"  ,TestName)
  temp <- ks.test(as.vector(ccreturn), "pt", df = fitdistribution_t$estimate[3], fitdistribution_t$estimate[2])
  TestStat <- c(temp[1],TestStat)
  pValue <- c(temp[2],pValue)
  DistrName <- c("t-Distr",DistrName)
  TestName <- c("ad test", TestName)
  temp <- goftest::ad.test(as.vector(ccreturn), "pt", df = fitdistribution_t$estimate[3], fitdistribution_t$estimate[2])
  TestStat <- c(temp[1],TestStat)
  pValue <- c(temp[2],pValue)
  randomdata_t <- rnorm(n = lengthofdata, fitdistribution_t$estimate[1], fitdistribution_t$estimate[2])
  Coeff_Corr<-c(cor(sort(as.vector(ccreturn)),sort(randomdata_t)),Coeff_Corr)
  Coeff_Corr<-c(cor(sort(as.vector(ccreturn)),sort(randomdata_t)),Coeff_Corr)
  
  
  # Try fitting Cauchy's Distribution
  fitdistribution_c <- fitdistr(ccreturn, "cauchy")
  DistrName <- c("Cauchy's", DistrName)
  TestName <- c("ks test", TestName)
  temp <- ks.test(as.vector(ccreturn), "pcauchy", fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2],pValue)
  DistrName <- c("Cauchy's", DistrName)
  TestName <- c("ad test", TestName)
  temp <- goftest::ad.test(as.vector(ccreturn), "pcauchy", fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2], pValue)
  randomdata_c <- rnorm(n = lengthofdata, fitdistribution_c$estimate[1], fitdistribution_c$estimate[2])
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_c)), Coeff_Corr)
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_c)), Coeff_Corr)
  
  
  # Logistic Distribution
  fitdistribution_l = fitdistr(ccreturn, "logistic")
  DistrName <- c("Logistic", DistrName)
  TestName <- c("ks test", TestName)
  temp <- ks.test(as.vector(ccreturn), "plogis", fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2], pValue)
  DistrName <- c("Logistic", DistrName)
  TestName <- c("ad test", TestName)
  temp<-goftest::ad.test(as.vector(ccreturn), "plogis", fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2], pValue)
  randomdata_l <- rlogis(n= lengthofdata, fitdistribution_l$estimate[1], fitdistribution_l$estimate[2])
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_l)),Coeff_Corr)
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_l)),Coeff_Corr)
  
  # Johnson Distribution
  fitdistribution_j <- JohnsonFit(ccreturn)
  DistrName <- c("Johnson's", DistrName)
  TestName <- c("ks test", TestName)
  temp <- ks.test(as.vector(ccreturn), "pJohnson", fitdistribution_j)
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2], pValue)
  temp <- goftest::ad.test(ccreturn, "pJohnson",fitdistribution_j)
  DistrName <- c("Johnson's", DistrName)
  TestName <- c("ad test", TestName)
  TestStat <- c(temp[1], TestStat)
  pValue <- c(temp[2], pValue)
  randomdata_j <- rJohnson(n = lengthofdata, fitdistribution_j)
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_j)), Coeff_Corr)
  Coeff_Corr <- c(cor(sort(ccreturn), sort(randomdata_j)), Coeff_Corr)
  results <- cbind(as.numeric(TestStat), as.numeric(pValue), TestName, DistrName, Coeff_Corr)
  colnames(results)[1] <- "TestStat"
  colnames(results)[2] <- "pValue"
  return(results)
}

res <- fitdistributionfunction(montedf)
warnings()
res

# From above function we found that distribution of means is normal.
head(src2)
fitnorm<-fitdistr(src2$ccreturn,"normal")
goftest::ad.test(src2$ccreturn,"pnorm",fitnorm$estimate[1],fitnorm$estimate[2])
normVaR<-qnorm(0.05,fitnorm$estimate[1],fitnorm$estimate[2])
normVaR

NormalVaR(returns = src2$ccreturn, cl = 0.95, hp = 10)
NormalVaR(returns = src2$ccreturn, cl = 0.99, hp = 1)
NormalES(returns = src2$ccreturn, cl = 0.95, hp = 1)
NormalES(returns = src2$ccreturn, cl = 0.99, hp = 1)

rollingNormalVaR95<-rollapply(src2$ccreturn,250,function(x)qnorm(0.05,fitdistr(x,"normal")$estimate[1],fitdistr(x,"normal")$estimate[2]))
write.csv(rollingNormalVaR95,"C:/Users/sgupta7/Documents/Bits/5th semester/project/NormalVaR95.csv")
rollingNormalVaR95

rollingNormalVaR99<-rollapply(src2$ccreturn,250,function(x)qnorm(0.01,fitdistr(x,"normal")$estimate[1],fitdistr(x,"normal")$estimate[2]))
write.csv(rollingNormalVaR99,"C:/Users/sgupta7/Documents/Bits/5th semester/project/NormalVaR99.csv")
plot(-rollingNormalVaR99)
install.packages("GAS", dependencies = TRUE)
library(GAS)
BacktestVaR(YesBank_ret_garch11_fit@fit$residuals[250:493], rollingNormalVaR95, alpha = 0.01, Lags = 4)

# Has to be modified with appropriate data:
# Christoffersen Backtest For Independence for given parameters
a <- rnorm(1*100)
b <- abs(rnorm(1*100))+2
ChristoffersenBacktestForIndependence(a, b, 0.95)
?ChristoffersenBacktestForIndependence




findFn('BacktestVaR')

HSVaR(YesBank_ret_garch11_fit@fit$residuals,c(0.95,0.99))
HSES(YesBank_ret_garch11_fit@fit$residuals,0.95)
HSES(YesBank_ret_garch11_fit@fit$residuals,0.99)
rollingHSVaR<-rollapply(Yesbank_ret_garch11_fit@fit$residuals,250,function(x)HSVaR(x,c(0.95,0.99)))
write.csv(rollingHSVaR,"D:/BITS/2019S1/HSVaR.csv")   

x<-ugarchroll(YesBank_ret_spec,data = YesBank_StockPrice$Returns,n.ahead = 1,forecast.length = 243,refit.every = 1,refit.window = c("moving"),calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05))
BacktestVaR(YesBank_StockPrice$Returns[251:493],x@forecast$VaR[,1],0.01,4)
x@forecast$VaR[,1]

library(evd)
library(fitdistrplus)
YesBank_Losses<-src2$ccreturn*(-1)
# Threshold
s1<-sum(YesBank_Losses>0.05)
s2<-sum(YesBank_Losses>0.04)
s3<-sum(YesBank_Losses>0.02)


gpd1<-fitdist(as.vector(YesBank_Losses[YesBank_Losses>0.02]),distr = "gpd",start = list(scale=0.01, shape=0),method = "mle")
length(YesBank_Losses)
gpd1$estimate[1]
gpd1$estimate[2]

probTCSLossGT3<-s3/length(YesBank_Losses)
condProb<-(1+gpd1$estimate[2]*(0.07-0.02)/gpd1$estimate[1])^(-1/gpd1$estimate[2])
uncondProb<-probTCSLossGT3*condProb
Var99<-0.02+(gpd1$estimate[1]/gpd1$estimate[2])*((1/probTCSLossGT3*(0.01))^(-gpd1$estimate[2])-1)

#VaR and ES 
# library(Dowd)
GPDVaR95<-GParetoVaR(YesBank_Losses,gpd1$estimate[1],gpd1$estimate[2],probTCSLossGT3,0.95)
GPDVaR99<-GParetoVaR(YesBank_Losses,gpd1$estimate[1],gpd1$estimate[2],probTCSLossGT3,0.99)
GPDES95<-GParetoES(YesBank_Losses,gpd1$estimate[1],gpd1$estimate[2],probTCSLossGT3,0.95)
GPDVaRES<-GParetoES(YesBank_Losses,gpd1$estimate[1],gpd1$estimate[2],probTCSLossGT3,0.99)
NormalVaR(returns = YesBank_Losses,cl = 0.99,hp = 1)





'''
%Y: 4-digit year (1982)
%y: 2-digit year (82)
%m: 2-digit month (01)
%d: 2-digit day of the month (13)
%A: weekday (Wednesday)
%a: abbreviated weekday (Wed)
%B: month (January)
%b: abbreviated month (Jan)
'''