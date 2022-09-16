library(ggrepel)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(readxl)
library(DBI)
library(odbc)
library(readr)
library(writexl)
library(knitr)
library(clipr)
library(car)
install.packages("MASS")
library(MASS)
library(np)

hw8_dt <- read.delim("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw8/Gambling.txt")

#(a) Regress gamble onto the other four predictors. 
#Do you see any evidence that the mean model or constant variance assumption is violated? Yes
#Are the errors normally distributed? No, skewed, look at histogram and qq plot

mod2a <- lm(gamble ~.,data = hw8_dt)
sum2a <- summary(mod2a)
plot(predict(mod2a), resid(mod2a),
     ylab  = "Residuals", xlab = "Predicted Values", main ="Residual Plot 1")
abline(0,0)

plot(hw8_dt$sex, resid(mod2a),
     ylab  = "Residuals", xlab = "Sex", main ="Residual Plot 2")
abline(0,0)

plot(hw8_dt$status, resid(mod2a),
     ylab  = "Residuals", xlab = "Status", main ="Residual Plot 3")
abline(0,0)

plot(hw8_dt$income, resid(mod2a),
     ylab  = "Residuals", xlab = "Income", main ="Residual Plot 4")
abline(0,0)

plot(hw8_dt$verbal, resid(mod2a),
     ylab  = "Residuals", xlab = "Verbal", main ="Residual Plot 5")
abline(0,0)

qqnorm(rstandard(mod2a), 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(rstandard(mod2a))

hist(resid(mod2a))

#b
#add small amount to gamble to make box cox work
hw8_dt$Y <- hw8_dt$gamble + 10**(-8)
bxcx2b <- boxcox(lm(Y~sex+status+income+verbal,data = hw8_dt[,-5]),plotit = T)
#looks like lambda  = 0.2
lambda <- 0.2
hw8_dt$Y_tilde <- (hw8_dt$Y^lambda - 1)/lambda
mod2b <- lm(Y_tilde~.,data = hw8_dt[,-c(5,6)])
sm2b <- summary(mod2b) 

plot(predict(mod2b), resid(mod2b),
     ylab  = "Residuals", xlab = "Predicted Values", main ="Residual Plot 1 BoxCox")
abline(0,0)

plot(hw8_dt$sex, resid(mod2b),
     ylab  = "Residuals", xlab = "Sex", main ="Residual Plot 2 BoxCox")
abline(0,0)

plot(hw8_dt$status, resid(mod2b),
     ylab  = "Residuals", xlab = "Status", main ="Residual Plot 3 BoxCox")
abline(0,0)

plot(hw8_dt$income, resid(mod2b),
     ylab  = "Residuals", xlab = "Income", main ="Residual Plot 4 BoxCox")
abline(0,0)

plot(hw8_dt$verbal, resid(mod2b),
     ylab  = "Residuals", xlab = "Verbal", main ="Residual Plot 5 BoxCox")
abline(0,0)

qqnorm(rstandard(mod2b), 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(rstandard(mod2b))

hist(resid(mod2b))
hist(hw8_dt$Y_tilde)
#new model seems to satisfy assumptions

#c
hw8_x <-as.matrix(hw8_dt[,-c(5,6,7)])
H8c <- hw8_x %*% solve(t(hw8_x)%*%hw8_x) %*% t(hw8_x)
hist(diag(H8c))
which(diag(H8c)>(2*4/47))
#there are some abnormally large leverage points here
#those points correspond to 33th, 42th observations
#now refit model in b without those points
mod2c <- lm(Y_tilde~.,data = hw8_dt[-c(33,42),-c(5,6)])
#coefficients don't change much
sm2c <- summary(mod2c)
#std error dont seem to change too much either

#d
cooks.distance(mod2b)
plot(cooks.distance(mod2b))
which(cooks.distance(mod2b)>(4/(41-4)))
#24th, 39th obs for model a
#20th ob for model b


#3c
dat3c <- read.csv("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw8/water.csv",sep = ",")
plot(dat3c$yr,dat3c$freq,main = 'Frequency of the word \'water\' in printed sources over time',
     xlab = 'Year',ylab = 'Frequency')
#What smoothing should we use?#
smooth.fraction <- 0.5   #Proportion of total points that are used in each fit
degree.poly <- 1   #Degree of locally fitted polynomial (0 = local mean, 1 = linear, 2 = quadratic, etc.). Adjust smooth.fraction and degree.poly to see how the fit changes

loess.mod3c <- loess(freq~yr, data = dat3c, span = smooth.fraction, degree = degree.poly)   #Fit loess smoother

x.new3c <- seq(1800,2000,by=0.01)   #Points at which to estimate function
predict.new3c <- predict(object = loess.mod3c, newdata = data.frame(yr=x.new3c))
lines(x.new3c, predict.new3c, col="orange")   #Plot fitted function

kern.3c <- npreg(freq~yr,data = dat3c) #h = 2.016924 for gaussian
kern.3c1 <- npreg(freq~yr,data = dat3c,ckertype='epanechnikov') #1.383307 for epa

kern.3c <- density(x=dat3c$yr,kernel = "gaussian") #h = 19.48 for gaussian
kern.3c1 <- density(x=dat3c$yr,kernel = "epanechnikov")#h = 19.48 for epa
kern.3c2 <- density(x=dat3c$yr,kernel = "cosine") #h = 19.48 for cosine

ksm1 <- ksmooth(x=dat3c$yr,y=dat3c$freq,kernel = 'normal',bandwidth = kern.3c$bw)
lines(ksm1$x,ksm1$y,col="red")
ksm2 <- ksmooth(x=dat3c$yr,y=dat3c$freq,kernel = 'normal',bandwidth = kern.3c1$bw)
lines(ksm2$x,ksm2$y,col="green")
