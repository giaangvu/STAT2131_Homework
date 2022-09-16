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

#read data
hw7 <- read.csv("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw7/hw7.3data.csv",sep = ",")

#6.10
#a
mod73 <- lm(Y ~ X1 + X2 + X3, data = hw7)
sum73 <- summary(mod73)
sum73

#c
#residuals against Yh
plot(predict(mod73), resid(mod73),
     ylab  = "Residuals", xlab = "Predicted Values", main ="Residuals against Yh")
abline(0,0)

#residuals against X1
plot(hw7$X1, resid(mod73),
     ylab  = "Residuals", xlab = "X1", main ="Residuals against X1")
abline(0,0)

#residuals against X2
plot(hw7$X2, resid(mod73),
     ylab  = "Residuals", xlab = "X2", main ="Residuals against X2")
abline(0,0)

#Residuals against X3
plot(hw7$X3, resid(mod73),
     ylab  = "Residuals", xlab = "X3", main ="Residuals against X3")
abline(0,0)

#Residuals against X1*X2
plot(hw7$X1*hw7$X2, resid(mod73),
     ylab  = "Residuals", xlab = "X1*X2", main ="Residuals against X1*X2")
abline(0,0)

#normal prob plot
qqnorm(rstandard(mod73), 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(rstandard(mod73))

#7.4) b) P value = 0.5712 > 0.05, fail to reject H0
linearHypothesis(mod73,c("X2=0"))
mod73_r <- lm(Y ~ X1 + X3, data = hw7)
anova(mod73_r, mod73)

#6.14)
#read data
hw7.4 <- read.csv("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw7/hw7.4data.csv",sep=",")
#a)
mod74 <- lm(Y ~ X1 + X2 + X3, data = hw7.4)
sum74 <- summary(mod74)
sum74
linearHypothesis(mod74,c("X1=0","X2=0","X3=0"))
#reject H0
#b) the only CI that doesnt contain 0 is that of X1 => might be the only significant predictor
confint(mod74,level=1-0.10/(2*3))
#c)
