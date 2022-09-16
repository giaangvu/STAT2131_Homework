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

hw6_dt <- read.delim("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw6/steam_text-1.txt")

#a) regress steam (Y) onto fat (X1) and glycerine (X2).
hw6_md <- lm(steam ~ fat + glycerine, data = hw6_dt)
#i)
hw6_sm<-summary(hw6_md) #mean model
hw6_anova <- summary(aov(hw6_md)) #variance model

#ii) plot residual as fcn of Y^, fat, glycerine
plot(predict(hw6_md), resid(hw6_md),
     ylab  = "Residuals", xlab = "Predicted Values", main ="Residual Plot 1")
abline(0,0)

plot(hw6_dt$fat, resid(hw6_md),
     ylab  = "Residuals", xlab = "Fat", main ="Residual Plot 2")
abline(0,0)

plot(hw6_dt$glycerine, resid(hw6_md),
     ylab  = "Residuals", xlab = "Glycerin", main ="Residual Plot 3")
abline(0,0)
#iii) p-value for F test is 0.1197 > 0.05 => fail to reject H0, notice R square really small too
linearHypothesis(hw6_md,c("fat=0","glycerine=0")) #same result

#iv) temp against residual => there's a decreasing trend, residuals affected by temp
plot(hw6_dt$temp, resid(hw6_md),
     ylab  = "Residuals", xlab = "Temp", main ="Residual Plot 4")
abline(0,0)

#b) regress steam (Y) onto fat (X1), glycerine (X2) and temp (X3)
hw6_md2 <- lm(steam ~ fat + glycerine + temp, data = hw6_dt)
summary(hw6_md2)
#i) p value for F test = 0.0005569 < 0.05 => reject H0
linearHypothesis(hw6_md2,c("fat=0","glycerine=0"))
#ii) bc of inclusion of X3? model in a underfitting, a lot of residuals are now accounted for by temp.
#with temp included, we have a better model, higher R squared