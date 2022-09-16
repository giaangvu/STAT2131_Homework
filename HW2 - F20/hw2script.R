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

hw2_data <- read.csv("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw2/hw2_data.csv",header = T,sep = ",")

#1.22 KNNL
#a
hw2_model <- lm(Y ~ X, data = hw2_data)
sm <- summary(hw2_model)
sm
anova(hw2_model)
#plot
plot(Y ~ X, data = hw2_data)
abline(hw2_model)

#mse
mse <- function(sm){ 
  sum(sm$residuals^2)/(nrow(hw2_data)-2)
}
mse_hw2 <- mse(sm)
#s^2{Yh}
xbar <- mean(hw2_data$X) #mean of Xi's - X bar
xbar
hw2_data$sq_diff_fr_mean <- (hw2_data$X - xbar)**2 
xsum_sqr <- sum(hw2_data$sq_diff_fr_mean) #sum of (Xi - Xbar)^2 for i = 1,...,n
xsum_sqr
#with a new point Xh = 30, we calculate the s^2{Y} as follows
sYh <- sqrt(mse_hw2/nrow(hw2_data) *(1+ (30 - xbar)^2/(xsum_sqr)))
sYh
#b
predict.lm(hw2_model,data.frame(X = 40))

#2.16 KNNL (read p.76 - 85)
#help("predict.lm") 
#a
predict.lm(hw2_model,data.frame(X = 30),interval = "confidence",level = 0.98,se.fit = T)
#b
predict.lm(hw2_model,data.frame(X = 30),interval = "prediction",level = 0.98)
#c
nudata <- data.frame(X = matrix(30, nrow = 10))
predict.lm(hw2_model,nudata,interval = "prediction",level = 0.98)
#d
#e



