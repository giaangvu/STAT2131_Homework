##3.14 KNNL
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

hw2_model <- lm(Y ~ X, data = hw2_data)
sm <- summary(hw2_model)
sm

#SSE - RESIDUALS (or SSE(R))
sse = sum((fitted(hw2_model) - hw2_data$Y)**2)

#ssr = sum((fitted(hw2_model) - mean(hw2_data$Y))**2)
#R2 = 1 - (ssr/(sse + ssr))

#SSPE or SSE(F)
hw2_data <- hw2_data %>% mutate(level=as.numeric(factor(X)))
hw2_data <- hw2_data %>% group_by(level) %>% mutate(lvl_mean=mean(Y))
hw2_data <- hw2_data %>% mutate(lvl_err_sqr=(Y-lvl_mean)**2) 
sspe <- sum(hw2_data$lvl_err_sqr)

#SSLF = SSE - SSPE
sslf <- sse - sspe

#### UPDATED ANSWER 2022!!!
#F test when X is discrete and has repeated measures (X = {16,24,32,40}, each has 4 measures)
c <- 4
n <- nrow(hw2_data)
hw2_data$X <- as.factor(hw2_data$X)
hw2_data <- hw2_data %>% group_by(X) %>% mutate(lvl_mean=mean(Y))
hw2_data$fitted <- hw2_model$fitted.values

#lack of fit (fitted - level mean)
SS.lof <- sum((hw2_data$fitted - hw2_data$lvl_mean)^2)

#pure error (observed - level mean)
SS.pe <- sum((hw2_data$Y - hw2_data$lvl_mean)^2)

#F stat
F_star <- (SS.lof/(c-2))/(SS.pe/(n-c)) 

#pval (P(F > F_star))
pf(F_star, c-2, n-c, lower.tail = F)

#very big so fail to reject the null H0: no lack of fit
#=> there is no lack of fit

##3.18 KLLN
hw3_data <- read.csv("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/HW/hw3/hw3_data.csv", header = T,sep = ",")
#a
plot(Y ~ X, data = hw3_data)
#b
hw3_data <- hw3_data %>% mutate(X2 = sqrt(X))
hw3_model <- lm(Y ~ X2, data = hw3_data)
sm3 <- summary(hw3_model)
anova(hw3_model)
#c
plot(Y ~ X2, data = hw3_data)
abline(hw3_model)
#d
plot(hw3_data$X2, resid(hw3_model),
     ylab  = "Residuals", xlab = "Square Root of Lot Size", main ="Residual Plot")
abline(0,0)
qqnorm(rstandard(hw3_model), 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(rstandard(hw3_model))
