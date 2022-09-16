#Kehui STAT2131 - HW5

#Q1
library(olsrr)
job <- read.table("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/Fall21/HW/CH09PR10.txt",
                  header = T)

##a
plot(job, main="scatterplot") #scatter plot
cor(job[,-1]) #corr matrix of X's
#Y vs X's have linear positive relationship
#based on how big the correlation values are:
#X1 & X2, X1 & X3 have slight corr 
#X1 & X4, X2 & X4 have slight/moderate corr
#X2 & X3, X3 & X4 have moderate/strong corr

##b
full.model <- lm(Y~., data = job)
summary(full.model)
#x2 is not stat signi

##c
best.subset <- ols_step_best_subset(full.model)
best.subset
#based on adj R^2
which.max(best.subset$adjr) #model with X1, X3, X4
#find what vars are included
predictors.include <- strsplit(best.subset$predictors[which.max(best.subset$adjr)], "[ ]+", perl=T)[[1]]
#find what vars are left out
colnames(job)[!(colnames(job)%in%predictors.include)]

#based on AIC
which.min(best.subset$aic) #same model as adj R^2

##d
alpha <- 0.05
forward <- ols_step_forward_p(full.model, penter = alpha)

alpha <- 0.1
forward <- ols_step_forward_p(full.model, penter = alpha)
forward

##e
#Forward step wise (both alpha vlues) lead to the same model as Adj R^2 and AIC
#We chose model with only X1, X3, X4
