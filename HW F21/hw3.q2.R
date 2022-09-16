## STAT 2131 - HW3 - Kehui, Fall 2021
#Question 2
setwd("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/Fall21/HW")
stock <- read.delim("stock.txt", header = T, sep = "")

#a
m <- lm(Months ~ Size + type + Size:type, data = stock)
s <- summary(m)
s
## (i) beta1^, (ii) beta1^ + beta3^, (iii) beta2^ + beta3^*X1

#b
m.reduced <- lm(Months ~ Size + type , data = stock)
anova(m.reduced, m)
#FTR H0, can use reduced model, no need for interaction

#c
summary(m.reduced)
## (i) beta1^, (ii) beta1^, (iii) beta2^