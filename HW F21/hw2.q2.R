## STAT 2131 - HW2 - Kehui, Fall 2021
#Question 2
setwd("/Users/giangvu/Desktop/STAT 2131 - Applied Stat Methods 1/Fall21/HW")
facemem <- read.csv("FACEMEM.csv", header = T)
colnames(facemem) <- c("disease", "age", "gender", "FACEMEM")
facemem$disease <- as.factor(facemem$disease)
facemem$gender <- as.factor(facemem$gender)

#fit
m <- lm(FACEMEM ~ disease + age + gender, data = facemem)
s <- summary(m)
s

#(a) mem score est for 40-yr 
27.96497 -0.02998*40 

#(b) wald t test

#(c) bet2^ and CI
-0.02998-1.962443*0.01082
-0.02998+1.962443*0.01082

#(d) read F test results from summary

#(e) compare model full with reduced model of just X1
m.reduced <- lm(FACEMEM ~ disease, data = facemem)
anova(m, m.reduced)


#Extra from Chris's HW