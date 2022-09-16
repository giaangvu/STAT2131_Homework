### 2131 Kehui - Fall 21
## HW1 - Q4

#real beta = 2
x <- 1:50 / 100
e <- rnorm(50)
y <- 1 + 2*x + e 
#a - mean and var of beta1 based on theory
beta.mean <- 2
beta.var <- 1/sum((x-mean(x))^2)


#loop
beta <- rep(0,100)
cover <- 0
for (j in 1:100){
  set.seed(j)
  x <- 1:50 / 100
  e <- rnorm(50)
  y <- 1 + 2*x + e 
  m <- lm(y~x)
  s <- summary(m)
  beta[j] <- s$coefficients[2,1]
  lowerCI <- s$coefficients[2,1] - s$coefficients[2,2]*qt(0.975,df=48)
  upperCI <- s$coefficients[2,1] + s$coefficients[2,2]*qt(0.975,df=48)
  if(2 > lowerCI & 2 < upperCI){cover <- cover + 1} #see if real beta of 2 falls into CIs
}

#b
hist(beta)
mean(beta)
sd(beta)
var(beta)
#c
cover/100

