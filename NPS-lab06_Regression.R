## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------
library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)


## .extracode {

## background-color: lightblue;

## }


## -----------------------------------------------------------------------------------------------------------------------------------

set.seed(1992)
n <- 30
B=1000


## -----------------------------------------------------------------------------------------------------------------------------------
# covariate values
x1 <- runif(n,0,10)
x2 <- (1:n)/5
x3 <- rnorm(n,5,5)


# generating model
b0 <- 2
b1 <- 3
b2 <- -2
b3 <- 0
Y <- b0 + b1*x1 + b2*x2 + b3*x3 + stabledist::rstable(n,1.2,0)




## -----------------------------------------------------------------------------------------------------------------------------------
plot(x1,Y,pch=16)
plot(x2,Y,pch=16)
plot(x3,Y,pch=16)



## -----------------------------------------------------------------------------------------------------------------------------------
# parametric inference
result <- lm(Y ~ x1 + x2 + x3)
summary(result)




## -----------------------------------------------------------------------------------------------------------------------------------
T0_glob <- summary(result)$f[1]
T0_glob


## -----------------------------------------------------------------------------------------------------------------------------------
T_H0glob <- numeric(B)

for(perm in 1:B){
  permutazione <- sample(n)
  
  Y.perm.glob <- Y[permutazione]
  T_H0glob[perm] <- summary(lm(Y.perm.glob ~ x1 + x2 + x3))$f[1]
  
}

sum(T_H0glob>=T0_glob)/B


## -----------------------------------------------------------------------------------------------------------------------------------
T0_x1 <- abs(summary(result)$coefficients[2,3])
T0_x1

T0_x2 <- abs(summary(result)$coefficients[3,3])
T0_x2

T0_x3 <- abs(summary(result)$coefficients[4,3])
T0_x3




## -----------------------------------------------------------------------------------------------------------------------------------

regr.H01 <- lm(Y ~ x2 + x3)
residui.H01 <- regr.H01$residuals

regr.H02 <- lm(Y ~ x1 + x3)
residui.H02 <- regr.H02$residuals

regr.H03 <- lm(Y ~ x1 + x2)
residui.H03 <- regr.H03$residuals




## -----------------------------------------------------------------------------------------------------------------------------------
 T_H01 <- T_H02 <- T_H03 <- numeric(B)

for(perm in 1:B){
  permutazione <- sample(n)
  
  residui.H01.perm <- residui.H01[permutazione]
  Y.perm.H01 <- regr.H01$fitted + residui.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ x1 + x2 + x3))$coefficients[2,3])
  
  residui.H02.perm <- residui.H02[permutazione]
  Y.perm.H02 <- regr.H02$fitted + residui.H02.perm
  T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ x1 + x2 + x3))$coefficients[3,3])
  
  residui.H03.perm <- residui.H03[permutazione]
  Y.perm.H03 <- regr.H03$fitted + residui.H03.perm
  T_H03[perm] <- abs(summary(lm(Y.perm.H03 ~ x1 + x2 + x3))$coefficients[4,3])
  
}

sum(T_H01>=T0_x1)/B
sum(T_H02>=T0_x2)/B
sum(T_H03>=T0_x3)/B



