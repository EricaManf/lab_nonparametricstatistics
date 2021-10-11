## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------
library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)


## .extracode {

## background-color: lightblue;

## }


## -----------------------------------------------------------------------------------------------------------------------------------

seed=2781991
B=10000
library(progress)


## -----------------------------------------------------------------------------------------------------------------------------------
data("Orange")
Orange



## -----------------------------------------------------------------------------------------------------------------------------------
O_3=subset(Orange,Orange$Tree==3)
attach(O_3)
#let's have a look at the data
plot(age,circumference,main='Circumference of Tree #3',xlab = 'Time (days)',ylab = 'Circumference')


## -----------------------------------------------------------------------------------------------------------------------------------
logistic=function(t,L,k,midpoint){L/(1+exp((midpoint-t)/k))}


## -----------------------------------------------------------------------------------------------------------------------------------
model=nls(circumference ~ logistic(age,L,k,midpoint) ,
          start=list(L=150, k=500, midpoint=700))

summary(model)


## -----------------------------------------------------------------------------------------------------------------------------------
age_fine=seq(0,2500)
circumference_fine=predict(model,list(age=age_fine))


## -----------------------------------------------------------------------------------------------------------------------------------
plot(age,circumference,main='Fitted curve vs data',xlim=c(0,2500),ylim=c(20,200))
lines(age_fine,circumference_fine,col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
fitted.obs=predict(model)
res.obs=circumference-fitted.obs


## -----------------------------------------------------------------------------------------------------------------------------------
L.obs = summary(model)$parameters[1,1]
T.boot.L = numeric(B)
formula.b = response.b ~ logistic(age,L,k,midpoint)

pb <- progress_bar$new(
  format = "  processing [:bar] :percent eta: :eta",
  total = B, clear = FALSE)

set.seed(seed)

for(b in 1:B) {
  
  
  response.b <- fitted.obs + sample(res.obs, replace = T)
  fm.b <- nls(formula.b ,start=list(L=150, k=500, midpoint=700))
  
  sm.b = summary(fm.b)

  T.boot.L[b] = sm.b$parameters[1,1]

  
  pb$tick()
  
}



## -----------------------------------------------------------------------------------------------------------------------------------
plot(ecdf(T.boot.L), main='Asymptote')
abline(v=L.obs, lty=2)


## -----------------------------------------------------------------------------------------------------------------------------------
sd(T.boot.L)


## -----------------------------------------------------------------------------------------------------------------------------------
mean(T.boot.L)-L.obs


## -----------------------------------------------------------------------------------------------------------------------------------
alpha <- 0.05
right.quantile.L <- quantile(T.boot.L, 1 - alpha/2)
left.quantile.L  <- quantile(T.boot.L, alpha/2)

CI.RP.L <- c(L.obs - (right.quantile.L - L.obs), L.obs - (left.quantile.L - L.obs))
names(CI.RP.L)=c('lwr','upr')
CI.RP.L



## -----------------------------------------------------------------------------------------------------------------------------------
plot(ecdf(T.boot.L), main='L')
abline(v = L.obs, lty=2)
abline(v = CI.RP.L)

plot(age,circumference,main='Fitted curve vs data',xlim=c(0,2500),ylim=c(20,200))
lines(age_fine,circumference_fine,col='red')
abline(h=L.obs, lty=2)
abline(h = CI.RP.L)

