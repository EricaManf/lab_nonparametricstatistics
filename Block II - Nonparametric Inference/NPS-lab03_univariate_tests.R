## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------
library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)


## .extracode {

## background-color: lightblue;

## }


## -----------------------------------------------------------------------------------------------------------------------------------
library(progress)


## -----------------------------------------------------------------------------------------------------------------------------------
alpha=0.05
n <- 10
B <- 1000
seed=26111992
sigma=2


## ----echo=TRUE----------------------------------------------------------------------------------------------------------------------
x.grid <- seq(-5, 5, by=0.01)
plot(x.grid, dnorm(x.grid,sd=sigma), type='l')


## ----echo=TRUE----------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B){
  x1 <- rnorm(n,sd=sigma)
  x2 = rnorm(n,sd=sigma)
  p.value[j] <- t.test(x1,y=x2)$p.value
#pb$tick()
}


## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Normal')
plot(ecdf(p.value), main = 'Normal')
abline(0,1, lty=2, col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))


## -----------------------------------------------------------------------------------------------------------------------------------
semiamp=sqrt(6)

plot(x.grid, dunif(x.grid,min=-semiamp,max=semiamp), type='l')


## -----------------------------------------------------------------------------------------------------------------------------------

p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x1 <- runif(n,min=-semiamp,max=semiamp)
  x2 =  runif(n,min=-semiamp,max=semiamp)
  p.value[j] <- t.test(x1,y=x2)$p.value
  #pb$tick()
}



## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Uniform')
plot(ecdf(p.value), main = 'Uniform')
abline(0,1, lty=2, col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))


## -----------------------------------------------------------------------------------------------------------------------------------
x.grid <- seq(-5, 5, by=0.01)
plot(x.grid, dt(x.grid, 2), type='l')
lines(x.grid, dnorm(x.grid,sd=sigma),col='red')



## -----------------------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x.1 <- rt(n, 1)
  x.2 = rt(n,1)
  p.value[j] <- t.test(x.1,y=x.2)$p.value
  #pb$tick()
}


## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Student-t')
plot(ecdf(p.value), main = 'Student-t')
abline(0,1, lty=2, col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))



## -----------------------------------------------------------------------------------------------------------------------------------
library(stabledist)


## -----------------------------------------------------------------------------------------------------------------------------------
x.grid <- seq(-5, 5, by=0.01)
plot(x.grid, dstable(x.grid,1,0), type='l') #cauchy
lines(x.grid, dstable(x.grid,2,0), type='l',col="red") #normal
lines(x.grid, dstable(x.grid,1.5,0), type='l',col="orange")
lines(x.grid, dstable(x.grid,0.5,0), type='l',col="green")
lines(x.grid, dstable(x.grid,0.1,0), type='l',col="blue")



## -----------------------------------------------------------------------------------------------------------------------------------
plot(x.grid, dstable(x.grid,1,0), type='l') #cauchy
lines(x.grid, dstable(x.grid,1,0.5), type='l',col="red")
lines(x.grid, dstable(x.grid,1,1), type='l',col="orange")



## -----------------------------------------------------------------------------------------------------------------------------------
x.grid <- seq(-5, 5, by=0.01)
plot(x.grid, dnorm(x.grid), type='l')
lines(x.grid, dstable(x.grid,1.5,0), type='l',col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x.1 <- rstable(n,1.5,0)
  x.2 = rstable(n,1.5,0)
  p.value[j] <- t.test(x.1,y=x.2)$p.value
  #pb$tick()
}



## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Stable - Alpha=1.5')
plot(ecdf(p.value), main = 'Alpha=1.5')
abline(0,1, lty=2, col='red')

## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))


## -----------------------------------------------------------------------------------------------------------------------------------
plot(x.grid, dnorm(x.grid), type='l', ylim=c(0,.8))
lines(x.grid, dstable(x.grid,.5,0), type='l',col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x.1 <- rstable(n,.5,0)
  x.2 = rstable(n,.5,0)
  p.value[j] <- t.test(x.1,y=x.2)$p.value
  #pb$tick()
  }


## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Stable - Alpha = 0.5')
plot(ecdf(p.value), main = 'Alpha = 0.5')
abline(0,1, lty=2, col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))


## -----------------------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x.1 <- rstable(n,1.5,0)
  x.2 = rstable(n,1.5,0)
  p.value[j] <- wilcox.test(x.1,y=x.2)$p.value
  #pb$tick()
}



## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Stable - Alpha = 0.5')
plot(ecdf(p.value), main = 'Alpha = 0.5')
abline(0,1, lty=2, col='red')


## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))



## -----------------------------------------------------------------------------------------------------------------------------------
perm_t_test=function(x,y,iter=1e3){
  
  T0=abs(mean(x)-mean(y))  
  T_stat=numeric(iter)
  x_pooled=c(x,y)
  n=length(x_pooled)
  n1=length(x)
  for(perm in 1:iter){
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
    
  }
  
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
}



## -----------------------------------------------------------------------------------------------------------------------------------
p.value <- numeric(B)
#pb=progress_bar$new(total=B)
#pb$tick(0)
set.seed(seed)
for(j in 1:B)
{
  x.1 <- rstable(n,1.5,0)
  x.2 = rstable(n,1.5,0)
  p.value[j] <- perm_t_test(x.1,x.2,iter=1000)
  #pb$tick()
}


## -----------------------------------------------------------------------------------------------------------------------------------
hist(p.value, main = 'Stable - Alpha = 0.5')
plot(ecdf(p.value), main = 'Alpha = 0.5')
abline(0,1, lty=2, col='red')

## -----------------------------------------------------------------------------------------------------------------------------------
estimated.alpha <- sum(p.value < alpha)/B
c(estimated.alpha - sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975), estimated.alpha, estimated.alpha + sqrt(estimated.alpha*(1-estimated.alpha)/B)*qnorm(0.975))


## -----------------------------------------------------------------------------------------------------------------------------------
delta_grid=seq(.5,2,by=.5)
set.seed(seed)


## -----------------------------------------------------------------------------------------------------------------------------------
power_wilcox=numeric(length(delta_grid))
for(ii in 1:length(delta_grid)){
      p.value <- numeric(B)
      #pb=progress_bar$new(total=B)
      #pb$tick(0)
      delta=delta_grid[ii]
      for(j in 1:B){
        
        x.1 <- rstable(n,0.5,0)
        x.2 = rstable(n,0.5,0,delta=delta)
        p.value[j] <- wilcox.test(x.1,y=x.2,correct = T)$p.value
        #pb$tick()
        }
      
      estimated.power <- sum(p.value < alpha)/B
      power_wilcox[ii]=estimated.power
}      



## -----------------------------------------------------------------------------------------------------------------------------------
power_perm_1=numeric(length(delta_grid))
set.seed(seed)
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B)
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rstable(n,0.5,0)
    x.2 = rstable(n,0.5,0,delta=delta)
    p.value[j] <- perm_t_test(x.1,x.2,iter=500)
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_perm_1[ii]=estimated.power
}      


## -----------------------------------------------------------------------------------------------------------------------------------
perm_median_test=function(x,y,iter=1e4){
  
  
  T0=abs(median(x)-median(y))  
  T_stat=numeric(iter)
  x_pooled=c(x,y)
  n=length(x_pooled)
  n1=length(x)
  for(perm in 1:iter){
    # permutation:
    permutation <- sample(1:n)
    x_perm <- x_pooled[permutation]
    x1_perm <- x_perm[1:n1]
    x2_perm <- x_perm[(n1+1):n]
    # test statistic:
    T_stat[perm] <- abs(median(x1_perm) - median(x2_perm))
  }
  # p-value
  p_val <- sum(T_stat>=T0)/iter
  return(p_val)
  
}


## -----------------------------------------------------------------------------------------------------------------------------------
power_perm=numeric(length(delta_grid))
set.seed(seed)
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B)
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rstable(n,0.5,0)
    x.2 = rstable(n,0.5,0,delta=delta)
    p.value[j] <- perm_median_test(x.1,x.2,iter=100)
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_perm[ii]=estimated.power
}      


## -----------------------------------------------------------------------------------------------------------------------------------
plot(delta_grid,power_perm,type='l', main='Power',ylim=c(0,1))
lines(delta_grid,power_wilcox,col='red')
lines(delta_grid,power_perm_1,col='blue')


## -----------------------------------------------------------------------------------------------------------------------------------

delta_grid=seq(.1,1,by=.2)

power_perm=numeric(length(delta_grid))
set.seed(seed)
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B)
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rnorm(100)
    x.2 = rnorm(100,mean=delta)
    p.value[j] <- perm_t_test(x.1,x.2,iter=1000)
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_perm[ii]=estimated.power
}      



## -----------------------------------------------------------------------------------------------------------------------------------
set.seed(seed)
power_wilcox=numeric(length(delta_grid))
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B)
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rnorm(100)
    x.2 = rnorm(100,mean=delta)
    p.value[j] <- wilcox.test(x.1,y=x.2,correct = T)$p.value
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_wilcox[ii]=estimated.power
}     





## -----------------------------------------------------------------------------------------------------------------------------------
set.seed(seed)
power_t=numeric(length(delta_grid)) #I can actually compute in an analytical fashion, but let's go with sim also here.
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B)
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rnorm(100)
    x.2 = rnorm(100,mean=delta)
    p.value[j] <- t.test(x.1,y=x.2)$p.value
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_t[ii]=estimated.power
}


## -----------------------------------------------------------------------------------------------------------------------------------
set.seed(seed)
power_median=numeric(length(delta_grid))
for(ii in 1:length(delta_grid)){
  p.value <- numeric(B)
  #pb=progress_bar$new(total=B, format = "  computing [:bar] :percent eta: :eta")
  #pb$tick(0)
  delta=delta_grid[ii]
  for(j in 1:B){
    
    x.1 <- rnorm(100)
    x.2 = rnorm(100,mean=delta)
    p.value[j] <- perm_median_test(x.1,x.2,iter=1000)
    #pb$tick()
    }
  
  estimated.power <- sum(p.value < alpha)/B
  power_median[ii]=estimated.power
} 


## -----------------------------------------------------------------------------------------------------------------------------------
plot(delta_grid,power_t,ylim=c(0,1), main='Power, normal case',type='l')
lines(delta_grid,power_wilcox,col='red')
lines(delta_grid,power_perm,col='blue')
#lines(delta_grid,power_median,col='green')


