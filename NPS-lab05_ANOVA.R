## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------
library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)


## .extracode {

## background-color: lightblue;

## }


## -----------------------------------------------------------------------------------------------------------------------------------

B = 1000
seed = 26111992


## -----------------------------------------------------------------------------------------------------------------------------------
head(chickwts)
attach(chickwts)
summary(chickwts)


## -----------------------------------------------------------------------------------------------------------------------------------
g <- nlevels(feed)
n <- dim(chickwts)[1]


plot(feed, weight, xlab='treat',col=rainbow(g),main='Original Data')


## -----------------------------------------------------------------------------------------------------------------------------------
fit <- aov(weight ~ feed)
summary(fit)



## -----------------------------------------------------------------------------------------------------------------------------------
T0 <- summary(fit)[[1]][1,4]
T0



## -----------------------------------------------------------------------------------------------------------------------------------
T_stat <- numeric(B) 
n <- dim(chickwts)[1]

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  weight_perm <- weight[permutation]
  fit_perm <- aov(weight_perm ~ feed)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}



## -----------------------------------------------------------------------------------------------------------------------------------
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val


## -----------------------------------------------------------------------------------------------------------------------------------
library(lmPerm)
lmp=aovp(weight_perm ~ feed,perm="Prob",Cp=0.1)#cp is supposed to stop iterations when standard error is at that level...
summary(lmp)


## -----------------------------------------------------------------------------------------------------------------------------------
lmp=aovp(weight_perm ~ feed,perm="Prob",Cp=1e-6)
summary(lmp)


## -----------------------------------------------------------------------------------------------------------------------------------
lmp=aovp(weight_perm ~ feed,perm="Prob",Cp=1e-6)
summary(lmp)

lmp=aovp(weight_perm ~ feed,perm="Prob",Cp=1e-6)
summary(lmp)

lmp=aovp(weight_perm ~ feed,perm="Prob",Cp=1e-6)
summary(lmp)



## -----------------------------------------------------------------------------------------------------------------------------------
data(iris)
attach(iris)
head(iris)


## -----------------------------------------------------------------------------------------------------------------------------------
species.name <- factor(Species, labels=c('setosa','versicolor','virginica'))
iris4        <- iris[,1:4]
plot(iris4,col=species.name)


## -----------------------------------------------------------------------------------------------------------------------------------
i1 <- which(species.name=='setosa')
i2 <- which(species.name=='versicolor')
i3 <- which(species.name=='virginica')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3

g  <- length(levels(species.name))
p  <- 4


## -----------------------------------------------------------------------------------------------------------------------------------
fit <- manova(as.matrix(iris4) ~ species.name)
summary.manova(fit,test="Wilks") 
T0 <- -summary.manova(fit,test="Wilks")$stats[1,2]
T0




## -----------------------------------------------------------------------------------------------------------------------------------
set.seed(seed)
T_stat <- numeric(B)

for(perm in 1:B){
  # choose random permutation
  permutation <- sample(1:n)
  species.name.perm <- species.name[permutation]
  fit.perm <- manova(as.matrix(iris4) ~ species.name.perm)
  T_stat[perm] <- -summary.manova(fit.perm,test="Wilks")$stats[1,2]
}


## -----------------------------------------------------------------------------------------------------------------------------------
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-2,1))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val




## -----------------------------------------------------------------------------------------------------------------------------------
km          <- c(18.7, 16.8, 20.1, 22.4, 14.0, 15.2, 22.0, 23.3)
station     <- factor(c('Esso','Esso','Esso','Esso','Shell','Shell','Shell','Shell'))
fuel        <- factor(c('95','95','98','98','95','95','98','98'))
station_fuel<- factor(c('Esso95','Esso95','Esso98','Esso98','Shell95','Shell95','Shell98','Shell98'))

M             <- mean(km)
Mstation      <- tapply(km,      station, mean)
Mfuel         <- tapply(km,       fuel, mean)
Mstation_fuel <- tapply(km, station_fuel, mean)


## -----------------------------------------------------------------------------------------------------------------------------------
plot(station_fuel, km, col=rainbow(5)[2:5], ylim=c(0,24))


## -----------------------------------------------------------------------------------------------------------------------------------
# Parametric test:
summary.aov(aov(km ~ station + fuel + station:fuel))
# Without interaction
summary.aov(aov(km ~ station + fuel))
# Without station
summary.aov(aov(km ~ fuel))



## -----------------------------------------------------------------------------------------------------------------------------------
summary.aov(aov(km ~ station + fuel + station:fuel))
T0_station_fuel <- summary.aov(aov(km ~ station + fuel + station:fuel))[[1]][3,4]
T0_station_fuel



## -----------------------------------------------------------------------------------------------------------------------------------

aov.H0station_fuel <- aov(km ~ station + fuel)
aov.H0station_fuel
residuals.H0station_fuel <- aov.H0station_fuel$residuals
n <- 8


T_station_fuel <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  residuals.H0station_fuel <- residuals.H0station_fuel[permutation]
  km.perm.H0station_fuel <- aov.H0station_fuel$fitted + residuals.H0station_fuel
  T_station_fuel[perm] <- summary.aov(aov(km.perm.H0station_fuel ~ station + fuel + station:fuel))[[1]][3,4]
}


## -----------------------------------------------------------------------------------------------------------------------------------
sum(T_station_fuel >= T0_station_fuel)/B


## -----------------------------------------------------------------------------------------------------------------------------------
T0_station <- summary.aov(aov(km ~ station + fuel))[[1]][1,4]
# residuals under H0:
# km = mu + beta*fuel
aov.H0station <- aov(km ~ fuel)
residuals.H0station <- aov.H0station$residuals

# TEST OF FACTOR FUEL   (H0: beta=0)
T0_fuel <- summary.aov(aov(km ~ station + fuel))[[1]][2,4]
# residuals under H0:
# km = mu + alpha*station
aov.H0fuel <- aov(km ~ station)
residuals.H0fuel <- aov.H0fuel$residuals



## -----------------------------------------------------------------------------------------------------------------------------------
# TEST OF FACTOR STATION ANF TEST OF FACTOR FUEL
# p-values
B <- 1000
T_fuel <- T_station <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  
  km.perm.H0station <- aov.H0station$fitted + residuals.H0station[permutation]
  T_station[perm] <- summary.aov(aov(km.perm.H0station ~ station + fuel))[[1]][1,4]
  
  km.perm.H0fuel <- aov.H0fuel$fitted + residuals.H0fuel[permutation]
  T_fuel[perm] <- summary.aov(aov(km.perm.H0fuel ~ station + fuel))[[1]][2,4]
}

sum(T_station >= T0_station)/B
sum(T_fuel >= T0_fuel)/B



## -----------------------------------------------------------------------------------------------------------------------------------
# TEST ON THE FACTOR FUEL
T0_fuel <- summary.aov(aov(km ~  fuel))[[1]][1,4]
# residuals under H0
# km = mu
residuals.H0fuel <- km - M

# Note that in this case, permuting the residuals under H0 
# and permuting the data is exactly the same:
permutation <- sample(n)
km.perm.H0fuel <- M + residuals.H0fuel[permutation]
km.perm        <- km[permutation]

km.perm.H0fuel
km.perm


## -----------------------------------------------------------------------------------------------------------------------------------
T_fuel <- numeric(B)
for(perm in 1:B){
  permutation <- sample(n)
  km.perm <- km[permutation]
  T_fuel[perm] <- summary.lm(aov(km.perm ~ fuel ))$f[1]
  
}
sum(T_fuel >= T0_fuel)/B


