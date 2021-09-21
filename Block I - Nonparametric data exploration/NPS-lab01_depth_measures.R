library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)

## .extracode {

## background-color: lightblue;

## }


library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(packagefinder)
library(aplpack)
library(robustbase)

set.seed(2781991) # reproducibility
n=500
df_bivariate_exp = cbind(rexp(n), rexp(n))
head(df_bivariate_exp)

plot(df_bivariate_exp[,1],df_bivariate_exp[,2], xlab="exp 1", ylab="exp 2")

bin=hexbin(df_bivariate_exp[,1],df_bivariate_exp[,2], xbins=10, xlab="exp 1", ylab="exp 2")
plot(bin, main="Hexagonal Binning") 

## findPackage(keywords = "depth", display = "viewer", limit.results=10)

tukey_depth=depth(u=df_bivariate_exp,method='Tukey')

depth(u = c(0, 0), X = df_bivariate_exp, method = 'Tukey') 

depthMedian(df_bivariate_exp,depth_params = list(method='Tukey'))

df_bivariate_exp[which.max(tukey_depth),]

depthContour(df_bivariate_exp,depth_params = list(method='Tukey'))

depthPersp(df_bivariate_exp,depth_params = list(method='Tukey'))

depthPersp(df_bivariate_exp,depth_params = list(method='Tukey'),plot_method = 'rgl')

maha_depth <- depth(df_bivariate_exp,method='Mahalanobis') 

sample_mean <- colMeans(df_bivariate_exp)
sample_S <- cov(df_bivariate_exp)

maha_depth_manual <- 1/(1+mahalanobis(x = df_bivariate_exp,center = sample_mean,cov = sample_S))


all(abs(maha_depth-maha_depth_manual)<1e-15) # food for thought: sqrt(2) ^ 2 == 2?

depthMedian(df_bivariate_exp,depth_params = list(method='Mahalanobis'))
df_bivariate_exp[which.max(maha_depth_manual),]

depthContour(df_bivariate_exp,depth_params = list(method='Mahalanobis'))
depthPersp(df_bivariate_exp,depth_params = list(method='Mahalanobis'))
depthPersp(df_bivariate_exp,depth_params = list(method='Mahalanobis'),plot_method = 'rgl')

set.seed(1992)
df_bivariate_cauchy = cbind(rcauchy(n,location=0,scale=.001), rcauchy(n,location = 0,scale=.001))
head(df_bivariate_cauchy)

mu_good = c(0,0) 
mu_outliers = c(7,7)

sigma_common = matrix(c(1,.7,.7,1), ncol = 2)

frac = .05
n=100
# sample points
n_good=ceiling(n*(1-frac))
n_outliers=n-n_good
df_contaminated_normals = data.frame(rbind(
  mvrnorm(n_good, mu_good, sigma_common),
  mvrnorm(n_outliers, mu_outliers, sigma_common)
))

class <- c(rep(1,n_good),rep(2,n_outliers))
plot(df_contaminated_normals,xlab="Norm 1", ylab="Norm 2",col=class)

depthContour(
  df_contaminated_normals,
  depth_params = list(method = 'Tukey'),
  points = TRUE,
  colors = colorRampPalette(c('white', 'navy')),
  levels = 10,
  pdmedian = F,
  graph_params = list(cex=.01, pch=1),
  pmean = F
)

bagplot(df_contaminated_normals)

aplpack::bagplot(df_contaminated_normals,show.whiskers = F,main="Bagplot")
aplpack::bagplot(df_contaminated_normals,show.loophull = F,main="Sunburst plot")

bagplot_cont_normals <- bagplot(df_contaminated_normals)
outlying_obs <- bagplot_cont_normals$pxy.outlier

df_clean_1 <-
  dplyr::anti_join(
    x = df_contaminated_normals,
    y = outlying_obs,
    by = c("X1" = "x", "X2" = "y"),
    copy = TRUE
  )

ind_outlying_obs <- which(apply(df_contaminated_normals,1,function(x) all(x %in% outlying_obs)))
df_clean_2 <- df_contaminated_normals[-ind_outlying_obs,]

all.equal(df_clean_1,df_clean_2)

data(starsCYG, package = "robustbase")
names(starsCYG)
plot(starsCYG, main="Star Cluster CYG OB1")

depthContour(as.matrix(starsCYG), depth_params = list(method='Tukey'), points=TRUE)

depthMedian(starsCYG) 

bagplot_starsCYG <- with(starsCYG,aplpack::bagplot(log.Te,log.light))
red_giants <- bagplot_starsCYG$pxy.outlier
ind_outlying_obs <- which(apply(starsCYG,1,function(x) all(x %in% red_giants)))
ind_outlying_obs

mu_good = rep(0,3)
mu_outliers = c(12,12,3)

sigma_common = diag(3)*2

frac = .1
n=300
# sample points
n_good=ceiling(n*(1-frac))
n_outliers=n-n_good
df_3 = data.frame(rbind(
  mvrnorm(n_good, mu_good, sigma_common),
  mvrnorm(n_outliers, mu_outliers, sigma_common)
))
class <- c(rep(1,n_good),rep(2,n_outliers))
pairs(df_3, col=class)

bagplot_matrix <- aplpack::bagplot.pairs(df_3)

df_good <- df_3[1:n_good,]
df_out <- df_3[(n_good+1):n,]
ddPlot(x = df_good,y = df_out,depth_params = list(method='Tukey'))

depth_good <- depth(u = df_3,X = df_good,method = "Tukey")
depth_out <- depth(u = df_3,X = df_out,method = "Tukey")
plot(depth_good,depth_out, col="blue", xlab="X depth", ylab="Y depth", main= "Depth vs. depth plot")
grid(10, 10, col="grey50", lty=1)
abline(0,1, col="grey50")

n_extra <- 100
df_extra <- data.frame(mvrnorm(n_extra, mu_good, sigma_common))
ddPlot(x = df_extra, df_good,depth_params=list(method='Tukey'))
