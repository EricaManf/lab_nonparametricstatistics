library(rgl)
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(webgl = hook_webgl)

library(roahd)

P <- 101
grid <-  seq( 0, 1, length.out =  P)
  
alpha <-  0.2
beta <-  0.2

C_st <- exp_cov_function( grid, alpha, beta )

image( C_st,
       main = 'Exponential covariance function',
       xlab = 'grid', ylab = 'grid')

m <- sin(pi*grid)+sin(2*pi*grid)

n <- 100
set.seed(26111992)
data <- generate_gauss_fdata(N = n,centerline = m,Cov=C_st)

matplot(grid,t(data), type="l", col=adjustcolor(col=1,alpha.f = .4))
lines(grid,m, col="blue", lwd=5)

f_data <- fData(grid,data)
plot(f_data) # what happens if I do plot(data)?
lines(grid,m, col="black", lwd=5)

class(f_data)

alpha <-  1
beta <-  0.2

C_st <- exp_cov_function( grid, alpha, beta )

data <- generate_gauss_fdata(N = n,centerline = m,Cov=C_st)

f_data <- fData(grid,data)
plot(f_data, main="High overall level of variability")

alpha <-  .1
beta <-  0.0001

C_st <- exp_cov_function( grid, alpha, beta )

data <- generate_gauss_fdata(N = n,centerline = m,Cov=C_st)

f_data <- fData(grid,data)
plot(f_data, main="High autocorrelation") 

alpha <-  .1
beta <-  100

C_st <- exp_cov_function( grid, alpha, beta )

data <- generate_gauss_fdata(N = n,centerline = m,Cov=C_st)

f_data <- fData(grid,data)
plot(f_data, main="Virtually uncorrelated signals") 

data("mfD_healthy") # 
univariate_fdata <- mfD_healthy$fDList[[1]] # I consider the first lead only
plot(univariate_fdata)

band_depth <- BD(Data = univariate_fdata)
modified_band_depth <- MBD(Data = univariate_fdata)

median_curve <- median_fData(fData = univariate_fdata, type = "MBD") # still an fData object

median_curve_manual <- univariate_fdata[which.max(modified_band_depth),] # still an fData object

all(median_curve_manual$values==median_curve$values) 

plot(univariate_fdata)
grid_ecg <- seq(median_curve_manual$t0,median_curve_manual$tP,by=median_curve_manual$h)
lines(grid_ecg,median_curve_manual$values)

bivariate_data <- as.mfData(list(mfD_healthy$fDList[[1]], mfD_healthy$fDList[[2]]))
plot(bivariate_data)
cor_spearman(bivariate_data, ordering='MEI')

MEI_first_lead <- MEI(bivariate_data$fDList[[1]])
MEI_second_lead <- MEI(bivariate_data$fDList[[2]])

cor(MEI_first_lead, MEI_second_lead)

do.call(args = lapply(1:2, function(ind)
  MEI(bivariate_data$fDList[[ind]])), what = "cor")


alpha <-  0.2
beta <-  0.002

C_st <- exp_cov_function( grid, alpha, beta )

data <- generate_gauss_fdata(N = n,centerline = m,Cov=C_st)

f_data <- fData(grid,data)

outlier_share <- .1
n_outliers <-   n*outlier_share
out_highlighter <- rep(c(1,2),c(n-n_outliers,n_outliers))
f_data_temp <- f_data[1:(n*(1-outlier_share)),] # Coding tip: subsetting is mabe possible by the S3 class fdata
mag_temp <- f_data[(n*(1-outlier_share)+1):n,] * runif(10,2,3)

f_data_mag <- append_fData(f_data_temp,mag_temp)
plot(f_data_mag, col=out_highlighter)

shift_q <- .5

mu_warp=mu=sin(pi*grid+shift_q)+sin(2*pi*grid+shift_q)

shape_temp=generate_gauss_fdata(N = n_outliers, mu_warp, Cov=C_st)
shape_temp=fData(grid,shape_temp)
fdata_shape=append_fData(f_data_temp,shape_temp) 
plot(fdata_shape, col=out_highlighter)

invisible(fbplot(f_data_mag, main="Magnitude outliers"))
invisible(outliergram(f_data_mag))

fbplot(fdata_shape, main="Shape outliers")
outliergram(fdata_shape)

out_shape <- outliergram(fdata_shape, display = FALSE)
out_shape$ID_outliers
