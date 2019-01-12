library(tidyverse)
# load data
my_data <- mtcars
attach(my_data)
# plot data that you want to model in our case we want to predict qsec based on hp
plot(qsec~hp) # simple scatter plot
scatter.smooth(qsec~hp) 
par(mfrow=c(1,2))
boxplot(qsec, main = 'qsec')
boxplot(hp)
cor(qsec,hp)
# add a column of ones to x
X_hp <- hp
X_hp <- cbind(x0 = rep(1,nrow(my_data)),x1 = X_hp)
Y <- qsec
theta <- rep(0,ncol(X_hp)) # initialize thetas
# some gradient descent settings
iterations <- 1500
alpha <- 0.01
# data set from Andrew NG ML course
X_NG <- ex1data1$V1
X_NG <- cbind(x0 = rep(1,nrow(ex1data1)),x1 = X_NG)
y_NG <- ex1data1$V2

# run gradient descent to find optimal thetas
min_J = gradient.descent(X_NG, y_NG, theta, alpha = 0.01, num_iters = 4000)
# compare with linear regression function
lin.reg.model <- lm(y_NG~X_NG[,2])
coefs <- lin.reg.model$coefficients
#plot the residuals of the linear regression model
plot(lin.reg.model$residuals, pch = 16, col = "red")
