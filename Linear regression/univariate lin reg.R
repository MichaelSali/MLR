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
#plot the regression
plot(y_NG~X_NG[,2])
abline(coef = coef(lin.reg.model))
coefs <- lin.reg.model$coefficients
#plot the residuals of the linear regression model
plot(lin.reg.model$residuals, pch = 16, col = "red")

#plot the cost function using surface plot
library(plotly)
df1 <- matrix(ncol = 200,nrow = 200)
colnames(df1) <- seq(-10,9.9,0.1)
row.names(df1) <- seq(-2,2.975,0.025)
for (i  in 1:ncol(df1)) {
  for (j in 1:nrow(df1)) {
    theta0 <- as.double(colnames(df1)[i])
    theta1 <- as.double(row.names(df1)[j])
    theta <- c(theta0,theta1)
    df1[j,i] <- compute.cost(X_NG,y_NG,theta)
  }
}
x <- as.double(row.names(df1))
y <- as.double(colnames(df1))
plot_ly(x = x, y = y, z = df1, type = 'surface')
