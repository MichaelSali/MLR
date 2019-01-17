library(tidyverse)
#load 1st data set
x <- as.matrix(ex2data1[,1:2])
colnames(x) <- c('Score.1','Score.2')
y <- ex2data1[,3]
m <- length(y)
# visualize the data
plot(x,pch = y,col = as.factor(y))

x <- cbind(x0 = rep(1,m),x)
initial_theta <- rep(0,ncol(x))
# finding optimal thetas using BFGS unconstrained algorithm
optim.j <- optim(initial_theta,optim.logit.cost,gr = optim.logit.gr, method = 'BFGS',control =  c(maxit = 400))

# plotting the linear decision boundary
optim_theta <- optim.j$par
intercept <- -optim_theta[1]/optim_theta[3]
slope <- -optim_theta[2]/optim_theta[3]
plot(x[,2:3],pch = y,col = as.factor(y))
abline(intercept,slope)
# how accurate was the regression predicting the training set values
predictions <- sigmoid(x%*%optim_theta)>=0.5
accuracy <- mean(predictions==y)*100

# load 2nd data set
x <- as.matrix(ex2data2[,1:2])
colnames(x) <- c('Chip.test.1','Chip.test.2')
y <- as.matrix(ex2data2$V3)
m <- length(y)
plot(x,pch = y + 1,col = as.factor(y))
legend('topright', legend = c('y = 1','y = 0'), col = c(2,1),pch = c(2,1))
x <- cbind(x0 = rep(1,m),x)
initial_theta <- rep(0,ncol(x))
# add polynomial features to x
poly_x <- add.polynomial.features(x[,2],x[,3],6)
initial_theta <- rep(1,ncol(poly_x))
# finding optimal thetas using BFGS unconstrained algorithm
optim.j <- optim(initial_theta,logit.cost,gr = logit.gr,x = poly_x,y = y,lambda = 0, method = 'BFGS',control =  c(maxit = 400))
# plotting the non-linear decision boundary

x.axis = seq(-1, 1.5, length.out =  50)
y.axis = seq(-1, 1.5, length.out = 50)
z <- matrix(nrow = 50,ncol = 50)
for (i  in 1:length(x.axis)) {
  for (j in 1:length(y.axis)) {
    z[i,j] = add.polynomial.features(x.axis[i], y.axis[j],6) %*% optim.j$par
    
  }
}

plot(x[,2:3],pch = y + 1,col = as.factor(y))
contour(x.axis,y.axis,z,nlevels = 1,add = TRUE)


# functions for performing gradient descent for logistic regression
sigmoid <- function(z){
  g <- 1/(1+exp(-z))
  return(g)
}
logit.cost <- function(x, y, theta, lambda = 0){
  m <- length(y)
  g <- sigmoid(x%*%theta)
  temp_theta <- theta
  temp_theta[1] <- 0
  J <- (-1/m)*sum(y*log(g) + (1-y)*log(1-g)) + (lambda/(2*m))*sum(temp_theta^2)
  return(J)
}

logit.gr <- function(x, y, theta, lambda = 0){
  m <- length(y)
  g <- sigmoid(x%*%theta)
  temp_theta <- theta
  temp_theta[1] <- 0
  grad <- (1/m) * (t(g-y)%*%x) + (lambda/m)*temp_theta
  return(grad)
}

logit.gradient.descent <- function(x,y,theta,alpha,num_iters,lambda = 0){
  # performs gradient descent in order to find theta parameters that
  # minimize the cost function J
  # it is advisable to perform feature scaling prior to gradient descent
  
  m <- length(y)
  g <- sigmoid(x%*%theta)
  temp_theta <- theta
  temp_theta[1] <- 0
  J_history <- NULL
  for (i in 1:num_iters) {
    delta_theta <- NULL  
    err <- as.vector((g - y),mode = 'numeric')
    temp <- err * x
    delta_theta <- alpha*colSums(temp)/m + (lambda/m)*temp_theta
    theta <- theta - delta_theta
    J_history <- c(J_history,logit.cost(x, y, theta))
  }
  return(list('min.theta' = theta,'J.history' = J_history))
}

add.polynomial.features <- function(x1,x2,degree){
  m <- length(x1)
  new_x <- rep(1,m)
  for (i in 1:degree) {
    for (j in 0:i) {
      new_x <- cbind(new_x,x1^(i-j) * x2^j)
    }
  }
  return(new_x)
}
