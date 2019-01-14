library(tidyverse)
#load data
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

# functions for performing gradient descent for logistic regression
sigmoid <- function(z){
  g <- 1/(1+exp(-z))
  return(g)
}
logit.compute.cost <- function(x,y,theta){
  g <- sigmoid(x %*% theta)
  J <- (-1/m)*sum(t(y)%*%log(g) + t(1-y)%*%log(1-g))
  return(J)
}
logit.gradient.descent <- function(x,y,theta,alpha,num_iters){
  # performs gradient descent in order to find theta parameters that
  # minimize the cost function J
  # it is advisable to perform feature scaling prior to gradient descent
  # grad = (1/m)*X'*(sigmoid(X*theta)-y)
  m <- length(y)
  J_history <- NULL
  for (i in 1:num_iters) {
    delta_theta <- NULL  
    err <- as.vector((sigmoid(x %*% theta) - y),mode = 'numeric')
    temp <- err * x
    delta_theta <- alpha*colSums(temp)/m
    theta <- theta - delta_theta
    J_history <- c(J_history,logit.compute.cost(x, y, theta))
  }
  return(list('min.theta' = theta,'J.history' = J_history))
}
# functions for finding optimum theta using advanced algorithms
optim.logit.cost <- function(theta){
  g <- sigmoid(x%*%theta)
  J <- (-1/m)*sum(y*log(g) + (1-y)*log(1-g))
  return(J)
}
optim.logit.gr <- function(theta){
  g <- sigmoid(x%*%theta)
  grad <- (1/m) * (t(g-y)%*%x)
  return(grad)
}