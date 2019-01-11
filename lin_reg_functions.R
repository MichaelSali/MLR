compute.cost <- function(x,y,theta){
  # computes the cost function J of linear regression model
  m <- length(y)
  J <- 0
  J <- sum((x%*%theta-y)^2)/(2*m)
  return(J)
}

gradient.descent <- function(x,y,theta,alpha,num_iters){
  # performs gradient descent in order to find theta parameters that
  # minimize the cost function J
  # it is advisable to perform feature scaling prior to gradient descent
  m <- length(y)
  J_history <- NULL
  for (i in 1:num_iters) {
    delta_theta <- NULL  
    err <- as.vector((x %*% theta - y),mode = 'numeric')
    temp <- err * x
    delta_theta <- alpha*colSums(temp)/m
    theta <- theta - delta_theta
    J_history <- c(J_history,compute.cost(x, y, theta))
  }
  return(list('min.theta' = theta,'J.history' = J_history))
}

feature.normalize <- function(x){
  # normalizes the features so that all of them will have mean = 0 and sd = 1
  mu <- as.vector(summarize_all(x,mean), mode = 'numeric')
  sigma <- as.vector(summarise_all(x,sd), mode = 'numeric')
  x <- as.matrix(x)
  x_norm <- sweep(x,2,mu)
  x_norm <- sweep(x_norm,2, FUN = '/',sigma)
  return(list('mean' = mu,'SD' = sigma, 'x_norm' = x_norm))
}


normal.eq <- function(x,y){
# normal equation solves the linear regression model and outputs optimal theta values
# not suitable for extremely large matrices (>50k rows/columns) as it becomes computationaly 
# expensive
# you don't need to perform feature normalization prior to using normal equations
  
  if (is.matrix(x)==FALSE) {
    x <- as.matrix(x)
  }
  x <- cbind(x0 = rep(1,nrow(x)),x)
  result <- solve(t(x) %*% x) %*%  t(x) %*% y
  return(result)
}
