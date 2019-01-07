compute.cost <- function(X,y,theta){
  m <- length(y)
  J <- 0
  J <- sum((X%*%theta-y)^2)/(2*m)
  return(J)
}

gradient.descent <- function(X,y,theta,alpha,num_iters){
  m <- length(y)
  J_history <- NULL
  for (i in 1:num_iters) {
    delta_theta <- c(0,0)
    delta_theta[1] <- sum((X%*%theta-y)*X[,1])*alpha/m
    delta_theta[2] <- sum((X%*%theta-y)*X[,2])*alpha/m
    theta <- theta - delta_theta
    J_history <- c(J_history,compute.cost(X, y, theta))
  }
  return(list('min.theta' = theta,'J.history' = J_history))
}

feature.normalize <- function(x){
  mu <- as.vector(summarize_all(x,mean), mode = 'numeric')
  sigma <- as.vector(summarise_all(x,sd), mode = 'numeric')
  x <- as.matrix(x)
  x_norm <- sweep(x,2,mu)
  x_norm <- sweep(x_norm,2, FUN = '/',sigma)
  return(list('mean' = mu,'SD' = sigma, 'x_norm' = x_norm))
}