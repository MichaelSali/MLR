library(tidyverse)
#load data
x_multi <- ex1data2[,1:2]
y_multi <- ex1data2[,3]

# perform feature scaling since our 2 features are on very different scales
norm_x <- feature.normalize(x_multi)
x_multi_norm <- norm_x$x_norm
# add x0 column
x_multi_norm <- cbind(x0 = rep(1,length(y_multi)),x_multi_norm)
# parameters for gradient descent
alpha <- 0.01
iterations <- 400
theta <- rep(0,ncol(x_multi_norm))

# run gradient descent to find optimal thetas
min_J_multi = gradient.descent(x_multi_norm, y_multi, theta, alpha, iterations)

plot(min_J_multi$J.history)

# predict the price of a 1650 sq foot 3 bedroom house
new_house <- c(1650,3)
# first normalize the values
norm_new_house <- (new_house - norm_x$mean)/norm_x$SD
norm_new_house <- c(1,norm_new_house)
pred.price <- sum(norm_new_house*min_J_multi$min.theta)

lin.reg.model <- lm(y_multi~ . , x_multi)
summary(lin.reg.model)
