# courtesy of http://www.statalgo.com/2011/10/17/stanford-ml-1-2-gradient-descent/

# Load data and initialize values
data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")
 
alpha <- 0.01
m <- nrow(data)
x <- matrix(c(rep(1,m), data$area), ncol=2)
y <- matrix(data$price, ncol=1) / 1000
 
# Z-Score the feature
x.scaled <- x
x.scaled[,2] <- (x[,2] - mean(x[,2]))/sd(x[,2])
 
# Gradient descent function
grad <- function(x, y, theta) {
    t(t(x) %*% ((x %*% t(theta)) - y))
}
 
gradient.path <- function(x) {
    # Initialize the parameters
    theta <- matrix(c(0, 0), nrow=1)
 
    # Look at the values over each iteration
    theta.path <- matrix(ncol=2)
    for (i in 1:500) {
        theta <- theta - alpha * 1/m * grad(x, y, theta)
        if(all(is.na(theta))) break
        theta.path <- rbind(theta.path, theta)
    }
    theta.path
}
 
unscaled.theta <- gradient.path(x)
scaled.theta <- gradient.path(x.scaled)
 
summary(lm(y ~ x[,2]))
summary(lm(y ~ x.scaled[,2]))
