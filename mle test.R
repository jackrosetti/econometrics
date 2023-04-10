# Define log likelihood function for a simple linear regression model
loglik_regression <- function(params, x, y) {
  beta0 <- params[1]
  beta1 <- params[2]
  sigma <- params[3]
  n <- length(x)
  
  ll <- -(n/2) * log(2*pi*sigma^2) - (1/(2*sigma^2)) * sum((y - beta0 - beta1*x)^2)
  
  return(ll)
}

# Define maximum likelihood estimator function
mle <- function(loglik_func, data, start_params, optimizer = "Nelder-Mead") {
  x <- data$x
  y <- data$y
  result <- optim(start_params, loglik_func, x = x, y = y, method = optimizer)
  
  return(result$par)
}

# Generate some data for a simple linear regression model
set.seed(123)
x <- rnorm(100)
y <- 2*x + rnorm(100)

# Use the MLE function to estimate the parameters of the simple linear regression model
start_params <- c(0, 0, sd(y))
data <- data.frame(x, y)
est_params <- mle(loglik_regression, data = data, start_params = start_params)

# View estimated parameters
est_params
