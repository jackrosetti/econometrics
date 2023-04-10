# Define log likelihood function for a normal distribution
loglik_normal <- function(params, x) {
  mu <- params[1]
  sigma <- params[2]
  n <- length(x)
  
  ll <- -(n/2) * log(2*pi) - (n/2) * log(sigma^2) - (1/(2*sigma^2)) * sum((x-mu)^2)
  
  return(ll)
}

# Define maximum likelihood estimator function
mle <- function(loglik_func, data, start_params, optimizer = "Nelder-Mead") {
  result <- optim(start_params, loglik_func, data = data, method = optimizer)
  
  return(result$par)
}

# Generate some data from a normal distribution
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)

# Use the MLE function to estimate the parameters of the normal distribution
start_params <- c(mean(x), sd(x))
est_params <- mle(loglik_normal, data = x, start_params = start_params)

# View estimated parameters
est_params
