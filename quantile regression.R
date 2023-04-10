# Load data
data <- read.csv("mydata.csv")

# Define quantile of interest
tau <- 0.5

# Create design matrix and response vector
X <- cbind(1, data$x1, data$x2, data$x3)
y <- data$y

# Number of observations and regressors
n <- nrow(X)
p <- ncol(X)

# Define optimization objective function
f <- function(beta) {
  sum(pmax(tau * (y - X %*% beta), (tau - 1) * (y - X %*% beta)))
}

# Perform optimization
start <- rep(0, p)
result <- optim(start, f, hessian = TRUE)

# Extract coefficient estimates and standard errors
beta_hat <- result$par
se <- sqrt(diag(solve(result$hessian)))

# Print results
cat(paste("Coefficient estimates:", beta_hat, "\n"))
cat(paste("Standard errors:", se, "\n"))
