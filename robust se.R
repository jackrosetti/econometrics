# Load data
data <- read.csv("mydata.csv")

# Create model with cluster-robust standard errors
model <- lm(y ~ x1 + x2 + x3, data = data)
cluster <- data$cluster_var
n <- nrow(data)
p <- ncol(model.matrix(model))
K <- length(unique(cluster))
X <- model.matrix(model)

# Calculate cluster-robust variance-covariance matrix
clusters <- split(1:n, cluster)
Q <- 0
for (k in 1:K) {
  Xk <- X[clusters[[k]], ]
  Ek <- resid(model)[clusters[[k]]]
  Qk <- t(Xk) %*% Xk %*% diag(Ek^2) %*% t(Xk)
  Q <- Q + Qk
}
V <- solve(t(X) %*% X) %*% Q %*% solve(t(X) %*% X)
se <- sqrt(diag(V))

# Print results
summary(model)
cbind(coefficients(model), se)
