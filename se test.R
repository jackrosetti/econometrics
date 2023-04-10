# Load data
data <- read.csv("mydata.csv")

# Create model
model <- lm(y ~ x1 + x2 + x3, data = data)

# Diagnostic tests
# 1. Breusch-Pagan test for heteroskedasticity
bp_test <- bptest(model)
bp_pval <- bp_test$p.value

# 2. White test for heteroskedasticity
white_test <- coeftest(model, vcov = vcovHC(model, type = "HC3"))
white_pval <- white_test[, "Pr(>|t|)"][4]

# 3. ARCH test for conditional heteroskedasticity
library(lmtest)
arch_test <- archtest(resid(model))
arch_pval <- arch_test$p.value

# 4. Shapiro-Wilk test for normality of residuals
shapiro_test <- shapiro.test(resid(model))
shapiro_pval <- shapiro_test$p.value

# Determine optimal type of standard error
if (bp_pval < 0.05 || white_pval < 0.05 || arch_pval < 0.05) {
  # Heteroskedastic or conditional heteroskedastic errors
  if (shapiro_pval < 0.05) {
    # Non-normal errors - use robust standard errors
    se_type <- "HC3"
  } else {
    # Normal errors - use OLS standard errors
    se_type <- "OLS"
  }
} else {
  # Homoskedastic errors
  se_type <- "OLS"
}

# Calculate standard errors
se <- sqrt(diag(vcovHC(model, type = se_type)))

# Print results
summary(model)
cbind(coefficients(model), se)
