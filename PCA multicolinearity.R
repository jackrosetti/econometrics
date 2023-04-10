# Load necessary libraries
library(tidyverse)
library(car)

# Import data
data <- read.csv("your_data.csv")

# Fit the regression model
model <- lm(outcome ~ predictor1 + predictor2 + predictor3, data = data)

# Check for multicollinearity
vif_values <- vif(model)

# If VIF > 5, then high multicollinearity is present
if(max(vif_values) > 5) {
  # Perform PCA on predictor variables
  pca_model <- prcomp(data[, -1], scale. = TRUE)
  
  # Extract principal component scores
  pc_scores <- as.data.frame(pca_model$x)
  
  # Add outcome variable to principal component scores
  pc_data <- cbind(data$outcome, pc_scores)
  
  # Fit a new regression model using principal components
  pc_model <- lm(pc_data[,1] ~ pc_data[,2] + pc_data[,3], data = pc_data)
  
  # View summary of new model
  summary(pc_model)
} else {
  # No high multicollinearity present, view summary of original model
  summary(model)
}
