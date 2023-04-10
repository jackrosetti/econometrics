# Load necessary packages
library(ggplot2)
library(cowplot)

# Load your data
# Replace 'your_data' with the name of your data frame
your_data <- read.csv("your_data.csv")

# Split the data into training and testing sets
# Replace '0.8' with the desired proportion of data for training
set.seed(123)
train_indices <- sample(1:nrow(your_data), floor(0.8*nrow(your_data)))
train_data <- your_data[train_indices, ]
test_data <- your_data[-train_indices, ]

# Fit the regression model on the training set
# Replace 'response_var' with the name of your response variable
# Replace 'predictor_vars' with a vector of the names of your predictor variables
model <- lm(response_var ~ predictor_vars, data = train_data)

# Perform regression diagnostic checks
# Plot the residuals against other variables
par(mfrow = c(2, 2))  # Set up a 2x2 grid of plots
plot(model, which = 1:4)

# Plot the residuals against the predictor
ggplot(train_data, aes(x = predictor_vars, y = residuals(model))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs. Predictor")

# Plot the magnitude of the residuals against the predictor
ggplot(train_data, aes(x = predictor_vars, y = abs(residuals(model)))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Magnitude of Residuals vs. Predictor")

# Plot the residuals against coordinates and each other
plot(residuals(model), pch = 16, cex = 0.8, main = "Residuals vs. Index")
plot(residuals(model), residuals(model, type = "p"), pch = 16, cex = 0.8, main = "Residuals vs. Fitted Values")

# Plot the distribution of the residuals
ggplot(train_data, aes(x = residuals(model))) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Residuals")

# Evaluate the model on the testing set
test_data$predicted <- predict(model, newdata = test_data)
test_data$residuals <- residuals(model, newdata = test_data)

# Examine the predictions and residuals on the testing set
print(head(test_data[c("response_var", "predicted", "residuals")]))
