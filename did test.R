# Load necessary libraries
library(tidyverse)

# Import data
data <- read.csv("your_data.csv")

# Convert variables to factors if necessary
data$group <- as.factor(data$group)
data$time <- as.factor(data$time)

# Create interaction variable
data$group_time <- interaction(data$group, data$time)

# Create difference-in-differences model
did_model <- lm(outcome ~ group + time + group_time, data = data)

# Plot the trends
ggplot(data, aes(x = time, y = outcome, group = group)) +
  geom_line(aes(color = group)) +
  labs(title = "Parallel Trends Plot", x = "Time", y = "Outcome")

# Calculate the difference-in-differences estimate
did_estimate <- summary(did_model)$coefficients[4,1]

# View the estimate
cat("The difference-in-differences estimate is:", did_estimate, "\n")
