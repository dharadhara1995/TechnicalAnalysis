# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Standard Deviation (stdev)
# data: numeric vector
# Returns the population standard deviation (divides by n), as in the
# formula given in the assignment. Note: R's sd() divides by n - 1.
stdev <- function(data) {
  n <- length(data)

  # Calculate the mean of the data
  mean_value <- sum(data) / n

  # Calculate the differences between the data points and the mean
  diff_values <- numeric(n)
  for (i in 1:n) {
    diff_values[i] <- data[i] - mean_value
  }

  # Calculate the squared differences
  squared_diff <- numeric(n)
  for (i in 1:n) {
    squared_diff[i] <- diff_values[i] * diff_values[i]
  }

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}
