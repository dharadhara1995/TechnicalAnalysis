# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Simple Moving Average (SMA)
# data:   numeric vector
# period: number of data points in each window
# Returns a vector of length (length(data) - period + 1).
# The first value is the average of data[1:period].
sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_values   <- length(data) - period + 1
  sma_values <- numeric(n_values)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_values) {
    current_window <- data[i:(i + period - 1)]       # the 'period' points in this window
    sma_values[i]  <- sum(current_window) / period   # average of the window
  }

  return(sma_values)
}
