# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Linear Regression (linreg)
# regressionSource: numeric vector (e.g. closing prices)
# regressionLength: how many recent points to use
# regressionOffset: how many of the most recent points to skip
# Returns a list with slope, intercept and predicted_values.
# The window is the last regressionLength points, shifted back by
# regressionOffset points. The x values are the positions 1, 2, 3, ...
# inside the selected window.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)

  # Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }

  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }

  # Calculate the starting index for the regressionSource
  # NOTE: the pseudocode uses n - regressionLength + regressionOffset, which
  # gives regressionLength + 1 points when the offset is 0 and fewer points
  # when the offset is above 0. I use n - regressionLength + 1 - regressionOffset
  # so the window always has exactly regressionLength points and ends
  # regressionOffset points before the last value.
  start_index <- max(1, n - regressionLength + 1 - regressionOffset)

  # Calculate the ending index for the regressionSource
  end_index <- min(n, n - regressionOffset)

  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]

  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)

  # Calculate the sum of index values and the sum of source_subset
  sum_index  <- sum(index_values)
  sum_source <- sum(source_subset)

  # Calculate the mean of index values and the mean of source_subset
  mean_index  <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)

  # Calculate the numerator and denominator for the linear regression formula
  numerator   <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)

  # Calculate the slope and intercept of the linear regression line
  slope     <- numerator / denominator
  intercept <- mean_source - slope * mean_index

  # Calculate the predicted values for the regressionSource
  predicted_values <- slope * index_values + intercept

  # Return the slope, intercept, and predicted values as a list
  result <- list(slope            = slope,
                 intercept        = intercept,
                 predicted_values = predicted_values)
  return(result)
}
