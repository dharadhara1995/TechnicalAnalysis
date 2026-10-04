# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Relative Strength Index (RSI)
# data:   numeric vector (e.g. closing prices)
# period: RSI period (usually 14)
# Returns a vector the same length as data. The first 'period' values are
# NA because there is not enough data yet. Values are between 0 and 100.
rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses (0 by default)
  gains  <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))

  # If there are not enough price changes, every RSI value stays NA
  if (length(diff_values) < period) {
    return(rsi_values)
  }

  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    if (avg_loss == 0 && avg_gain == 0) {
      rsi_values[i] <- 50             # no movement at all: neutral
    } else if (avg_loss == 0) {
      rsi_values[i] <- 100            # only gains: RS is infinite
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }

  return(rsi_values)
}
