# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Stochastic RSI uses rsi() and sma(), so load them if needed
if (!exists("rsi")) source("rsi.R")
if (!exists("sma")) source("sma.R")

# Stochastic RSI (StochRSI)
# data:     numeric vector (e.g. closing prices)
# period:   RSI period
# k_period: SMA period for the %K line
# d_period: SMA period for the %D line
# Returns a list with k_line and d_line (values between 0 and 1).
# Both start with NA values because the first 'period' RSI values are NA.
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # The first 'period' RSI values are NA, so ignore them for min and max
  if (all(is.na(rsi_values))) {
    warning("Not enough data to calculate RSI with this period. Returning NA.")
    return(list(k_line = NA_real_, d_line = NA_real_))
  }

  # Calculate the StochRSI (normalize RSI between 0 and 1)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  if (max_rsi == min_rsi) {
    k_values <- ifelse(is.na(rsi_values), NA, 0)   # flat RSI: avoid dividing by 0
  } else {
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}
