# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# MACD uses ema(), so load it if it is not loaded yet
if (!exists("ema")) source("ema.R")

# Moving Average Convergence Divergence (MACD)
# data:          numeric vector (e.g. closing prices)
# short_period:  period of the short-term EMA
# long_period:   period of the long-term EMA
# signal_period: period of the signal line (EMA of the MACD line)
# Returns a list with macd_line, signal_line and histogram
# (all the same length as data).
macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema  <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line   = macd_line,
                 signal_line = signal_line,
                 histogram   = histogram)
  return(result)
}
