# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Crossover
# arr1, arr2: numeric vectors of the same length
# Returns a character vector the same length as arr1:
#   "Up"   = arr1 crossed above arr2 at this point
#   "Down" = arr1 crossed below arr2 at this point
#   "None" = no cross (always "None" for the first point)
# (as in the pseudocode; use crossover(a, b) == "Up" to get TRUE/FALSE)
crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- rep("None", length(arr1))

  # Check for crossovers at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      # isTRUE() makes points with NA values count as "None"
      if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1])) {
        crossover_signals[i] <- "Up"
      } else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }

  return(crossover_signals)
}
