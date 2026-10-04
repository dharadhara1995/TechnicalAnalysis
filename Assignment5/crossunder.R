# BDA400 - Assignment 5: Technical Analysis using R, Development Phase
# Student: Dhara Patel
# Implemented with base R only (no packages), following the provided
# template and pseudocode.

# Crossunder
# arr1, arr2: numeric vectors of the same length
# Returns a character vector the same length as arr1:
#   "True"  = arr1 crossed under arr2 at this point
#   "False" = no crossunder
#   "None"  = first point (nothing to compare with)
# (as in the pseudocode; use crossunder(a, b) == "True" to get TRUE/FALSE)
crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"

  # Check for crossunder signals at each data point
  if (length(arr1) >= 2) {
    for (i in 2:length(arr1)) {
      # isTRUE() makes points with NA values count as "False"
      if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }

  return(crossunder_signals)
}
