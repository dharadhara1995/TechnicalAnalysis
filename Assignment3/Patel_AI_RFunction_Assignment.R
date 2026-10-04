# ======================================================================
# BDA400 - Assignment 3: Prompting R Functions with AI
# Student: Dhara [Last Name]
#
# AI Assistance Declaration: I used Claude (Claude Opus 5.5, claude.ai)
# to generate and revise an R function that removes outliers, and to
# explain the code. Prompts used: listed below and in full in
# LastName_Appendix.docx. I verified outputs using a manual calculation
# in Excel, a comparison with boxplot.stats(), and edge-case tests that I
# ran in RStudio Desktop. All final calculations are done by myself.
# I am responsible for the accuracy and originality of this work.
#
# ----------------------------------------------------------------------
# ORIGINAL TASK DESCRIPTION
# "Remove outliers from a numeric vector using the 1.5 x IQR rule and
#  report which values were removed."
#
# ----------------------------------------------------------------------
# PROMPTS USED (in order)
#  P1 (seed):  Write an R function that removes outliers from a numeric
#              vector using the 1.5 x IQR rule and reports which values
#              were removed.
#  P2:         Revise the function to include comments, argument
#              validation, and return a clean result.
#  P3:         Explain what each line of the R code does and how you
#              might test if it works correctly.
#  P4:         Update the function to handle NA values. Right now NA
#              values stay in the cleaned result as NA.
#  P5:         My result does not match boxplot.stats() for
#              c(34, 16, 46, 37, 55, 40). Why, and how can I make the
#              function match it?
#  P6:         Optimize the function for efficiency and readability.
#  P7:         How could you test this function with edge cases?
#  P8:         Summarize how this R function changed across revisions.
#              Highlight what improvements were human-driven vs.
#              AI-generated.
#
# VERSION HISTORY
#  v1 - AI output from P1. Breaks when the data has NA.
#  v2 - AI output from P2. Validation added, but NA shows up in result.
#  v3 - AI output from P4. NA handled properly.
#  v4 - FINAL. Option to match boxplot.stats() (from P5), edge cases
#       (from P6/P7), returns a list with clean data and removed values.
# ======================================================================


# ======================================================================
# VERSION 1 - AI-generated (seed prompt P1), kept as-is for comparison
# ======================================================================
remove_outliers_v1 <- function(x) {
  Q1 <- quantile(x, 0.25)          # 25th percentile (first quartile)
  Q3 <- quantile(x, 0.75)          # 75th percentile (third quartile)
  IQR <- Q3 - Q1                   # interquartile range (note: this overwrites the name of R's IQR() function)
  lower <- Q1 - 1.5 * IQR          # lower fence
  upper <- Q3 + 1.5 * IQR          # upper fence
  removed <- x[x < lower | x > upper]  # values outside the fences
  print(paste("Removed:", paste(removed, collapse = ", ")))  # prints the removed values
  x[x >= lower & x <= upper]       # returns values inside the fences
}
# Problem found in testing: quantile() stops with an error if x has NA.


# ======================================================================
# VERSION 2 - AI-generated (refinement prompt P2)
# ======================================================================
remove_outliers_v2 <- function(x) {
  # Check that the input is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  # Quartiles, ignoring missing values
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- q3 - q1
  # Fences
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  # Return values inside the fences without names
  unname(x[x >= lower & x <= upper])
}
# Problem found in testing: no error any more, but NA values come back
# as NA in the result, because NA >= lower gives NA and x[NA] = NA.
# Also the "report removed values" part from my task was dropped.


# ======================================================================
# VERSION 3 - AI-generated (prompt P4: handle NA values)
# ======================================================================
remove_outliers_v3 <- function(x) {
  if (!is.numeric(x)) stop("Input must be a numeric vector.")
  n_na <- sum(is.na(x))                 # count missing values
  x <- x[!is.na(x)]                     # drop missing values first
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr_value <- q3 - q1
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  is_out <- x < lower | x > upper
  message(n_na, " NA value(s) removed, ", sum(is_out), " outlier(s) removed.")
  unname(x[!is_out])
}
# Problem found in testing: result did not match boxplot.stats() for
# c(34, 16, 46, 37, 55, 40). quantile() and boxplot.stats() calculate
# the quartiles in different ways (see v4 notes).


# ======================================================================
# VERSION 4 - FINAL FUNCTION
# ======================================================================
# remove_outliers()
#   Removes outliers from a numeric vector using the 1.5 x IQR rule.
#
# Arguments:
#   x        numeric vector
#   coef     how many IQRs past the quartiles counts as an outlier (1.5)
#   method   "quantile" = quartiles from quantile() (same as Excel
#                         QUARTILE.INC)
#            "tukey"    = Tukey hinges from fivenum(), the same method
#                         boxplot.stats() and boxplot() use
#   na.rm    TRUE = drop NA values before checking for outliers
#
# Returns a list with:
#   clean     the vector without outliers (and without NA)
#   outliers  the values that were removed
#   lower, upper   the fences that were used
#   n_na      how many NA values were dropped
# ----------------------------------------------------------------------
remove_outliers <- function(x, coef = 1.5,
                            method = c("quantile", "tukey"),
                            na.rm = TRUE) {

  # --- Argument validation ---
  if (!is.numeric(x)) {                                   # stop if text, factor, etc.
    stop("'x' must be a numeric vector.")
  }
  if (!is.numeric(coef) || length(coef) != 1 || coef < 0) {  # coef must be one positive number
    stop("'coef' must be a single non-negative number.")
  }
  method <- match.arg(method)                             # only allow "quantile" or "tukey"

  # --- Handle missing values ---
  n_na <- sum(is.na(x))                                   # count NA values
  if (n_na > 0 && !na.rm) {                               # user asked not to remove NA
    stop("'x' has ", n_na, " NA value(s). Use na.rm = TRUE to drop them.")
  }
  x <- x[!is.na(x)]                                       # keep only real numbers

  # --- Edge case: nothing left to check ---
  if (length(x) == 0) {                                   # empty or all NA
    warning("No non-missing values in 'x'. Returning empty result.")
    return(list(clean = numeric(0), outliers = numeric(0),
                lower = NA_real_, upper = NA_real_, n_na = n_na))
  }
  if (length(x) < 4) {                                    # quartiles are not meaningful
    warning("Fewer than 4 values: outlier check may not be reliable.")
  }

  # --- Quartiles ---
  if (method == "quantile") {
    q <- quantile(x, c(0.25, 0.75), names = FALSE)        # Q1 and Q3
  } else {
    q <- fivenum(x)[c(2, 4)]                              # lower and upper hinge
  }
  iqr_value <- q[2] - q[1]                                # interquartile range

  # --- Fences and outlier check ---
  lower  <- q[1] - coef * iqr_value                       # lower fence
  upper  <- q[2] + coef * iqr_value                       # upper fence
  is_out <- x < lower | x > upper                         # TRUE for outliers

  # --- Clean result ---
  list(clean    = x[!is_out],                             # values kept
       outliers = x[is_out],                              # values removed
       lower    = lower,
       upper    = upper,
       n_na     = n_na)
}


# ======================================================================
# TESTS (run this section in RStudio Desktop)
# ======================================================================
cat("\n===== TEST 1: basic example from the instructions =====\n")
sales <- c(10, 15, 999, 20, 25)
print(remove_outliers(sales))
# Manual check: I calculated Q1, Q3, IQR and the fences in Excel
# (QUARTILE.INC). 999 should be the only outlier.

cat("\n===== TEST 2: data with NA, all versions =====\n")
sales_na <- c(10, 15, NA, 999, 20, 25)
cat("v1: "); print(tryCatch(remove_outliers_v1(sales_na),
                            error = function(e) paste("ERROR:", conditionMessage(e))))
cat("v2: "); print(remove_outliers_v2(sales_na))
cat("v3: "); print(remove_outliers_v3(sales_na))
cat("v4: "); print(remove_outliers(sales_na)$clean)

cat("\n===== TEST 3: comparison with boxplot.stats() =====\n")
delivery_mins <- c(34, 16, 46, 37, 55, 40)
cat("boxplot.stats() outliers:      "); print(boxplot.stats(delivery_mins)$out)
cat("method = 'quantile' outliers:  "); print(remove_outliers(delivery_mins)$outliers)
cat("method = 'tukey' outliers:     "); print(remove_outliers(delivery_mins, method = "tukey")$outliers)

cat("\n===== TEST 4: built-in iris dataset (Sepal.Width) =====\n")
iris_result <- remove_outliers(iris$Sepal.Width, method = "tukey")
cat("Outliers found: "); print(iris_result$outliers)
cat("Same as boxplot.stats()? ",
    identical(sort(iris_result$outliers), sort(boxplot.stats(iris$Sepal.Width)$out)), "\n")
cat("Values kept:", length(iris_result$clean), "of", length(iris$Sepal.Width), "\n")

cat("\n===== TEST 5: edge cases =====\n")
cat("No outliers:     "); print(remove_outliers(c(5, 6, 7, 8, 9))$outliers)
cat("All same values: "); print(remove_outliers(c(3, 3, 3, 3))$clean)
cat("Negative values: "); print(remove_outliers(c(-50, -2, -1, 0, 1, 2))$outliers)
cat("All NA:          "); print(withCallingHandlers(remove_outliers(c(NA_real_, NA_real_))$clean,
                                 warning = function(w) { cat("[warning]", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }))
cat("Text input:      "); print(tryCatch(remove_outliers(c("a", "b")),
                                         error = function(e) paste("ERROR:", conditionMessage(e))))
cat("na.rm = FALSE:   "); print(tryCatch(remove_outliers(sales_na, na.rm = FALSE),
                                         error = function(e) paste("ERROR:", conditionMessage(e))))
cat("Bad coef:        "); print(tryCatch(remove_outliers(sales, coef = -1),
                                         error = function(e) paste("ERROR:", conditionMessage(e))))
