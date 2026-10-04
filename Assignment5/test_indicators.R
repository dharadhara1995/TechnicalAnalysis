# BDA400 - Assignment 5: Test file for all indicators
# Student: Dhara Patel
#
# HOW TO RUN: open this file in RStudio, set the working directory to the
# Assignment5 folder (Session > Set Working Directory > To Source File
# Location) and click Source. No packages are needed.
#
# The indicator functions use base R only. This test file uses a few
# built-in base R functions (mean, sd, lm, Reduce) ONLY to check the
# results independently.

# ---- Load every indicator file ----
for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) {
  source(f)
}

results <- c()   # stores PASS/FAIL for the summary at the end
check <- function(name, condition) {
  status <- ifelse(isTRUE(condition), "PASS", "FAIL")
  cat(sprintf("  %-58s %s\n", name, status))
  results[name] <<- status
}
expect_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
section <- function(title) cat("\n==================", title, "==================\n")


# ======================================================================
section("1. SMA")
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_result <- sma(data, period = 3)
print(sma_result)
check("SMA length is n - period + 1", length(sma_result) == length(data) - 3 + 1)
check("SMA matches mean() of each window",
      isTRUE(all.equal(sma_result, sapply(1:7, function(i) mean(data[i:(i + 2)])))))
check("SMA first value = (10+12+15)/3",
      isTRUE(all.equal(sma_result[1], (10 + 12 + 15) / 3)))
check("SMA with period = length gives overall mean", isTRUE(all.equal(sma(data, 9), mean(data))))
check("SMA error when data is shorter than period", expect_error(sma(1:3, 5)))


# ======================================================================
section("2. EMA")
ema_result <- ema(data, period = 3)
print(ema_result)
mult <- 2 / (3 + 1)
ema_check <- Reduce(function(prev, x) prev + mult * (x - prev), data, accumulate = TRUE)
check("EMA length equals data length", length(ema_result) == length(data))
check("EMA first value equals first data point", ema_result[1] == data[1])
check("EMA second value = (12 - 10) * 0.5 + 10 = 11", ema_result[2] == 11)
check("EMA matches independent Reduce() calculation", isTRUE(all.equal(ema_result, ema_check)))
check("EMA of a constant series stays constant", all(ema(rep(5, 10), 4) == 5))


# ======================================================================
section("3. MACD")
macd_data <- c(100, 105, 110, 115, 120, 125, 130)
macd_result <- macd(macd_data, short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)
check("MACD returns macd_line, signal_line, histogram",
      all(c("macd_line", "signal_line", "histogram") %in% names(macd_result)))
check("MACD line = EMA(short) - EMA(long)",
      isTRUE(all.equal(macd_result$macd_line, ema(macd_data, 3) - ema(macd_data, 5))))
check("Signal line = EMA of MACD line",
      isTRUE(all.equal(macd_result$signal_line, ema(macd_result$macd_line, 2))))
check("Histogram = MACD line - signal line",
      isTRUE(all.equal(macd_result$histogram, macd_result$macd_line - macd_result$signal_line)))
check("MACD is positive in a steady uptrend (after day 1)", all(macd_result$macd_line[-1] > 0))


# ======================================================================
section("4. Standard Deviation")
stdev_result <- stdev(data)
print(stdev_result)
check("stdev matches sqrt(mean((x - mean(x))^2))",
      isTRUE(all.equal(stdev_result, sqrt(mean((data - mean(data))^2)))))
check("stdev = sd() * sqrt((n-1)/n) (population vs sample)",
      isTRUE(all.equal(stdev_result, sd(data) * sqrt((length(data) - 1) / length(data)))))
check("stdev of identical values is 0", stdev(rep(7, 5)) == 0)
check("stdev of c(2,4,4,4,5,5,7,9) is exactly 2", stdev(c(2, 4, 4, 4, 5, 5, 7, 9)) == 2)


# ======================================================================
section("5. Linear Regression")
lr <- linreg(data, regressionLength = 9, regressionOffset = 0)
print(lr)
fit <- lm(data ~ seq_along(data))
check("Slope matches lm()", isTRUE(all.equal(lr$slope, unname(coef(fit)[2]))))
check("Intercept matches lm()", isTRUE(all.equal(lr$intercept, unname(coef(fit)[1]))))
check("Predicted values match lm() fitted values",
      isTRUE(all.equal(lr$predicted_values, unname(fitted(fit)))))
check("Perfect line y = 2x + 1 gives slope 2, intercept 1", {
  r <- linreg(2 * (1:10) + 1, 10, 0); isTRUE(all.equal(c(r$slope, r$intercept), c(2, 1)))
})
lr_off <- linreg(data, regressionLength = 5, regressionOffset = 2)
window <- data[3:7]   # last 5 points, skipping the newest 2: 15 20 18 22 25
fit_off <- lm(window ~ seq_along(window))
check("Offset = 2: window has exactly 5 points", length(lr_off$predicted_values) == 5)
check("Offset = 2: slope matches lm() on data[3:7]",
      isTRUE(all.equal(lr_off$slope, unname(coef(fit_off)[2]))))
check("Length 5, offset 0 uses the last 5 points (data[5:9])",
      isTRUE(all.equal(linreg(data, 5, 0)$slope, unname(coef(lm(data[5:9] ~ seq(1, 5)))[2]))))
check("Error when regressionLength > number of elements", expect_error(linreg(data, 20, 0)))
check("Error when regressionOffset >= regressionLength", expect_error(linreg(data, 5, 5)))


# ======================================================================
section("6. RSI")
rsi_data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
rsi_result <- rsi(rsi_data, period = 5)
print(round(rsi_result, 2))
check("RSI length equals data length", length(rsi_result) == length(rsi_data))
check("First 'period' RSI values are NA", all(is.na(rsi_result[1:5])))
check("All RSI values are between 0 and 100",
      all(rsi_result[!is.na(rsi_result)] >= 0 & rsi_result[!is.na(rsi_result)] <= 100))
check("Prices that only go up give RSI = 100", all(rsi(1:20, 5)[6:20] == 100))
check("Prices that only go down give RSI = 0", all(rsi(20:1, 5)[6:20] == 0))
check("Flat prices give RSI = 50 (no division by zero)", all(rsi(rep(10, 10), 3)[4:10] == 50))


# ======================================================================
section("7. Stochastic RSI")
cat("Example from the instructions (period = 14 with only 10 values):\n")
print(suppressWarnings(stoch_rsi(rsi_data, period = 14, k_period = 3, d_period = 3)))
cat("Same data with period = 3 (enough data):\n")
sr <- stoch_rsi(rsi_data, period = 3, k_period = 3, d_period = 3)
print(lapply(sr, round, 3))
check("Warning when there is not enough data for RSI",
      inherits(tryCatch(stoch_rsi(rsi_data, 14, 3, 3), warning = function(w) w), "warning"))
check("%K length = n - k_period + 1", length(sr$k_line) == length(rsi_data) - 3 + 1)
check("%D length = length(%K) - d_period + 1", length(sr$d_line) == length(sr$k_line) - 3 + 1)
check("All %K and %D values are between 0 and 1",
      all(c(sr$k_line, sr$d_line)[!is.na(c(sr$k_line, sr$d_line))] >= 0 &
          c(sr$k_line, sr$d_line)[!is.na(c(sr$k_line, sr$d_line))] <= 1))
check("%D = SMA of %K", isTRUE(all.equal(sr$d_line, sma(sr$k_line, 3))))


# ======================================================================
section("8. Crossover")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
crossover_signals <- crossover(arr1, arr2)
print(crossover_signals)
check("Crossover 'Up' only at point 4 (15<=22, then 20>18)",
      identical(which(crossover_signals == "Up"), 4L))
check("Swapped arrays give 'Down' at point 4",
      identical(which(crossover(arr2, arr1) == "Down"), 4L))
check("First value is always 'None'", crossover_signals[1] == "None")
check("Error when arrays have different lengths", expect_error(crossover(1:5, 1:4)))
check("NA values do not cause an error", !expect_error(crossover(c(NA, 1, 3), c(2, 2, 2))))


# ======================================================================
section("9. Crossunder")
crossunder_signals <- crossunder(arr1, arr2)
print(crossunder_signals)
check("No crossunder in the example (arr1 never goes under arr2)",
      !any(crossunder_signals == "True"))
check("Swapped arrays give a crossunder at point 4",
      identical(which(crossunder(arr2, arr1) == "True"), 4L))
check("First value is 'None'", crossunder_signals[1] == "None")
check("Error when arrays have different lengths", expect_error(crossunder(1:5, 1:4)))


# ======================================================================
section("SUMMARY")
cat(sum(results == "PASS"), "of", length(results), "tests passed\n")
if (any(results == "FAIL")) print(names(results)[results == "FAIL"])
