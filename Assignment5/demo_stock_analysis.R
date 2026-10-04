# BDA400 - Assignment 5: Demo on real stock data
# Student: Dhara Patel
#
# Uses my indicator functions on the stocks in portfolio.txt:
#   1. builds a data frame for each stock (Date, Open, High, Low, Close, Volume)
#   2. filters the data by time period
#   3. calculates the indicators
#   4. filters stocks using indicator criteria
#   5. applies simple buy/sell trading rules
#
# quantmod is used ONLY to download the price data (as in Assignment 2).
# All indicators come from my own base R functions.
# Needs an internet connection. Run test_indicators.R first to check the functions.

library(quantmod)
options(width = 120)

for (f in c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
            "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")) {
  source(f)
}

# ---- Settings ----
start_date <- "2025-10-01"
end_date   <- "2026-10-01"
filter_from <- as.Date("2026-04-01")   # time period filter: last 6 months


# ======================================================================
# 1. Build a data frame for each stock
# ======================================================================
symbols <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
symbols <- symbols[symbols != ""]

stocks <- list()
for (sym in symbols) {
  x <- tryCatch(getSymbols(sym, src = "yahoo", from = start_date, to = end_date,
                           auto.assign = FALSE),
                error = function(e) { cat("Could not download", sym, "\n"); NULL })
  if (!is.null(x)) {
    df <- data.frame(Date = index(x), coredata(x)[, 1:5])
    names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
    stocks[[sym]] <- df
    cat("Loaded", sym, "-", nrow(df), "rows\n")
  }
}


# ======================================================================
# 2-3. Filter by time period and add indicators
# ======================================================================
add_indicators <- function(df) {
  close <- df$Close
  m <- macd(close, short_period = 12, long_period = 26, signal_period = 9)
  df$EMA_20      <- ema(close, 20)
  df$SMA_20      <- c(rep(NA, 19), sma(close, 20))   # pad so it lines up with the dates
  df$RSI_14      <- rsi(close, 14)
  df$MACD        <- m$macd_line
  df$MACD_Signal <- m$signal_line
  df$MACD_Hist   <- m$histogram
  df$Buy  <- crossover(df$MACD, df$MACD_Signal) == "Up"      # trading rule: buy
  df$Sell <- crossunder(df$MACD, df$MACD_Signal) == "True"   # trading rule: sell
  df
}

# Indicators are calculated on the full year (so the EMAs have time to
# settle) and then the data is filtered to the chosen time period
analysis <- lapply(stocks, function(df) {
  df <- add_indicators(df)
  df[df$Date >= filter_from, ]
})


# ======================================================================
# 4. Summary table and stock filter
# ======================================================================
summary_table <- do.call(rbind, lapply(names(analysis), function(sym) {
  df   <- analysis[[sym]]
  last <- df[nrow(df), ]
  trend <- linreg(df$Close, regressionLength = 20, regressionOffset = 0)
  data.frame(
    Symbol      = sym,
    Last_Close  = round(last$Close, 2),
    RSI_14      = round(last$RSI_14, 1),
    MACD_Hist   = round(last$MACD_Hist, 3),
    Trend_Slope = round(trend$slope, 3),           # 20-day regression slope
    Volatility  = round(stdev(df$Close), 2),       # stdev of closing price
    Buy_Signals = sum(df$Buy),
    Sell_Signals = sum(df$Sell)
  )
}))
cat("\n===== Indicator summary (", format(filter_from), "to end ) =====\n")
print(summary_table, row.names = FALSE)

# My stock filter: upward trend, positive MACD momentum, and not overbought
cat("\n===== Stocks that pass my filter =====\n")
cat("Rule: Trend_Slope > 0 AND MACD_Hist > 0 AND RSI_14 < 70\n")
passed <- subset(summary_table, Trend_Slope > 0 & MACD_Hist > 0 & RSI_14 < 70)
if (nrow(passed) == 0) cat("No stocks pass the filter today.\n") else print(passed, row.names = FALSE)


# ======================================================================
# 5. Trading signals and charts
# ======================================================================
for (sym in names(analysis)) {
  df <- analysis[[sym]]
  cat("\n=====", sym, "trading signals =====\n")
  signals <- df[df$Buy | df$Sell, c("Date", "Close", "RSI_14", "Buy", "Sell")]
  signals$Close  <- round(signals$Close, 2)
  signals$RSI_14 <- round(signals$RSI_14, 1)
  if (nrow(signals) == 0) cat("No signals in this period\n") else print(signals, row.names = FALSE)

  par(mfrow = c(2, 1), mar = c(3, 4, 2, 1))
  plot(df$Date, df$Close, type = "l", main = paste(sym, "- Close, EMA 20 and signals"),
       xlab = "", ylab = "Price")
  lines(df$Date, df$EMA_20, col = "blue")
  points(df$Date[df$Buy],  df$Close[df$Buy],  pch = 24, bg = "green", cex = 1.3)
  points(df$Date[df$Sell], df$Close[df$Sell], pch = 25, bg = "red",   cex = 1.3)
  legend("topleft", c("Close", "EMA 20", "Buy", "Sell"), col = c("black", "blue", "black", "black"),
         lty = c(1, 1, NA, NA), pch = c(NA, NA, 24, 25), pt.bg = c(NA, NA, "green", "red"),
         bty = "n", cex = 0.8)
  plot(df$Date, df$MACD, type = "l", col = "blue", main = "MACD (12, 26, 9)",
       xlab = "", ylab = "MACD", ylim = range(c(df$MACD, df$MACD_Signal, df$MACD_Hist)))
  lines(df$Date, df$MACD_Signal, col = "red")
  abline(h = 0, lty = 2)
}
par(mfrow = c(1, 1))
