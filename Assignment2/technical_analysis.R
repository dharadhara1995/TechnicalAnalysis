# ============================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# Author: Dhara
#
# AI Assistance Declaration: I used Claude (Claude Opus 5.5, claude.ai)
# to help draft and explain the functions in this script. I ran the
# script myself in RStudio, checked that every stock loaded, and
# spot-checked the statistics. I am responsible for the accuracy and
# originality of this work.
# ============================================================

# ---- Packages ----
# Run this line once if the packages are not installed yet:
# install.packages(c("quantmod", "TTR"))
library(quantmod)   # downloads stock data from Yahoo Finance
library(TTR)        # technical indicators like moving averages


# ---- Settings ----
portfolio_file <- "portfolio.txt"
start_date     <- "2025-10-01"   # one year of data
end_date       <- "2026-10-01"
ma_days        <- 20             # days for the moving average


# ============================================================
# 1. Read the portfolio file
# ============================================================
read_portfolio <- function(file) {
  symbols <- readLines(file, warn = FALSE)
  symbols <- toupper(trimws(symbols))   # remove spaces, make uppercase
  symbols <- symbols[symbols != ""]     # remove empty lines
  return(symbols)
}


# ============================================================
# 2. Import and load the stock data
# ============================================================
# Reads portfolio.txt, downloads each stock with quantmod and
# returns a list with one data frame per symbol. Each data frame
# is also saved as its own object (e.g. AAPL_df) so it shows up
# separately in the RStudio Environment pane.
load_stock_data <- function(file = portfolio_file,
                            from = start_date, to = end_date) {
  symbols    <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    cat("Downloading", sym, "...\n")
    stock_xts <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        cat("  Could not download", sym, "-", conditionMessage(e), "\n")
        NULL
      }
    )

    if (!is.null(stock_xts)) {
      # convert from xts to a normal data frame
      stock_df <- data.frame(Date = index(stock_xts), coredata(stock_xts))
      names(stock_df) <- c("Date", "Open", "High", "Low",
                           "Close", "Volume", "Adjusted")
      stock_list[[sym]] <- stock_df
      assign(paste0(make.names(sym), "_df"), stock_df, envir = .GlobalEnv)
      cat("  Loaded", nrow(stock_df), "rows for", sym, "\n")
    }
  }
  return(stock_list)
}


# ============================================================
# 3. Statistics functions
# ============================================================

# Mode: stock prices almost never repeat exactly (e.g. 231.47),
# so I round to the nearest dollar first and then find the
# price level that appears most often.
get_mode <- function(x) {
  x_rounded <- round(x, 0)
  counts    <- table(x_rounded)
  as.numeric(names(counts)[which.max(counts)])
}

# Adds 20-day and 50-day simple moving averages to a stock data frame
add_moving_averages <- function(stock_df) {
  stock_df$SMA_20 <- SMA(stock_df$Close, n = 20)
  stock_df$SMA_50 <- SMA(stock_df$Close, n = 50)
  return(stock_df)
}

# Calculates the statistics for one stock (based on closing price)
calculate_statistics <- function(stock_df, symbol, n = ma_days) {
  close_prices <- stock_df$Close
  moving_avg   <- SMA(close_prices, n = n)

  data.frame(
    Symbol         = symbol,
    Days           = length(close_prices),
    Latest_Close   = tail(close_prices, 1),
    Moving_Avg_20  = tail(moving_avg, 1),   # latest 20-day moving average
    Mean           = mean(close_prices, na.rm = TRUE),
    Median         = median(close_prices, na.rm = TRUE),
    Mode           = get_mode(close_prices),
    Std_Dev        = sd(close_prices, na.rm = TRUE)
  )
}

# Runs calculate_statistics() for every stock and combines the
# results into one data frame (one row per stock)
calculate_all_statistics <- function(stock_list) {
  results <- lapply(names(stock_list), function(sym) {
    calculate_statistics(stock_list[[sym]], sym)
  })
  do.call(rbind, results)
}


# ============================================================
# 4. Display functions
# ============================================================

# Shows a summary and the first/last rows of each stock
display_stock_data <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    cat("\n==============================\n")
    cat(" ", sym, "\n")
    cat("==============================\n")
    cat("Date range:", format(min(df$Date)), "to", format(max(df$Date)),
        "|", nrow(df), "trading days\n\n")
    cat("First", n, "rows:\n")
    print(head(df, n), row.names = FALSE)
    cat("\nLast", n, "rows:\n")
    print(tail(df, n), row.names = FALSE)
  }
}

# Shows the statistics table rounded to 2 decimals
display_statistics <- function(stats_df) {
  cat("\n===== Statistics (Closing Price, USD) =====\n")
  out <- stats_df
  num_cols <- sapply(out, is.numeric)
  out[num_cols] <- round(out[num_cols], 2)
  print(out, row.names = FALSE)
}

# Line chart: closing price with 20-day and 50-day moving averages
plot_price_with_ma <- function(stock_df, symbol) {
  stock_df <- add_moving_averages(stock_df)
  plot(stock_df$Date, stock_df$Close, type = "l", col = "black", lwd = 1.5,
       main = paste(symbol, "- Closing Price and Moving Averages"),
       xlab = "Date", ylab = "Price (USD)")
  lines(stock_df$Date, stock_df$SMA_20, col = "blue",  lwd = 1.5)
  lines(stock_df$Date, stock_df$SMA_50, col = "red",   lwd = 1.5)
  abline(h = mean(stock_df$Close), col = "darkgreen", lty = 2)
  legend("topleft", legend = c("Close", "20-day MA", "50-day MA", "Mean"),
         col = c("black", "blue", "red", "darkgreen"),
         lty = c(1, 1, 1, 2), bty = "n", cex = 0.8)
}

# Candlestick chart with volume, similar to Yahoo Finance
plot_candlestick <- function(stock_df, symbol) {
  stock_xts <- xts(stock_df[, c("Open", "High", "Low", "Close", "Volume")],
                   order.by = stock_df$Date)
  chartSeries(stock_xts, name = symbol, theme = chartTheme("white"),
              TA = "addVo(); addSMA(n = 20, col = 'blue')")
}

# Bar chart comparing the mean price of each stock (with SD shown)
plot_statistics <- function(stats_df) {
  bars <- barplot(stats_df$Mean, names.arg = stats_df$Symbol,
                  col = "steelblue", ylim = c(0, max(stats_df$Mean + stats_df$Std_Dev) * 1.1),
                  main = "Mean Closing Price by Stock (+/- 1 SD)",
                  ylab = "Price (USD)")
  arrows(bars, stats_df$Mean - stats_df$Std_Dev,
         bars, stats_df$Mean + stats_df$Std_Dev,
         angle = 90, code = 3, length = 0.05)
}


# ============================================================
# 5. Run the analysis
# ============================================================
stocks <- load_stock_data()

display_stock_data(stocks)

stock_stats <- calculate_all_statistics(stocks)
display_statistics(stock_stats)

# Charts - use the arrows in the RStudio Plots pane to go through them
for (sym in names(stocks)) {
  plot_price_with_ma(stocks[[sym]], sym)
  plot_candlestick(stocks[[sym]], sym)
}
plot_statistics(stock_stats)
