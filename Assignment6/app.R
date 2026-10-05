# ======================================================================
#
# AI Assistance Declaration: I used Claude (Claude Opus 5.5, claude.ai) to
# help write and explain this Shiny app and the cover page. I ran the app
# myself in RStudio and checked the charts, indicators and signals for
# each stock. I am responsible for the accuracy and originality of this work.
#
# HOW TO RUN
#   1. Install the packages once (Step 1 below).
#   2. Open this file in RStudio and click "Run App" (top right of the
#      editor), or run: shiny::runApp("path/to/Assignment6")
#   3. An internet connection is needed for Yahoo Finance. If Yahoo is not
#      available, the app uses the last saved copy of the data
#      (data_cache folder) or you can upload a CSV file.
#
# CODE SECTIONS (same order as the assignment instructions)
#   Step 1: Data collection and setup
#   Step 2: Visualizing stock data (UI widgets + chart functions)
#   Step 3: Overlay technical indicators (functions from Assignment 5)
#   Step 4: Trading rules and annotations
#   Shiny UI, Shiny server, run the app
# ======================================================================


# ======================================================================
# STEP 1: DATA COLLECTION AND SETUP
# ======================================================================

# ---- 1a. Install and load packages ----
# Run this once if the packages are not installed:
# install.packages(c("shiny", "ggplot2", "quantmod", "patchwork"))
library(shiny)      # web dashboard
library(ggplot2)    # charts
library(quantmod)   # download stock data from Yahoo Finance
library(patchwork)  # stack the price, RSI, MACD and volume panels in one chart

# ---- 1b. Data sources ----
# Main source: Yahoo Finance (through quantmod).
# Backup 1:    local cache - every successful download is saved in the
#              data_cache folder and used if Yahoo cannot be reached.
# Backup 2:    a CSV file uploaded by the user (Date, Open, High, Low,
#              Close, Volume columns, e.g. exported from Yahoo Finance).
cache_dir <- "data_cache"

# Stock list from my portfolio.txt (Assignment 2). Any other symbol can
# also be typed into the app.
if (file.exists("portfolio.txt")) {
  portfolio <- toupper(trimws(readLines("portfolio.txt", warn = FALSE)))
  portfolio <- portfolio[portfolio != ""]
} else {
  portfolio <- c("AAPL", "MSFT", "AMZN", "TSLA", "NVDA")
}

# ---- 1c. Clean OHLCV data ----
# Keeps Open/High/Low/Close/Volume, gives them standard names and removes
# rows with missing prices (Yahoo sometimes returns NA rows).
clean_ohlcv <- function(x) {
  x <- x[, 1:5]
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
  n_before <- nrow(x)
  x <- x[complete.cases(x[, c("Open", "High", "Low", "Close")]), ]
  x$Volume[is.na(x$Volume)] <- 0
  attr(x, "rows_removed") <- n_before - nrow(x)
  x
}

# ---- 1d. Fetch historical stock data (with error handling) ----
# Returns a list: data (xts), source (text shown in the app), removed (NA rows)
fetch_stock_data <- function(symbol, from, to) {
  symbol <- toupper(trimws(symbol))
  if (symbol == "") stop("Please enter a stock symbol.")

  yahoo <- tryCatch(
    suppressWarnings(getSymbols(symbol, src = "yahoo", from = from,
                                to = as.Date(to) + 1,      # 'to' is exclusive in Yahoo
                                auto.assign = FALSE)),
    error = function(e) NULL
  )

  cache_file <- file.path(cache_dir, paste0(symbol, ".rds"))

  if (!is.null(yahoo) && nrow(yahoo) > 0) {
    data <- clean_ohlcv(yahoo)
    dir.create(cache_dir, showWarnings = FALSE)
    saveRDS(data, cache_file)                               # save a backup copy
    source_text <- "Yahoo Finance (live)"
  } else if (file.exists(cache_file)) {
    data <- readRDS(cache_file)
    data <- data[index(data) >= as.Date(from) & index(data) <= as.Date(to)]
    source_text <- "Local cache (Yahoo Finance could not be reached)"
  } else {
    stop("Could not download '", symbol, "' from Yahoo Finance and no saved copy exists. ",
         "Check the symbol and your internet connection, or upload a CSV file.")
  }

  if (nrow(data) == 0) stop("No data for '", symbol, "' in the selected date range.")
  list(data = data, source = source_text, removed = attr(data, "rows_removed"))
}

# Read an uploaded CSV file (e.g. downloaded from Yahoo Finance)
read_csv_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- trimws(names(df))
  needed <- c("Date", "Open", "High", "Low", "Close", "Volume")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("CSV is missing column(s): ", paste(missing_cols, collapse = ", "))
  }
  df$Date <- as.Date(df$Date)
  for (col in needed[-1]) df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df <- df[!is.na(df$Date), ]
  x <- xts(df[, needed[-1]], order.by = df$Date)
  data <- clean_ohlcv(x)
  list(data = data, source = "Uploaded CSV file", removed = attr(data, "rows_removed"))
}


# ======================================================================
# STEP 2: VISUALIZING STOCK DATA - time frame and data frame helpers
# ======================================================================

# Converts daily data to weekly or monthly bars
apply_time_frame <- function(x, time_frame) {
  switch(time_frame,
         "Daily"   = x,
         "Weekly"  = to.weekly(x, name = NULL, indexAt = "endof"),
         "Monthly" = to.monthly(x, name = NULL, indexAt = "endof"))
}

# xts -> data frame with a Date column (easier to use with ggplot2)
to_df <- function(x) {
  df <- data.frame(Date = as.Date(index(x)), coredata(x))
  names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume")
  df
}


# ======================================================================
# STEP 3: TECHNICAL INDICATORS
# My own base R functions from Assignment 5 (same logic as sma.R, ema.R,
# rsi.R, macd.R, crossover.R, crossunder.R), copied here so this one
# script has all the code.
# ======================================================================
sma <- function(data, period) {
  if (length(data) < period) stop("Data length should be greater than or equal to the period")
  n_values <- length(data) - period + 1
  sma_values <- numeric(n_values)
  for (i in 1:n_values) sma_values[i] <- sum(data[i:(i + period - 1)]) / period
  sma_values
}

ema <- function(data, period) {
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  for (i in seq_along(data)) {
    ema_values[i] <- if (i == 1) data[i] else (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
  }
  ema_values
}

rsi <- function(data, period) {
  diff_values <- diff(data)
  gains  <- ifelse(diff_values > 0, diff_values, 0)
  losses <- ifelse(diff_values < 0, abs(diff_values), 0)
  rsi_values <- rep(NA_real_, length(data))
  if (length(diff_values) < period) return(rsi_values)
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rsi_values[i] <- if (avg_loss == 0 && avg_gain == 0) 50
                     else if (avg_loss == 0) 100
                     else 100 - 100 / (1 + avg_gain / avg_loss)
  }
  rsi_values
}

macd <- function(data, short_period, long_period, signal_period) {
  macd_line   <- ema(data, short_period) - ema(data, long_period)
  signal_line <- ema(macd_line, signal_period)
  list(macd_line = macd_line, signal_line = signal_line, histogram = macd_line - signal_line)
}

crossover <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  signals <- rep("None", length(arr1))
  if (length(arr1) >= 2) for (i in 2:length(arr1)) {
    if (isTRUE(arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]))      signals[i] <- "Up"
    else if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) signals[i] <- "Down"
  }
  signals
}

crossunder <- function(arr1, arr2) {
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length")
  signals <- rep("False", length(arr1)); signals[1] <- "None"
  if (length(arr1) >= 2) for (i in 2:length(arr1)) {
    if (isTRUE(arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1])) signals[i] <- "True"
  }
  signals
}

# Moving average padded with NA at the start so it lines up with the dates
moving_average <- function(x, n, type = "SMA") {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  if (type == "EMA") return(ema(x, n))
  c(rep(NA_real_, n - 1), sma(x, n))
}

# Adds every indicator column to the data frame
add_indicators <- function(df, ma_type, short_n, long_n, rsi_n, fast, slow, sig) {
  df$MA_Short <- moving_average(df$Close, short_n, ma_type)
  df$MA_Long  <- moving_average(df$Close, long_n, ma_type)
  df$RSI      <- rsi(df$Close, rsi_n)
  m <- macd(df$Close, fast, slow, sig)
  df$MACD        <- m$macd_line
  df$MACD_Signal <- m$signal_line
  df$MACD_Hist   <- m$histogram
  df
}


# ======================================================================
# STEP 4: TRADING RULES
# Buy / Sell is marked on the bar where the rule is triggered, every other
# bar is Hold. Rules and their parameters can be changed in the app.
#   MA Crossover:   Buy when the short MA crosses above the long MA,
#                   Sell when it crosses below.
#   MACD Crossover: Buy when MACD crosses above its signal line,
#                   Sell when it crosses below.
#   RSI Reversal:   Buy when RSI moves back above the oversold level,
#                   Sell when RSI drops back below the overbought level.
#   MA + RSI Filter: MA crossover Buy only if RSI is below the overbought
#                   level (don't buy when overbought); Sell on an MA cross
#                   down OR when RSI falls back below the overbought level.
# ======================================================================
generate_signals <- function(df, rule, rsi_low, rsi_high) {
  n <- nrow(df)
  ma_cross   <- crossover(df$MA_Short, df$MA_Long)
  macd_cross <- crossover(df$MACD, df$MACD_Signal)
  rsi_up     <- crossover(df$RSI, rep(rsi_low, n)) == "Up"
  rsi_down   <- crossunder(df$RSI, rep(rsi_high, n)) == "True"

  buy <- switch(rule,
    "MA Crossover"    = ma_cross == "Up",
    "MACD Crossover"  = macd_cross == "Up",
    "RSI Reversal"    = rsi_up,
    "MA + RSI Filter" = ma_cross == "Up" & !is.na(df$RSI) & df$RSI < rsi_high)
  sell <- switch(rule,
    "MA Crossover"    = ma_cross == "Down",
    "MACD Crossover"  = macd_cross == "Down",
    "RSI Reversal"    = rsi_down,
    "MA + RSI Filter" = ma_cross == "Down" | rsi_down)

  df$Signal <- ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
  df
}

# Simple back-test: buy at the close of a Buy bar, sell at the close of a
# Sell bar, one position at a time. Compared with just buying and holding.
backtest <- function(df) {
  in_trade <- FALSE; entry <- NA; growth <- 1; trades <- 0
  for (i in seq_len(nrow(df))) {
    if (!in_trade && df$Signal[i] == "Buy") {
      in_trade <- TRUE; entry <- df$Close[i]
    } else if (in_trade && df$Signal[i] == "Sell") {
      in_trade <- FALSE; growth <- growth * df$Close[i] / entry; trades <- trades + 1
    }
  }
  if (in_trade) { growth <- growth * df$Close[nrow(df)] / entry; trades <- trades + 1 }
  data.frame(
    Measure = c("Buy signals", "Sell signals", "Completed trades",
                "Strategy return", "Buy & hold return"),
    Value = c(sum(df$Signal == "Buy"), sum(df$Signal == "Sell"), trades,
              sprintf("%.1f%%", (growth - 1) * 100),
              sprintf("%.1f%%", (df$Close[nrow(df)] / df$Close[1] - 1) * 100))
  )
}


# ======================================================================
# CHART FUNCTIONS (Steps 2-4: chart type, indicator layers, annotations)
# ======================================================================
build_chart <- function(df, symbol, chart_type, indicators, ma_type, short_n, long_n,
                        rsi_low, rsi_high, show_signals, label_all, shade_trend) {
  bar_width <- if (nrow(df) > 1) as.numeric(median(diff(df$Date))) * 0.35 else 0.4
  df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")

  # ---- Price panel: chart type ----
  p <- ggplot(df, aes(x = Date))

  # Optional background shading: green when the short MA is above the long MA
  if (shade_trend && "Moving Averages" %in% indicators) {
    trend <- df[!is.na(df$MA_Short) & !is.na(df$MA_Long), ]
    trend$Uptrend <- trend$MA_Short > trend$MA_Long
    trend$Next    <- c(trend$Date[-1], max(trend$Date) + 1)   # each band runs to the next bar (no gaps on weekends)
    p <- p + geom_rect(data = trend,
                       aes(xmin = Date, xmax = Next,
                           ymin = -Inf, ymax = Inf, fill = Uptrend),
                       alpha = 0.08, inherit.aes = FALSE, show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = "darkgreen", `FALSE` = "firebrick"))
  }

  if (chart_type == "Line") {
    p <- p + geom_line(aes(y = Close, colour = "Close"), linewidth = 0.7)
  } else if (chart_type == "Area") {
    p <- p + geom_ribbon(aes(ymin = min(Low) * 0.98, ymax = Close), fill = "steelblue", alpha = 0.25) +
             geom_line(aes(y = Close, colour = "Close"), linewidth = 0.7)
  } else {  # Candlestick
    p <- p + geom_segment(aes(xend = Date, y = Low, yend = High, colour = Direction),
                          linewidth = 0.4, show.legend = FALSE) +
             geom_rect(aes(xmin = Date - bar_width, xmax = Date + bar_width,
                           ymin = pmin(Open, Close), ymax = pmax(Open, Close), colour = Direction),
                       fill = ifelse(df$Direction == "Up", "#2E8B57", "#C0392B"),
                       linewidth = 0.2, show.legend = FALSE)
  }

  # ---- Step 3: Moving average layers (on/off) ----
  if ("Moving Averages" %in% indicators) {
    p <- p + geom_line(aes(y = MA_Short, colour = paste0(ma_type, " ", short_n)), linewidth = 0.8, na.rm = TRUE) +
             geom_line(aes(y = MA_Long,  colour = paste0(ma_type, " ", long_n)),  linewidth = 0.8, na.rm = TRUE)
  }

  # ---- Step 4: Signal annotations on the bars ----
  if (show_signals) {
    buys  <- df[df$Signal == "Buy", ]
    sells <- df[df$Signal == "Sell", ]
    offset <- diff(range(c(df$Low, df$High))) * 0.04
    p <- p +
      geom_point(data = buys,  aes(y = Low - offset),  shape = 24, size = 3.2, fill = "#2E8B57", colour = "black") +
      geom_text(data = buys,   aes(y = Low - offset * 2.3, label = "Buy"),  colour = "#2E8B57", size = 3.3, fontface = "bold") +
      geom_point(data = sells, aes(y = High + offset), shape = 25, size = 3.2, fill = "#C0392B", colour = "black") +
      geom_text(data = sells,  aes(y = High + offset * 2.3, label = "Sell"), colour = "#C0392B", size = 3.3, fontface = "bold")
    if (label_all) {   # label every bar with its signal, including Hold
      holds <- df[df$Signal == "Hold", ]
      holds$LabelY <- if (chart_type == "Candlestick") holds$Low - offset else holds$Close - offset
      p <- p + geom_text(data = holds, aes(y = LabelY, label = "H"), colour = "grey55", size = 2.3)
    }
  }

  line_colours <- c("Close" = "black", "Up" = "#2E8B57", "Down" = "#C0392B")
  line_colours[paste0(ma_type, " ", short_n)] <- "#1F77B4"
  line_colours[paste0(ma_type, " ", long_n)]  <- "#FF7F0E"
  p <- p + scale_colour_manual(values = line_colours, breaks = setdiff(names(line_colours), c("Up", "Down")), name = NULL) +
    labs(title = paste(symbol, "-", chart_type, "chart"), x = NULL, y = "Price") +
    theme_minimal(base_size = 13) + theme(legend.position = "top")

  panels <- list(p); heights <- c(3)
  x_scale <- scale_x_date(limits = range(df$Date) + c(-bar_width * 2, bar_width * 2))

  # ---- Step 3: RSI layer (on/off) ----
  if ("RSI" %in% indicators) {
    r <- ggplot(df, aes(x = Date, y = RSI)) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = rsi_high, ymax = 100, fill = "#C0392B", alpha = 0.08) +
      annotate("rect", xmin = min(df$Date), xmax = max(df$Date), ymin = 0, ymax = rsi_low, fill = "#2E8B57", alpha = 0.08) +
      geom_hline(yintercept = c(rsi_low, rsi_high), linetype = "dashed", colour = "grey40") +
      geom_line(colour = "purple", linewidth = 0.7, na.rm = TRUE) +
      scale_y_continuous(limits = c(0, 100), breaks = c(0, rsi_low, 50, rsi_high, 100)) +
      labs(x = NULL, y = "RSI") + theme_minimal(base_size = 12)
    panels <- c(panels, list(r)); heights <- c(heights, 1)
  }

  # ---- Step 3: MACD layer (on/off) ----
  if ("MACD" %in% indicators) {
    m <- ggplot(df, aes(x = Date)) +
      geom_col(aes(y = MACD_Hist, fill = MACD_Hist >= 0), width = bar_width * 2, show.legend = FALSE) +
      scale_fill_manual(values = c(`TRUE` = "#2E8B57", `FALSE` = "#C0392B")) +
      geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.7) +
      geom_line(aes(y = MACD_Signal, colour = "Signal"), linewidth = 0.7) +
      geom_hline(yintercept = 0, colour = "grey50") +
      scale_colour_manual(values = c(MACD = "#1F77B4", Signal = "#FF7F0E"), name = NULL) +
      labs(x = NULL, y = "MACD") + theme_minimal(base_size = 12) + theme(legend.position = "right")
    panels <- c(panels, list(m)); heights <- c(heights, 1.2)
  }

  # ---- Volume layer (on/off) ----
  if ("Volume" %in% indicators) {
    v <- ggplot(df, aes(x = Date, y = Volume / 1e6, fill = Direction)) +
      geom_col(width = bar_width * 2, show.legend = FALSE) +
      scale_fill_manual(values = c(Up = "#2E8B57", Down = "#C0392B")) +
      labs(x = NULL, y = "Vol (M)") + theme_minimal(base_size = 12)
    panels <- c(panels, list(v)); heights <- c(heights, 0.8)
  }

  panels <- lapply(panels, function(pl) pl + x_scale)
  wrap_plots(panels, ncol = 1, heights = heights)
}


# ======================================================================
# SHINY UI (Step 2: app skeleton and interactive widgets)
# ======================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f6f8fa; }
    .well { background-color: #ffffff; border-radius: 8px; }
    .status { color: #555; font-size: 13px; margin-bottom: 6px; }
    h4 { color: #1F3864; margin-top: 18px; }
  "))),
  titlePanel("Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Data"),
      radioButtons("data_source", "Data source:", c("Yahoo Finance", "Upload CSV"), inline = TRUE),
      conditionalPanel("input.data_source == 'Yahoo Finance'",
        selectizeInput("symbol", "Stock symbol (pick or type):", choices = portfolio,
                       selected = portfolio[1], options = list(create = TRUE))),
      conditionalPanel("input.data_source == 'Upload CSV'",
        fileInput("csv_file", "CSV file (Date, Open, High, Low, Close, Volume):", accept = ".csv")),
      dateRangeInput("date_range", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly")),
      radioButtons("chart_type", "Chart type:", c("Candlestick", "Line", "Area"), inline = TRUE),

      h4("Indicators"),
      checkboxGroupInput("technical_indicators", "Show on chart:",
                         choices = c("Moving Averages", "RSI", "MACD", "Volume"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      selectInput("ma_type", "Moving average type:", c("SMA", "EMA")),
      sliderInput("short_ma", "Short MA period:", min = 5, max = 50, value = 20),
      sliderInput("long_ma", "Long MA period:", min = 20, max = 200, value = 50),
      numericInput("rsi_period", "RSI period:", value = 14, min = 2, max = 50),
      sliderInput("rsi_levels", "RSI oversold / overbought:", min = 10, max = 90, value = c(30, 70)),
      fluidRow(
        column(4, numericInput("macd_fast", "MACD fast", 12, min = 2)),
        column(4, numericInput("macd_slow", "slow", 26, min = 3)),
        column(4, numericInput("macd_signal", "signal", 9, min = 2))),

      h4("Trading rules"),
      selectInput("trading_rule", "Rule:",
                  c("MA Crossover", "MACD Crossover", "RSI Reversal", "MA + RSI Filter")),
      checkboxInput("show_signals", "Show Buy/Sell annotations", TRUE),
      checkboxInput("label_all", "Label every bar (H = Hold)", FALSE),
      checkboxInput("shade_trend", "Shade trend (short MA above/below long MA)", FALSE)
    ),

    mainPanel(width = 9,
      tabsetPanel(
        tabPanel("Chart",
                 div(class = "status", textOutput("status")),
                 plotOutput("stock_chart", height = "780px")),
        tabPanel("Signals",
                 h4("Strategy summary"), tableOutput("backtest_table"),
                 h4("Buy / Sell signals"), tableOutput("signal_table")),
        tabPanel("Portfolio",
                 p("Compare all stocks in portfolio.txt over the selected date range."),
                 actionButton("load_portfolio", "Load portfolio", class = "btn-primary"),
                 br(), br(),
                 plotOutput("portfolio_chart", height = "420px"),
                 tableOutput("portfolio_table")),
        tabPanel("Data", h4("Latest bars with indicators"), tableOutput("data_table"))
      )
    )
  )
)


# ======================================================================
# SHINY SERVER (Steps 2-4: reactive data, chart, indicators, signals)
# ======================================================================
server <- function(input, output, session) {

  # ---- Step 1: fetch data (extra history before the start date so the
  #      moving averages are ready at the start of the chart) ----
  raw_data <- reactive({
    validate(need(input$date_range[1] < input$date_range[2], "Start date must be before end date."))
    lookback <- c(Daily = 400, Weekly = 1100, Monthly = 4000)[[input$time_frame]]
    result <- tryCatch({
      if (input$data_source == "Upload CSV") {
        validate(need(input$csv_file, "Please upload a CSV file."))
        read_csv_data(input$csv_file$datapath)
      } else {
        validate(need(input$symbol, "Please choose a stock symbol."))
        fetch_stock_data(input$symbol, input$date_range[1] - lookback, input$date_range[2])
      }
    }, error = function(e) {
      if (inherits(e, "shiny.silent.error")) stop(e)
      validate(need(FALSE, conditionMessage(e)))
    })
    if (grepl("cache", result$source)) {
      showNotification("Yahoo Finance not reachable - showing saved data.", type = "warning")
    }
    result
  })

  symbol_name <- reactive({
    if (input$data_source == "Upload CSV") "Uploaded data" else toupper(input$symbol)
  })

  # ---- Steps 2-4: time frame, indicators, signals, then date filter ----
  chart_data <- reactive({
    validate(need(input$short_ma < input$long_ma, "Short MA period must be smaller than the long MA period."),
             need(input$macd_fast < input$macd_slow, "MACD fast period must be smaller than the slow period."))
    bars <- apply_time_frame(raw_data()$data, input$time_frame)
    df <- to_df(bars)
    df <- add_indicators(df, input$ma_type, input$short_ma, input$long_ma, input$rsi_period,
                         input$macd_fast, input$macd_slow, input$macd_signal)
    df <- generate_signals(df, input$trading_rule, input$rsi_levels[1], input$rsi_levels[2])
    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) >= 2, "Not enough bars in this date range. Choose a longer range or a shorter time frame."))
    df
  })

  output$status <- renderText({
    df <- chart_data(); info <- raw_data()
    paste0("Source: ", info$source, " | ", nrow(df), " ", tolower(input$time_frame), " bars from ",
           format(min(df$Date)), " to ", format(max(df$Date)),
           if (isTRUE(info$removed > 0)) paste0(" | ", info$removed, " rows with missing prices removed") else "",
           if (sum(!is.na(df$MA_Long)) == 0 && "Moving Averages" %in% input$technical_indicators)
             " | Not enough bars for the long MA" else "")
  })

  # ---- Main chart ----
  output$stock_chart <- renderPlot({
    build_chart(chart_data(), symbol_name(), input$chart_type, input$technical_indicators,
                input$ma_type, input$short_ma, input$long_ma,
                input$rsi_levels[1], input$rsi_levels[2],
                input$show_signals, input$label_all, input$shade_trend)
  })

  # ---- Signals tab ----
  output$backtest_table <- renderTable(backtest(chart_data()))
  output$signal_table <- renderTable({
    df <- chart_data()
    s <- df[df$Signal != "Hold", c("Date", "Close", "MA_Short", "MA_Long", "RSI", "MACD", "Signal")]
    validate(need(nrow(s) > 0, "No Buy or Sell signals in this period with the current rule."))
    s$Date <- format(s$Date)
    s
  }, digits = 2)

  # ---- Data tab ----
  output$data_table <- renderTable({
    df <- tail(chart_data(), 15)
    df$Date <- format(df$Date)
    df[, c("Date", "Open", "High", "Low", "Close", "Volume", "MA_Short", "MA_Long", "RSI", "MACD", "Signal")]
  }, digits = 2)

  # ---- Portfolio tab: all stocks in portfolio.txt ----
  portfolio_data <- eventReactive(input$load_portfolio, {
    withProgress(message = "Loading portfolio", value = 0, {
      out <- list()
      for (s in portfolio) {
        incProgress(1 / length(portfolio), detail = s)
        res <- tryCatch(fetch_stock_data(s, input$date_range[1] - 400, input$date_range[2]),
                        error = function(e) NULL)
        if (is.null(res)) { showNotification(paste("Could not load", s), type = "error"); next }
        df <- to_df(res$data)
        df <- add_indicators(df, input$ma_type, input$short_ma, input$long_ma, input$rsi_period,
                             input$macd_fast, input$macd_slow, input$macd_signal)
        df <- generate_signals(df, input$trading_rule, input$rsi_levels[1], input$rsi_levels[2])
        df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
        if (nrow(df) > 1) out[[s]] <- df
      }
      validate(need(length(out) > 0, "No portfolio data could be loaded."))
      out
    })
  })

  output$portfolio_chart <- renderPlot({
    pdata <- portfolio_data()
    all <- do.call(rbind, lapply(names(pdata), function(s) {
      d <- pdata[[s]]; data.frame(Date = d$Date, Symbol = s, Growth = d$Close / d$Close[1] * 100)
    }))
    ggplot(all, aes(Date, Growth, colour = Symbol)) +
      geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
      geom_line(linewidth = 0.8) +
      labs(title = "Growth of $100 invested at the start date", x = NULL, y = "Value ($)") +
      theme_minimal(base_size = 13)
  })

  output$portfolio_table <- renderTable({
    pdata <- portfolio_data()
    do.call(rbind, lapply(names(pdata), function(s) {
      d <- pdata[[s]]; last <- d[nrow(d), ]
      last_signal <- d[d$Signal != "Hold", ]
      data.frame(Symbol = s, Last_Close = last$Close,
                 Period_Return = sprintf("%.1f%%", (last$Close / d$Close[1] - 1) * 100),
                 RSI = last$RSI,
                 Trend = ifelse(isTRUE(last$MA_Short > last$MA_Long), "Up", "Down"),
                 Last_Signal = if (nrow(last_signal) > 0)
                   paste(last_signal$Signal[nrow(last_signal)], "on", format(last_signal$Date[nrow(last_signal)]))
                   else "None")
    }))
  }, digits = 2)
}


# ======================================================================
# RUN THE APP
# ======================================================================
shinyApp(ui, server)
