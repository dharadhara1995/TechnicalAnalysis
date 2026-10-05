# Assignment 6: Technical Analysis using R, Visualization Phase

**Student:** Dhara Patel
**Course:** BDA400 - Data Science Tools and Techniques, CDI College
**Repository:** [https://github.com/[username]/TechnicalAnalysis/tree/main/Assignment6](https://github.com/[username]/TechnicalAnalysis/tree/main/Assignment6)

## Project Description

An interactive portfolio dashboard built with R Shiny. It fetches stock data from Yahoo Finance, shows it as a candlestick, line or area chart, and lets the user turn technical indicators (moving averages, RSI, MACD, volume) on and off. It applies a choice of trading rules and annotates the chart bars with Buy and Sell signals. The indicators use my own functions from Assignment 5. This is the final stage of the three-part Technical Analysis project (Assignments 2, 5 and 6).

## How to Run

1. Install the packages once:

```r
install.packages(c("shiny", "ggplot2", "quantmod", "patchwork"))
```

2. Open `app.R` in RStudio and click **Run App**, or run `shiny::runApp("path/to/Assignment6")`.
3. An internet connection is needed for Yahoo Finance. If Yahoo is not reachable, the app uses the last saved copy in `data_cache/`, or you can upload a CSV file.

## Files

| File | Description |
|------|-------------|
| `app.R` | All the code for the dashboard, in sections for Steps 1 to 4 |
| `portfolio.txt` | My stock symbols (from Assignment 2) |
| `DharaPatel_BDA400_A06.docx` | Cover page and documentation with screenshots |

## Features

- **Data:** Yahoo Finance with error handling, local cache backup, CSV upload, and removal of rows with missing prices
- **Widgets:** stock symbol, date range, time frame (daily, weekly, monthly), chart type
- **Indicators:** SMA or EMA (adjustable periods), RSI (adjustable period and levels), MACD (adjustable periods), volume, each with an on/off switch
- **Trading rules:** MA crossover, MACD crossover, RSI reversal, MA + RSI filter
- **Annotations:** Buy/Sell markers on the bars, optional Hold labels, optional trend shading
- **Extra tabs:** signal list with a simple back-test, portfolio comparison, and a data table
