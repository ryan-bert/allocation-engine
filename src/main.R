rm(list = ls())

suppressMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
})

###################### ASSET SELECTION & PRE-PROCESSING ######################

# Load postgress credentials
current_dir <- dirname(sys.frame(1)$ofile)
credentials_path <- file.path("~/Documents/Credentials/Raspberry Pi/financial-database.json")
credentials <- fromJSON(credentials_path)

# Load strategy and engine functions
source(file.path(current_dir, "strategy.R"))
source(file.path(current_dir, "engine.R"))

# Connect to the database
conn <- dbConnect(
  Postgres(),
  dbname = credentials$dbname,
  host = credentials$host,
  port = as.integer(credentials$port),
  user = credentials$user,
  password = credentials$password
)

# Load the data from the database
stocks_df <- dbGetQuery(conn, "SELECT * FROM equities")
stocks_df$Date <- as.Date(stocks_df$Date)
etf_df <- dbGetQuery(conn, "SELECT * FROM etfs")
etf_df$Date <- as.Date(etf_df$Date)

# ETF selection
etf_df <- etf_df %>%
  filter(Ticker %in% c("SPY", "QQQ", "GLD", "SOXX", "EFA"))

# Stock selection
stocks_df <- stocks_df %>%
  filter(Ticker %in% c("AAPL", "MSFT", "AMZN", "GOOGL", "NVDA"))

# Combine the data
portfolio_df <- bind_rows(stocks_df, etf_df)

# Ensure all assets have the same date range
portfolio_df <- align_dates(portfolio_df)

###################### STRATEGY ######################

portfolio_df <- apply_strategy(portfolio_df)

###################### BACKTEST ######################

# Run backtest to compute portfolio returns and value
backtest_df <- run_backtest(portfolio_df, "2018-01-01")

# Compute rolling portfolio drawdown
backtest_df <- compute_drawdown(backtest_df)

# Get performance metrics
bonds_df <- dbGetQuery(conn, "SELECT * FROM bonds")
performance_df <- analyse_performance(backtest_df, bonds_df)

# Include benchmark to backtest
benchmark_df <- dbGetQuery(conn, "SELECT * FROM etfs")
backtest_df <- include_benchmark(backtest_df, benchmark_df, "SPY")

# Compute rolling benchmark drawdown
backtest_df <- compute_drawdown(backtest_df, is_benchmark = TRUE)

# Get performance metrics for benchmark and combine
performance_df <- analyse_performance(
  backtest_df,
  bonds_df,
  is_benchmark = TRUE
) %>%
  left_join(performance_df, by = "Metric") %>%
  select(Metric, Portfolio, Benchmark)

# Print performance metrics
cat("\nPerformance Metrics:\n\n")
print(performance_df)

# Generate plots
generate_plots(backtest_df)
