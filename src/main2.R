rm(list = ls())

suppressMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})

###################### ASSET SELECTION & PRE-PROCESSING ######################

# Load postgress credentials
current_dir <- dirname(sys.frame(1)$ofile)
credentials_path <- file.path("~/Documents/Credentials/Raspberry Pi/financial-database.json")
credentials <- fromJSON(credentials_path)

# Load allocation engine functions
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

# Get maximum min date
start_date <- portfolio_df %>%
  group_by(Ticker) %>%
  summarise(min_date = min(Date)) %>%
  ungroup() %>%
  summarise(max_min_date = max(min_date)) %>%
  pull()

# Filter data to start from maximum min date
portfolio_df <- portfolio_df %>%
  filter(Date >= start_date) %>%
  select(-Price)

###################### STRATEGY ######################

# Define allocation
allocation <- c(
  "SPY" = 0,
  "QQQ" = 0.6,
  "GLD" = 0.4,
  "SOXX" = 0,
  "EFA" = 0,
  "AAPL" = 0,
  "MSFT" = 0,
  "AMZN" = 0,
  "GOOGL" = 0,
  "NVDA" = 0
)

# Convert to data frame
allocation_df <- data.frame(
  Ticker = names(allocation),
  Weight = as.numeric(allocation)
)

# Print allocation
cat("Allocation:\n\n")
allocation_df %>%
  filter(Weight > 0) %>%
  print()

# Left join portfolio and allocation
portfolio_df <- portfolio_df %>%
  left_join(allocation_df, by = "Ticker") %>%
  filter(Weight > 0)

####################################################

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
  left_join(performance_df, by = "Metric")

# Print performance metrics
cat("\nPerformance Metrics:\n\n")
print(performance_df)
