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

# Load data from the database
etf_df <- dbGetQuery(conn, "SELECT * FROM etfs") %>% mutate(Date = as.Date(Date))
stocks_df <- dbGetQuery(conn, "SELECT * FROM equities") %>% mutate(Date = as.Date(Date))
crypto_df <- dbGetQuery(conn, "SELECT * FROM crypto") %>% mutate(Date = as.Date(Date))
index_df <- dbGetQuery(conn, "SELECT * FROM indices") %>% mutate(Date = as.Date(Date))
futures_df <- dbGetQuery(conn, "SELECT * FROM futures") %>% mutate(Date = as.Date(Date))
macros_df <- dbGetQuery(conn, "SELECT * FROM macros") %>% mutate(Date = as.Date(Date))

# Combine into single dataframe
assets_df <- etf_df %>%
  bind_rows(stocks_df) %>%
  bind_rows(crypto_df) %>%
  bind_rows(index_df) %>%
  bind_rows(futures_df) %>%
  bind_rows(macros_df)

# Choose assets to include in the strategy
portfolio_df <- assets_df %>%
  filter(Ticker %in% c("SPY", "QQQ", "GLD", "SOXX", "EFA", "BRK-B"))

###################### STRATEGY ######################

# Ensure all assets have the same date range
portfolio_df <- align_dates(portfolio_df)

# Apply and validate the strategy
portfolio_df <- apply_strategy(portfolio_df)
validate_strategy(portfolio_df)

###################### NUANCES ######################

# Take rebalancing into account
portfolio_df <- apply_rebalancing(portfolio_df, rebalance_freq = 21)

# Apply transaction fees
portfolio_df <- apply_fees(portfolio_df, tx_fee = 0.007)

# Apply interest
portfolio_df <- apply_interest(portfolio_df, assets_df)

###################### BACKTEST ######################

# Run backtest to compute portfolio returns and value
backtest_df <- run_backtest(portfolio_df, "1990-01-01", "2021-01-12")

# Compute rolling portfolio drawdown
backtest_df <- compute_drawdown(backtest_df)

# Get performance metrics
performance_df <- analyse_performance(backtest_df, assets_df)

# Include benchmark to backtest
backtest_df <- include_benchmark(backtest_df, assets_df, "SPY")

# Compute rolling benchmark drawdown
backtest_df <- compute_drawdown(backtest_df, is_benchmark = TRUE)

# Get performance metrics for benchmark and combine
performance_df <- analyse_performance(
  backtest_df,
  assets_df,
  is_benchmark = TRUE
) %>%
  left_join(performance_df, by = "Metric") %>%
  select(Metric, Portfolio, Benchmark)

# Print performance metrics
cat("\nPerformance Metrics:\n\n")
print(performance_df)

# Generate plots
generate_plots(backtest_df)
plot_weights(portfolio_df, backtest_df)

# Save the strategy
save_strategy(conn, backtest_df, name = "QQQ_GLD_STATIC")
