suppressMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

###################### ASSET SELECTION & PRE-PROCESSING ######################

# Load postgress credentials
current_dir <- dirname(sys.frame(1)$ofile)
credentials_path <- file.path("~/Documents/Credentials/Raspberry Pi/financial-database.json")
credentials <- fromJSON(credentials_path)

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
etf_df <- dbGetQuery(conn, "SELECT * FROM etfs")

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
  filter(Date >= start_date)

###################### STRATEGY ######################

# Define weights
weights <- c(
  "SPY" = 0.25,
  "QQQ" = 0,
  "GLD" = 0.5,
  "SOXX" = 0,
  "EFA" = 0.25,
  "AAPL" = 0,
  "MSFT" = 0,
  "AMZN" = 0,
  "GOOGL" = 0,
  "NVDA" = 0
)

# Convert to data frame
weights_df <- data.frame(
  Ticker = names(weights),
  Weight = as.numeric(weights)
)

# Left join portfolio and weights
portfolio_df <- portfolio_df %>%
  left_join(weights_df, by = "Ticker") %>%
  filter(Weight > 0)

###################### BACKTEST ######################

# Calculate portfolio returns
backtest_df <- portfolio_df %>%
  group_by(Date) %>%
  summarise(
    Portfolio_Return = sum(Return * Weight)
  ) %>%
  ungroup()

# Define start date for backtest
start_date <- min(backtest_df$Date)
backtest_df <- backtest_df %>%
  filter(Date >= start_date)

# Calculate cumulative returns
backtest_df <- backtest_df %>%
  mutate(
    Cumulative_Return = cumprod(1 + Portfolio_Return) - 1
  )

# Calculate indexed return
backtest_df <- backtest_df %>%
  mutate(
    Indexed_Return = 100 * (1 + Cumulative_Return)
  )

###################### PERFORMANCE ######################

# Calculate rolling drawdown
backtest_df <- backtest_df %>%
  mutate(
    Roll_Max = cummax(Indexed_Return),
    Drawdown = (Indexed_Return - Roll_Max) / Roll_Max
  )

# Pull bond data and compute RFR
bonds_df <- dbGetQuery(conn, "SELECT * FROM bonds") %>%
  filter(Ticker == "DGS3MO") %>%
  mutate(Annual_RFR = Yield / 100) %>%
  select(Date, Annual_RFR)

# Merge with backtest data
backtest_df <- backtest_df %>%
  left_join(bonds_df, by = "Date") %>%
  arrange(Date) %>%
  fill(Annual_RFR, .direction = "down") %>%
  fill(Annual_RFR, .direction = "up")

# Calculate CAGR
start_value <- first(backtest_df$Indexed_Return)
end_value <- last(backtest_df$Indexed_Return)
annual_return <- ((end_value / start_value) ^ (252 / nrow(backtest_df))) - 1

# Calculate annualized volatility
annual_vol <- sd(backtest_df$Portfolio_Return) * sqrt(252)

# Compute avg RFR and convert to daily rate
avg_annual_rfr <- mean(backtest_df$Annual_RFR)

# Calculate Sharpe Ratio
sharpe_ratio <- (annual_return - avg_annual_rfr) / annual_vol

# Calculate max drawdown
max_drawdown <- min(backtest_df$Drawdown)

# Calculate Sortino Ratio
neg_returns <- backtest_df$Portfolio_Return[backtest_df$Portfolio_Return < 0]
sortino_ratio <- (annual_return - avg_annual_rfr) / (sd(neg_returns) * sqrt(252))

# Calculate Calmar Ratio
calmar_ratio <- annual_return / abs(max_drawdown)

# Combine performance metrics
performance_df <- data.frame(
  Metric = c("CAGR", "Annualized Volatility", "Sharpe Ratio", "Max Drawdown", "Sortino Ratio", "Calmar Ratio"),
  Value = c(annual_return, annual_vol, sharpe_ratio, max_drawdown, sortino_ratio, calmar_ratio)
)
