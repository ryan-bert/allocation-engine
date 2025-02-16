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

###################### BACKTEST ######################

# Calculate portfolio returns
backtest_df <- portfolio_df %>%
  group_by(Date) %>%
  summarise(
    Portfolio_Return = sum(Return * Weight)
  ) %>%
  ungroup()

# Define start date for backtest
start_date <- "2018-01-01"
backtest_df <- backtest_df %>%
  filter(Date >= start_date)
cat("\nBacktest Start Date:", start_date, "\n")

# Calculate cumulative returns
backtest_df <- backtest_df %>%
  mutate(
    Cumulative_Return = cumprod(1 + Portfolio_Return) - 1,
    Indexed_Return = 100 * (1 + Cumulative_Return)
  ) %>%
  select(-Cumulative_Return)

###################### PERFORMANCE ######################

# Calculate rolling drawdown
backtest_df <- backtest_df %>%
  mutate(
    Roll_Max = cummax(Indexed_Return),
    Drawdown = (Indexed_Return - Roll_Max) / Roll_Max
  ) %>%
  select(-Roll_Max)

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
  Portfolio = c(annual_return, annual_vol, sharpe_ratio, max_drawdown, sortino_ratio, calmar_ratio)
)

####################### BENCHMARK #######################

# Load benchmark data
benchmark_df <- etf_df %>%
  filter(Ticker == "SPY") %>%
  filter(Date >= start_date) %>%
  select(Date, Benchmark_Return = Return) %>%
  arrange(Date)

# Calculate cumulative returns
benchmark_df <- benchmark_df %>%
  mutate(
    Cum_Benchmark_Return = cumprod(1 + Benchmark_Return) - 1,
    Benchmark_Index = 100 * (1 + Cum_Benchmark_Return)
  ) %>%
  select(-Cum_Benchmark_Return)

# Calculate rolling drawdown
benchmark_df <- benchmark_df %>%
  mutate(
    Roll_Max = cummax(Benchmark_Index),
    Benchmark_Drawdown = (Benchmark_Index - Roll_Max) / Roll_Max
  ) %>%
  select(-Roll_Max)

# Merge with backtest data
backtest_df <- backtest_df %>%
  left_join(benchmark_df, by = "Date")

# Compute CAGR for benchmark
start_value <- first(backtest_df$Benchmark_Index)
end_value <- last(backtest_df$Benchmark_Index)
benchmark_cagr <- ((end_value / start_value) ^ (252 / nrow(backtest_df))) - 1

# Calculate annualized volatility for benchmark
benchmark_vol <- sd(backtest_df$Benchmark_Return) * sqrt(252)

# Compute Sharpe Ratio for benchmark
benchmark_sharpe <- (benchmark_cagr - avg_annual_rfr) / benchmark_vol

# Calculate max drawdown for benchmark
benchmark_drawdown <- min(backtest_df$Benchmark_Drawdown)

# Calculate Sortino Ratio for benchmark
benchmark_neg_returns <- backtest_df$Benchmark_Return[backtest_df$Benchmark_Return < 0]
benchmark_sortino <- (benchmark_cagr - avg_annual_rfr) / (sd(benchmark_neg_returns) * sqrt(252))

# Calculate Calmar Ratio for benchmark
benchmark_calmar <- benchmark_cagr / abs(benchmark_drawdown)

# Combine benchmark performance metrics
benchmark_performance_df <- data.frame(
  Metric = c("CAGR", "Annualized Volatility", "Sharpe Ratio", "Max Drawdown", "Sortino Ratio", "Calmar Ratio"),
  Benchmark = c(benchmark_cagr, benchmark_vol, benchmark_sharpe, benchmark_drawdown, benchmark_sortino, benchmark_calmar)
)

# Merge benchmark performance with portfolio performance
performance_df <- performance_df %>%
  left_join(benchmark_performance_df, by = "Metric")


# Print performance metrics
cat("\nPerformance Metrics:\n\n")
print(performance_df)

######################### PLOTS #########################

# Compute the mean of returns and the density
mean_return <- mean(backtest_df$Portfolio_Return, na.rm = TRUE)
density_data <- density(backtest_df$Portfolio_Return, na.rm = TRUE)
max_density <- max(density_data$y)

# Plot the distribution of returns
ggplot(backtest_df, aes(x = Portfolio_Return)) +
  geom_density(fill = "#7b7b7b", alpha = 0.5) +
  geom_vline(
    xintercept = mean_return,
    linetype = "dashed",
    color = "red"
  ) +
  annotate(
    "text",
    x = mean_return + 0.015,
    y = max_density,
    label = paste("Mean =", round(mean_return * 100, 2), "%"),
    vjust = -0.5
  ) +
  labs(
    title = "Distribution of Portfolio Returns",
    x = "Daily Return",
    y = "Density"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))
suppressMessages({
  ggsave(file.path(current_dir, "../plots/returns_distribution.png"))
})

# Plot the indexed return over time with benchmark comparison
ggplot(backtest_df, aes(x = Date)) +
  geom_line(aes(y = Benchmark_Index, color = "Benchmark")) +
  geom_line(aes(y = Indexed_Return, color = "Portfolio")) +
  scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  labs(
    title = "Indexed Return Over Time",
    x = "Date",
    y = "Indexed Return",
    color = "Legend"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))
suppressMessages({
  ggsave(file.path(current_dir, "../plots/indexed_return.png"))
})

# Plot the rolling drawdown with benchmark comparison
ggplot(backtest_df, aes(x = Date)) +
  geom_line(aes(y = Benchmark_Drawdown, color = "Benchmark")) +
  geom_line(aes(y = Drawdown, color = "Portfolio")) +
  scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  labs(
    title = "Rolling Drawdown",
    x = "Date",
    y = "Drawdown",
    color = "Legend"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))
suppressMessages({
  ggsave(file.path(current_dir, "../plots/rolling_drawdown.png"))
})

# Create the Indexed Return plot (upper panel)
p1 <- ggplot(backtest_df, aes(x = Date)) +
  geom_line(aes(y = Benchmark_Index, color = "Benchmark")) +
  geom_line(aes(y = Indexed_Return, color = "Portfolio")) +
  scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  labs(
    title = "Indexed Return Over Time",
    x = "",
    y = "Indexed Return",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )
# Create the Rolling Drawdown plot (lower panel)
p2 <- ggplot(backtest_df, aes(x = Date)) +
  geom_line(aes(y = Benchmark_Drawdown, color = "Benchmark")) +
  geom_line(aes(y = Drawdown, color = "Portfolio")) +
  scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  labs(
    title = "Rolling Drawdown",
    x = "Date",
    y = "Drawdown",
    color = "Legend"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14)
  )
# Combine the two plots vertically with patchwork
combined_plot <- p1 / p2 + plot_layout(heights = c(3, 1))
suppressMessages({
  ggsave(file.path(current_dir, "../plots/combined_plot.png"))
})

# Scatter plot of Portfolio vs Benchmark returns
ggplot(backtest_df, aes(x = Benchmark_Return, y = Portfolio_Return)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Portfolio vs Benchmark Returns",
    x = "Benchmark Return",
    y = "Portfolio Return"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))
suppressMessages({
  ggsave(file.path(current_dir, "../plots/scatter_plot.png"))
})