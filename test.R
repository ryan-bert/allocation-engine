suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(lubridate)
  library(scales)
})

# Create the wide-form data
wide_data <- data.frame(
  Date = as.Date(c("2025-01-03", "2025-01-04", "2025-01-05", "2025-01-06", "2025-01-07",
                   "2025-01-08", "2025-01-09", "2025-01-10", "2025-01-11", "2025-01-12",
                   "2025-01-13", "2025-01-14", "2025-01-15", "2025-01-16", "2025-01-17",
                   "2025-01-18", "2025-01-19", "2025-01-20", "2025-01-21", "2025-01-22")),
  QQQ_Weight = rep(0.6, 20),
  GLD_Weight = rep(0.4, 20),
  QQQ_Return = c(0.1, -0.15, 0.14, -0.05, 0.05, 0.02, 0.02, -0.12, 0.05, 0.02, 0.02, -0.1, 0.1, -0.15, 0.14, -0.05, 0.14, -0.05, 0.1, 0.1),
  GLD_Return = c(0.05, 0.02, 0.02, -0.1, 0.1, -0.15, 0.14, -0.05, 0.1, -0.15, 0.14, -0.05, 0.05, 0.02, 0.02, -0.1, -0.15, 0.14, -0.05, 0.05)
)

# Convert to long format properly
long_data <- wide_data %>%
  pivot_longer(cols = ends_with("_Weight"), names_to = "Ticker", values_to = "Weight") %>%
  mutate(Ticker = sub("_Weight", "", Ticker)) %>%
  pivot_longer(cols = ends_with("_Return"), names_to = "Ticker_Return", values_to = "Return") %>%
  mutate(Ticker_Return = sub("_Return", "", Ticker_Return)) %>%
  filter(Ticker == Ticker_Return) %>%
  select(-Ticker_Return)

# Print result
print(long_data)

apply_rebalancing <- function(portfolio_df, rebalance_freq = 5) {
  
  # Get unique sorted dates
  unique_dates <- portfolio_df %>%
    distinct(Date) %>%
    arrange(Date) %>%
    pull(Date)
  
  # Select every nth date as rebalance date
  rebalance_dates <- unique_dates[seq(1, length(unique_dates), by = rebalance_freq)]
  
  # Mark rebalance days
  portfolio_df <- portfolio_df %>%
    mutate(Is_Rebalance = Date %in% rebalance_dates)
  
  # Sort by Date and Ticker
  portfolio_df <- portfolio_df %>%
    arrange(Date, Ticker)

  # Store rebalance weights
  portfolio_df <- portfolio_df %>%
    group_by(Ticker) %>%
    mutate(Rebalance_Weight = ifelse(Is_Rebalance, Weight, NA_real_)) %>%
    fill(Rebalance_Weight, .direction = "down") %>%
    ungroup()

  # Store the date on rebalance days
  portfolio_df <- portfolio_df %>%
    group_by(Ticker) %>%
    mutate(Rebalance_Date = ifelse(Is_Rebalance, Date, NA_real_)) %>%
    fill(Rebalance_Date, .direction = "down") %>%
    ungroup()

  # Calculate cum returns since last rebalance for each ticker
  portfolio_df <- portfolio_df %>%
    group_by(Ticker, Rebalance_Date) %>%
    mutate(
      Return_Since_Rebalance = cumprod(1 + Return) - 1
    )

  # Compute actual weights based on return since rebalance
  portfolio_df <- portfolio_df %>%
    mutate(Weight = if_else(!Is_Rebalance, Rebalance_Weight * (1 + lag(Return_Since_Rebalance)), Rebalance_Weight))

  # Normalize weights to ensure they sum to 1 each day
  portfolio_df <- portfolio_df %>%
    group_by(Date) %>%
    mutate(Weight = Weight / sum(Weight, na.rm = TRUE)) %>%
    ungroup() %>%
    select(Date, Ticker, Return, Weight, Is_Rebalance)

  return(portfolio_df)
}

long_data <- apply_rebalancing(long_data, rebalance_freq = 2)