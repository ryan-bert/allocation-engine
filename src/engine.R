suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(lubridate)
  library(scales)
  library(stringr)
})

#' Align Dates in Portfolio Data
#'
#' This function aligns the start date of all tickers in a portfolio by setting
#' the earliest common start date.
#'
#' @param portfolio_df A data frame containing portfolio data with columns: Ticker, Date, and Price.
#'
#' @return A filtered data frame with the aligned start date and no Price column.
align_dates <- function(portfolio_df) {

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

  return(portfolio_df)
}

#' Apply Portfolio Rebalancing
#'
#' Adjusts portfolio weights based on a specified rebalancing frequency, allowing weights 
#' to drift with returns between rebalancing dates.
#'
#' @param portfolio_df A data frame containing portfolio data with
#' columns: Date, Return, and Weight.
#' @param rebalance_freq Integer specifying rebalancing frequency in days (default: `5`).
#'
#' @return A data frame with updated weights, containing:
#'   - `Date` (Date): Observation date.
#'   - `Ticker` (character): Asset identifier.
#'   - `Return` (numeric): Daily asset return.
#'   - `Weight` (numeric): Adjusted portfolio weight.
#'   - `Is_Rebalance` (logical): `TRUE` if rebalancing occurred.
apply_rebalancing <- function(portfolio_df, rebalance_freq = 5) {

  # Print rebalance frequency
  cat("\nRebalance frequency: ", rebalance_freq, "days\n")
  
  # Get unique sorted dates
  unique_dates <- portfolio_df %>%
    distinct(Date) %>%
    arrange(Date) %>%
    pull(Date)

  # Tickers must have a weight on every date (even if zero)
  portfolio_df <- portfolio_df %>%
    group_by(Ticker) %>%
    complete(Date = unique_dates) %>%
    ungroup() %>%
    mutate(
      Weight = if_else(is.na(Weight), 0, Weight),
      Return = if_else(is.na(Return), 0, Return)
    )

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
    mutate(Weight = if_else(
      !Is_Rebalance,
      Rebalance_Weight * lag(1 + Return_Since_Rebalance),
      Rebalance_Weight
  ))

  # Normalise weights to ensure they sum to 1 each day
  portfolio_df <- portfolio_df %>%
    group_by(Date) %>%
    mutate(Weight = if_else(
      Weight == 0,
      0,
      Weight / sum(abs(Weight)),
    )) %>%
    ungroup() %>%
    select(Date, Ticker, Return, Weight, Is_Rebalance)

  return(portfolio_df)
}

#' Apply Transaction Fees to Portfolio Returns
#'
#' Adjusts portfolio returns to account for transaction costs incurred on rebalancing days.
#'
#' @param portfolio_df A data frame containing portfolio data with columns: Date, Ticker, Return, and Weight.
#' @param tx_fee Numeric, transaction fee rate (default: 0.001 or 0.1% per rebalance trade).
#'
#' @return A data frame with transaction costs applied to returns.
apply_fees <- function(portfolio_df, tx_fee = 0.001) {

  # Print transaction fee
  cat(paste0("Transaction fee:  ", tx_fee * 100, "%\n"))

  # Calculate real weight on rebalance days
  portfolio_df <- portfolio_df %>%
    group_by(Ticker) %>%
    mutate(Real_Weight = ifelse(
      Is_Rebalance,
      lag(Weight, default = 0) * lag(1 + Return, default = 1),
      Weight
    )) %>%
    ungroup()

  # Normalise real weights
  portfolio_df <- portfolio_df %>%
    group_by(Date) %>%
    mutate(Real_Weight = if_else(
      Real_Weight == 0,
      0,
      Real_Weight / sum(abs(Real_Weight)),
    )) %>%
    ungroup() %>%
    select(Date, Ticker, Return, Real_Weight, Weight, Is_Rebalance)

  # Calculate transaction costs on rebalance days
  portfolio_df <- portfolio_df %>%
    group_by(Ticker) %>%
    mutate(Tx_Cost = ifelse(
      Is_Rebalance,
      abs(Real_Weight - Weight) * tx_fee,
      0
    )) %>%
    ungroup() %>%
    select(-Real_Weight)

  # Adjust returns for transaction costs
  portfolio_df <- portfolio_df %>%
    mutate(Return = if_else(
      Weight < 0,
      (1 + Return) / (1 - Tx_Cost) - 1,
      (1 + Return) * (1 - Tx_Cost) - 1
    ))

  return(portfolio_df)
}

#' Run Portfolio Backtest
#'
#' This function calculates portfolio returns and cumulative returns,
#' filtering data from a given start date.
#'
#' @param portfolio_df A data frame containing portfolio data with
#' columns: Date, Return, and Weight.
#' @param start_date A date object or string in "YYYY-MM-DD" format, specifying
#' the start date for the backtest.
#'
#' @return A data frame with columns: Date, Portfolio_Return, and Indexed_Return
run_backtest <- function(portfolio_df, start_date, end_date = Sys.Date()) {

  # Calculate portfolio returns
  backtest_df <- portfolio_df %>%
    group_by(Date) %>%
    summarise(
      Portfolio_Return = sum(Return * Weight)
    ) %>%
    ungroup()

  # Filter data to start date and end date
  backtest_df <- backtest_df %>%
    filter(Date >= as.Date(start_date) & Date <= as.Date(end_date))

  # Calculate cumulative returns
  backtest_df <- backtest_df %>%
    mutate(
      Cumulative_Return = cumprod(1 + Portfolio_Return) - 1,
      Indexed_Return = 100 * (1 + Cumulative_Return)
    ) %>%
    select(-Cumulative_Return)

  return(backtest_df)
}

#' Compute Portfolio or Benchmark Drawdown
#'
#' This function calculates rolling drawdown for either the portfolio or a benchmark.
#'
#' @param backtest_df A data frame containing indexed return data.
#' @param is_benchmark A logical value; if TRUE, computes benchmark drawdown. Default is FALSE (portfolio).
#'
#' @return A data frame with added drawdown columns.
compute_drawdown <- function(backtest_df, is_benchmark = FALSE) {

  # Calculate rolling drawdown for BENCHMARK
  if (is_benchmark) {
    backtest_df <- backtest_df %>%
      mutate(
        Roll_Max = cummax(Benchmark_Index),
        Benchmark_Drawdown = (Benchmark_Index - Roll_Max) / Roll_Max
      ) %>%
      select(-Roll_Max)
  } else {

    # Calculate rolling drawdown for PORTFOLIO
    backtest_df <- backtest_df %>%
      mutate(
        Roll_Max = cummax(Indexed_Return),
        Drawdown = (Indexed_Return - Roll_Max) / Roll_Max
      ) %>%
      select(-Roll_Max)
  }

  return(backtest_df)
}

#' Analyze Portfolio Performance
#'
#' Computes key performance metrics including CAGR, volatility, Sharpe ratio, drawdowns, Sortino ratio, and Calmar ratio.
#'
#' @param backtest_df A data frame with portfolio returns.
#' @param macros_df A data frame containing risk-free rate data.
#' @param is_benchmark A logical value; if TRUE, computes benchmark performance.
#'
#' @return A data frame with key performance metrics.
analyse_performance <- function(backtest_df, macros_df, is_benchmark = FALSE) {

  # Define inputs based on is_benchmark
  if (is_benchmark) {
    indexed_returns <- backtest_df$Benchmark_Index
    returns <- backtest_df$Benchmark_Return
    rolling_drawdown <- backtest_df$Benchmark_Drawdown
  } else {
    indexed_returns <- backtest_df$Indexed_Return
    returns <- backtest_df$Portfolio_Return
    rolling_drawdown <- backtest_df$Drawdown
  }

  # Pull bond data and compute RFR
  macros_df <- macros_df %>%
    filter(Ticker == "DGS3MO") %>%
    mutate(Annual_RFR = Price / 100) %>%
    select(Date, Annual_RFR)

  # Merge with backtest data
  backtest_df <- backtest_df %>%
    left_join(macros_df, by = "Date") %>%
    arrange(Date) %>%
    fill(Annual_RFR, .direction = "down") %>%
    fill(Annual_RFR, .direction = "up")

  # Calculate CAGR
  start_value <- first(indexed_returns)
  end_value <- last(indexed_returns)
  annual_return <- ((end_value / start_value) ^ (252 / nrow(backtest_df))) - 1

  # Calculate annualized volatility
  annual_vol <- sd(returns) * sqrt(252)

  # Compute avg RFR
  avg_annual_rfr <- mean(backtest_df$Annual_RFR)

  # Calculate Sharpe Ratio
  sharpe_ratio <- (annual_return - avg_annual_rfr) / annual_vol

  # Calculate max drawdown
  max_drawdown <- min(rolling_drawdown)

  # Calculate Sortino Ratio
  neg_returns <- returns[returns < 0]
  sortino_ratio <- (annual_return - avg_annual_rfr) / (sd(neg_returns) * sqrt(252))

  # Calculate Calmar Ratio
  calmar_ratio <- annual_return / abs(max_drawdown)

  # Combine performance metrics
  if (is_benchmark) {
    performance_df <- data.frame(
    Metric = c("CAGR", "Annualized Volatility", "Sharpe Ratio", "Max Drawdown", "Sortino Ratio", "Calmar Ratio"),
    Benchmark = c(annual_return, annual_vol, sharpe_ratio, max_drawdown, sortino_ratio, calmar_ratio)
    )
  } else {
    performance_df <- data.frame(
    Metric = c("CAGR", "Annualized Volatility", "Sharpe Ratio", "Max Drawdown", "Sortino Ratio", "Calmar Ratio"),
    Portfolio = c(annual_return, annual_vol, sharpe_ratio, max_drawdown, sortino_ratio, calmar_ratio)
    )
  }

  return(performance_df)
}


#' Include Benchmark in Backtest Data
#'
#' Merges benchmark data with backtest portfolio data.
#'
#' @param backtest_df A data frame with portfolio returns.
#' @param benchmark_df A data frame with benchmark return data.
#' @param benchmark_ticker The ticker symbol of the benchmark to include.
#'
#' @return A data frame with the merged benchmark data.
include_benchmark <- function(backtest_df, benchmark_df, benchmark_ticker) {

  # Print benchmark
  cat("Benchmark: ", benchmark_ticker, "\n")

  # Print backtest period
  cat("\nBacktest period: ", format(min(backtest_df$Date), "%Y-%m-%d"), "-", format(max(backtest_df$Date), "%Y-%m-%d"), "\n")

  # Load benchmark data (no formula)
  if(!str_detect(benchmark_ticker, "\\+")) {

    # Constant benchmark: A specified annual risk-free rate
    if(str_detect(benchmark_ticker, "RFR_")) {

      # Extract RFR from ticker string
      annual_RFR <- as.numeric(str_split(benchmark_ticker, "_", simplify = TRUE)[2]) / 100

      # Define a vector of unique dates
      unique_dates <- unique(backtest_df$Date)

      # Create a data frame with the unique dates
      benchmark_df <- data.frame(Date = unique_dates)

      # Calculate daily return based on annual RFR
      benchmark_df <- benchmark_df %>%
        arrange(Date) %>%
        mutate(
          Delta_t = as.numeric(Date - lag(Date, default = first(Date))),
          Benchmark_Return = (1 + annual_RFR) ^ (Delta_t / 365.25) - 1
        ) %>%
        select(Date, Benchmark_Return)

    # Standard benchmark: A given ticker
    } else {

      benchmark_df <- benchmark_df %>%
        filter(Ticker == benchmark_ticker) %>%
        select(Date, Benchmark_Return = Return)
    }

  # Set benchmark according to formula
  } else {

    # Parse benchmark formula (eg "0.6*SPY + 0.4*TLT")
    benchmark_components <- str_split(benchmark_ticker, " \\+ ", simplify = TRUE)

    # Extract tickers and weights
    benchmark_weights <- lapply(benchmark_components, function(component) {
      parts <- str_split(component, "\\*", simplify = TRUE)
      weight <- as.numeric(parts[1])
      ticker <- parts[2]
      return(data.frame(Ticker = ticker, Weight = weight, stringsAsFactors = FALSE))
    })

    # Convert list to data frame
    benchmark_weights <- bind_rows(benchmark_weights)

    # Left join to benchmark data
    benchmark_df <- benchmark_df %>%
      filter(Ticker %in% benchmark_weights$Ticker) %>%
      left_join(benchmark_weights, by = "Ticker")

    # Get maximum min-date
    max_min_date <- benchmark_df %>%
      group_by(Ticker) %>%
      summarise(Min_Date = min(Date)) %>%
      summarise(Min_Date = max(Min_Date)) %>%
      pull(Min_Date)

    # Filter dates such that benchmark assets have same date range
    benchmark_df <- benchmark_df %>%
      filter(Date >= max_min_date)

    # Apply weights to benchmark data
    benchmark_df <- benchmark_df %>%
      group_by(Date) %>%
      summarise(Benchmark_Return = sum(Return * Weight))
  }

  # Merge with backtest data
  backtest_df <- backtest_df %>%
    left_join(benchmark_df, by = "Date")

  # Check for missing benchmark data
  if (any(is.na(backtest_df$Benchmark_Return))) {
    cat("Warning: Missing benchmark data for some dates\n")
  }

  # Calculate cumulative returns
  backtest_df <- backtest_df %>%
    mutate(
      Cum_Benchmark_Return = cumprod(1 + Benchmark_Return) - 1,
      Benchmark_Index = 100 * (1 + Cum_Benchmark_Return)
    ) %>%
    select(-Cum_Benchmark_Return)

  return(backtest_df)
}

#' Generate Performance Plots
#'
#' Creates and saves plots for indexed returns, rolling drawdowns, return distributions, and portfolio vs. benchmark returns.
#'
#' @param backtest_df A data frame containing indexed return data.
#'
#' @return Saves multiple plots in the `plots/` directory.
generate_plots <- function(backtest_df) {

  # Define the current directory
  current_dir <- dirname(sys.frame(1)$ofile)

# Pivot the data to long-form
density_df <- backtest_df %>%
  select(Date, Portfolio = Portfolio_Return, Benchmark = Benchmark_Return) %>%
  pivot_longer(
    cols = c(Portfolio, Benchmark),
    names_to = "Ticker",
    values_to = "Return"
  ) %>%
  drop_na()

# Plot the distribution of daily returns
ggplot(density_df, aes(x = Return, fill = Ticker, color = Ticker)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
  labs(
    title = "Distribution of Daily Returns",
    x = "Daily Return",
    y = "Density",
    fill = "Ticker",
    color = "Ticker"
  ) +
  theme(plot.title = element_text(face = "bold", size = 14))
suppressMessages({
  ggsave(file.path(current_dir, "plots/returns_distribution.png"))
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
    ggsave(file.path(current_dir, "plots/rolling_drawdown.png"))
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
      legend.position = "right"
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
    ) +
    guides(color = "none")
  # Combine the two plots vertically with patchwork
  combined_plot <- p1 / p2 + plot_layout(heights = c(3, 1))
  suppressMessages({
    ggsave(
      file.path(current_dir, "plots/indexed_return.png"),
      plot = combined_plot
    )
  })

  # Indexed returns on a log scale
  log_plot <- ggplot(backtest_df, aes(x = Date)) +
    geom_line(aes(y = Benchmark_Index, color = "Benchmark")) +
    geom_line(aes(y = Indexed_Return, color = "Portfolio")) +
    scale_color_manual(values = c("Portfolio" = "blue", "Benchmark" = "black")) +
    scale_y_continuous(trans = 'log10', 
                      labels = comma_format(accuracy = 1)) +
    labs(title = "Log-Scale Indexed Returns",
        x = "Date",
        y = "Indexed Return (Log10)",
        color = "Legend") +
    theme(plot.title = element_text(face = "bold", size = 14))
  # Combine the two plots vertically with patchwork
  combined_log <- log_plot / p2 + plot_layout(heights = c(3, 1))
  suppressMessages({
    ggsave(
      file.path(current_dir, "plots/log_indexed_return.png"),
      plot = combined_log
    )
  })

  # Calculate the performance ratio
  backtest_df <- backtest_df %>%
    mutate(Performance_Ratio = Indexed_Return / Benchmark_Index) %>%
    as.data.frame()

  # Plot the performance ratio
  ggplot(backtest_df, aes(x = Date, y = Performance_Ratio)) +
    geom_line(color = "blue") +
    labs(
      title = "Portfolio vs. Benchmark Performance Ratio",
      x = "Date",
      y = "Performance Ratio"
    ) +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "plots/performance_ratio.png"))
  })

  # Scatter plot of Portfolio vs Benchmark returns
  ggplot(backtest_df, aes(x = Benchmark_Return, y = Portfolio_Return)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "blue") +
    labs(
      title = "Portfolio vs Benchmark Returns",
      x = "Benchmark Return",
      y = "Portfolio Return"
    ) +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "plots/scatter_plot.png"))
  })

  # Calculate monthly returns
  monthly_returns <- backtest_df %>%
    mutate(Month = floor_date(Date, "month")) %>%
    group_by(Month) %>%
    summarise(
      Portfolio = prod(1 + Portfolio_Return) - 1,
      Benchmark = prod(1 + Benchmark_Return) - 1
    ) %>%
    ungroup()

  # Monthly returns scatter plot with quadratic trend line
  ggplot(monthly_returns, aes(x = Benchmark, y = Portfolio)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
                color = "blue", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Monthly Returns: Portfolio vs Benchmark",
        x = "Benchmark Monthly Return",
        y = "Portfolio Monthly Return") +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "plots/monthly_quadratic_scatter.png"))
  })

  # Calculate yearly returns
  yearly_returns <- backtest_df %>%
    mutate(Year = year(Date)) %>%
    group_by(Year) %>%
    summarise(
      Portfolio = prod(1 + Portfolio_Return) - 1,
      Benchmark = prod(1 + Benchmark_Return) - 1
    ) %>%
    ungroup()

  # Yearly returns scatter plot with quadratic trend line
  ggplot(yearly_returns, aes(x = Benchmark, y = Portfolio)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
                color = "blue", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Yearly Returns: Portfolio vs Benchmark",
        x = "Benchmark Yearly Return",
        y = "Portfolio Yearly Return") +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "plots/yearly_quadratic_scatter.png"))
  })
}

#' Plot Portfolio Weights Over Time
#'
#' This function generates and saves a set of time-series plots showing the evolution
#' of portfolio weights for each asset in the portfolio.
#'
#' @param portfolio_df A data frame containing portfolio weights with columns:
#' Date, Ticker, and Weight.
#' @param backtest_df A data frame containing backtest data, used to match the date range.
#'
#' @return Saves a combined grid of weight plots in the `plots/` directory.
plot_weights <- function(portfolio_df, backtest_df) {

  # Set date range to match backtest period
  portfolio_df <- portfolio_df %>%
    filter(Date >= min(backtest_df$Date) & Date <= max(backtest_df$Date))

  # Determine the global min and max of weight
  weight_min <- min(portfolio_df$Weight, na.rm = TRUE)
  weight_max <- max(portfolio_df$Weight, na.rm = TRUE)

  # Identify tickers with only-zero weights
  invalid_tickers <- portfolio_df %>%
    group_by(Ticker) %>%
    summarise(Weight_Sum = sum(Weight)) %>%
    filter(Weight_Sum == 0) %>%
    pull(Ticker)

  # Identify tickers with only NA weights
  invalid_tickers <- portfolio_df %>%
    group_by(Ticker) %>%
    arrange(Date) %>%
    summarise(All_NA = all(is.na(Weight))) %>%
    filter(All_NA) %>%
    pull(Ticker) %>%
    union(invalid_tickers)

  # Filter out invalid tickers
  portfolio_df <- portfolio_df %>%
    filter(!Ticker %in% invalid_tickers)

  # Generate a list of plots for each ticker
  weight_plots <- portfolio_df %>%
    group_split(Ticker) %>%
    lapply(function(df) {
      ggplot(df, aes(x = Date, y = Weight)) +
        geom_line(color = "blue") +
        ylim(weight_min, weight_max) +  # Set consistent y-axis limits
        labs(title = paste("Weight Over Time:", unique(df$Ticker)),
             x = "Date", y = "Weight") +
        theme_minimal()
    })
  # Combine all weight plots into a grid
  stitched_weight_plot <- wrap_plots(weight_plots) + plot_layout(ncol = 2)
  suppressMessages({
    ggsave(file.path(current_dir, "plots/weights_over_time.png"), plot = stitched_weight_plot, width = 12, height = 8)
  })
}


#' Save Strategy to Database
#'
#' This function saves the strategy data to the database for further analysis.
#'
#' @param conn A database connection object.
#' @param backtest_df A data frame containing backtest data with columns: Date, Portfolio_Return, and Indexed_Return.
#' @param name A character string specifying the strategy name (default: "TEMP_STRATEGY").
#'
#' @return Saves the strategy data to the database.
save_strategy <- function(conn, backtest_df, name = "TEMP_STRATEGY") {

  # Convert data to "asset-form"
  strategy_df <- backtest_df %>%
    mutate(Ticker = name) %>%
    select(
      Date,
      Ticker,
      Price = Indexed_Return,
      Return = Portfolio_Return
    )

  # Read current strategies data
  all_strategies_df <- dbGetQuery(conn, "SELECT * FROM strategies")

  # If strategy already exists, remove it
  all_strategies_df <- all_strategies_df %>%
    filter(!Ticker %in% strategy_df$Ticker)

  # Append the new strategy
  all_strategies_df <- all_strategies_df %>%
    bind_rows(strategy_df)

  # Overwrite to the database
  dbWriteTable(
    conn,
    "strategies",
    all_strategies_df,
    overwrite = TRUE,
    append = FALSE
  )
}