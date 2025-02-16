suppressMessages({
  library(dplyr)
  library(tidyr)
})

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
run_backtest <- function(portfolio_df, start_date) {

  # Avoid "no visible binding for global variable" warnings
  Date <- Return <- Weight <- Portfolio_Return <- Cumulative_Return <- NULL

  # Calculate portfolio returns
  backtest_df <- portfolio_df %>%
    group_by(Date) %>%
    summarise(
      Portfolio_Return = sum(Return * Weight)
    ) %>%
    ungroup()

  # Filter data to start date
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

  return(backtest_df)
}

compute_drawdown <- function(backtest_df, is_benchmark = FALSE) {

  # Avoid "no visible binding for global variable" warnings
  Benchmark_Index <- Roll_Max <- Indexed_Return <- NULL

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

analyse_performance <- function(backtest_df, bonds_df, is_benchmark = FALSE) {

  # Avoid "no visible binding for global variable" warnings
  Ticker <- Yield <- Date <- Annual_RFR <- NULL

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
  bonds_df <- bonds_df %>%
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

include_benchmark <- function(backtest_df, benchmark_df, benchmark_ticker) {

  # Avoid "no visible binding for global variable" warnings
  Ticker <- Date <- Return <- Benchmark_Return <- Cum_Benchmark_Return <- NULL

  # Load benchmark data
  benchmark_df <- benchmark_df %>%
    filter(Ticker == benchmark_ticker) %>%
    select(Date, Benchmark_Return = Return)

  # Merge with backtest data
  backtest_df <- backtest_df %>%
    left_join(benchmark_df, by = "Date")

  # Calculate cumulative returns
  backtest_df <- backtest_df %>%
    mutate(
      Cum_Benchmark_Return = cumprod(1 + Benchmark_Return) - 1,
      Benchmark_Index = 100 * (1 + Cum_Benchmark_Return)
    ) %>%
    select(-Cum_Benchmark_Return)

  return(backtest_df)
}