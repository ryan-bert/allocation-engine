suppressMessages({
  library(dplyr)
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