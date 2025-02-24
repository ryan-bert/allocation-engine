suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(lubridate)
  library(scales)
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

  # Avoid "no visible binding for global variable" warnings
  Ticker <- Date <- min_date <- Price <- NULL

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

#' Compute Portfolio or Benchmark Drawdown
#'
#' This function calculates rolling drawdown for either the portfolio or a benchmark.
#'
#' @param backtest_df A data frame containing indexed return data.
#' @param is_benchmark A logical value; if TRUE, computes benchmark drawdown. Default is FALSE (portfolio).
#'
#' @return A data frame with added drawdown columns.
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

#' Analyze Portfolio Performance
#'
#' Computes key performance metrics including CAGR, volatility, Sharpe ratio, drawdowns, Sortino ratio, and Calmar ratio.
#'
#' @param backtest_df A data frame with portfolio returns.
#' @param bonds_df A data frame containing risk-free rate data.
#' @param is_benchmark A logical value; if TRUE, computes benchmark performance.
#'
#' @return A data frame with key performance metrics.
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

#' Generate Performance Plots
#'
#' Creates and saves plots for indexed returns, rolling drawdowns, return distributions, and portfolio vs. benchmark returns.
#'
#' @param backtest_df A data frame containing indexed return data.
#'
#' @return Saves multiple plots in the `../plots/` directory.
generate_plots <- function(backtest_df) {

  # Avoid "no visible binding for global variable" warnings
  Portfolio_Return <- Date <- Benchmark_Index <- Indexed_Return <- NULL
  Benchmark_Drawdown <- Benchmark_Return <- Drawdown <- combined_plot <- NULL

  # Define the current directory
  current_dir <- dirname(sys.frame(1)$ofile)


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
    ggsave(
      file.path(current_dir, "../plots/combined_plot.png"),
      plot = combined_plot
    )
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

  # Indexed returns on a log scale
  ggplot(backtest_df, aes(x = Date)) +
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
  suppressMessages({
    ggsave(file.path(current_dir, "../plots/log_scale_returns.png"))
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
                color = "red", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Monthly Returns: Portfolio vs Benchmark",
        x = "Benchmark Monthly Return",
        y = "Portfolio Monthly Return") +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "../plots/monthly_quadratic_scatter.png"))
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
                color = "red", se = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = "Yearly Returns: Portfolio vs Benchmark",
        x = "Benchmark Yearly Return",
        y = "Portfolio Yearly Return") +
    theme(plot.title = element_text(face = "bold", size = 14))
  suppressMessages({
    ggsave(file.path(current_dir, "../plots/yearly_quadratic_scatter.png"))
  })
}

plot_weights <- function(portfolio_df, backtest_df) {

  # Set date range to match backtest period
  portfolio_df <- portfolio_df %>%
    filter(Date >= min(backtest_df$Date) & Date <= max(backtest_df$Date))

  # Determine the global min and max of weight
  weight_min <- min(portfolio_df$Weight, na.rm = TRUE)
  weight_max <- max(portfolio_df$Weight, na.rm = TRUE)

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