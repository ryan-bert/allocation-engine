suppressMessages({
  library(dplyr)
})

apply_strategy <- function(portfolio_df) {

  # Avoid "no visible binding for global variable" warnings
  Weight <- NULL

  # Define allocation
  allocation <- c(
    "SPY" = 0,
    "QQQ" = 0.55,
    "SOXX" = 0,
    "EFA" = 0,
    "BRK-B" = 0,
    "GLD" = 0.35
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

  return(portfolio_df)
}