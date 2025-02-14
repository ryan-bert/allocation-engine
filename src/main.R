suppressMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
  library(dplyr)
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

# Close the connection
dbDisconnect(conn)

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







###################### BACKTEST ######################




###################### PERFORMANCE ######################
