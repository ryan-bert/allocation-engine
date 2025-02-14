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
  filter(Ticker != "TLT")

# Stock selection
stocks_df <- stocks_df %>%
  filter(Ticker %in% c("AAPL", "MSFT", "AMZN", "GOOGL"))



###################### STRATEGY ######################







###################### BACKTEST ######################




###################### PERFORMANCE ######################
