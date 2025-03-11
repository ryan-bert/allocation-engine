rm(list = ls())

suppressMessages({
  library(DBI)
  library(RPostgres)
  library(jsonlite)
  library(stringr)
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
stocks_df$Date <- as.Date(stocks_df$Date)
benchmark_df <- dbGetQuery(conn, "SELECT * FROM etfs")
benchmark_df$Date <- as.Date(benchmark_df$Date)

benchmark_formula <- "0.6*SPY + 0.4*TLT"

# Parse benchmark formula (eg "0.6*SPY + 0.4*TLT")
benchmark_components <- str_split(benchmark_formula, " \\+ ", simplify = TRUE)

# Extract tickers and weights
parsed_components <- lapply(benchmark_components, function(component) {
  parts <- str_split(component, "\\*", simplify = TRUE)
  weight <- as.numeric(parts[1])
  ticker <- parts[2]
  return(data.frame(Ticker = ticker, Weight = weight, stringsAsFactors = FALSE))
})

parsed_components <- bind_rows(parsed_components)

# Left join to benchmark data
benchmark_df <- benchmark_df %>%
  filter(Ticker %in% parsed_components$Ticker) %>%
  left_join(parsed_components, by = "Ticker")

# Get maximum min-date
min_date <- benchmark_df %>%
  group_by(Ticker) %>%
  summarise(Min_Date = min(Date)) %>%
  summarise(Min_Date = max(Min_Date)) %>%
  pull(Min_Date)

# Filter dates such that benchmark assets have same date range
benchmark_df <- benchmark_df %>%
  filter(Date >= min_date)

# Apply weights to benchmark data
benchmark_df <- benchmark_df %>%
  group_by(Date) %>%
  mutate(Return = sum(Return * Weight, na.rm = TRUE))


