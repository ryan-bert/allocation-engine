import pandas as pd

# Define constants
ETF_DATA_PATH = '~/Documents/Financial Data/daily_etf_index_returns.csv'
FUTURES_DATA_PATH = '~/Documents/Financial Data/daily_futures_returns.csv'

def main():

    # Load ETF data
    etf_df = pd.read_csv(ETF_DATA_PATH)
    etf_df["Date"] = pd.to_datetime(etf_df["Date"])

    # Load futures data
    futures_df = pd.read_csv(FUTURES_DATA_PATH)
    futures_df["Date"] = pd.to_datetime(futures_df["Date"])

    # Select tickers for backtest
    etf_df = etf_df[["Date", "SPY"]]
    futures_df = futures_df[["Date", "GC", "CO"]]

    # Combine ETF and futures data
    assets_df = pd.merge(etf_df, futures_df, on="Date", how="inner")

    # Remove initial zero returns
    assets_df = remove_initial_zero_returns(assets_df)


def remove_initial_zero_returns(df):
    
    # Get a list of tickers
    tickers = [col for col in df.columns if col != "Date"]

    # Create a mask where all tickers are nonzero
    mask = True
    for ticker in tickers:
        mask &= (df[ticker] != 0)

    # Find the earliest date in remaining data
    start_date = df.loc[mask, "Date"].min()

    # Keep only rows from start_date onwards
    filtered_df = df[df["Date"] >= start_date].reset_index(drop=True)
    return filtered_df

if __name__ == '__main__':
    main()