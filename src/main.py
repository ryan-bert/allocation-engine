import pandas as pd

# Define constants
ETF_DATA_PATH = 'data/etf_data.csv'
FUTURES_DATA_PATH = 'data/futures_data.csv'

def main():

    # Load ETF data
    etf_df = pd.read_csv(ETF_DATA_PATH, index_col=0)
    etf_df["Date"] = pd.to_datetime(etf_df["Date"])

    # Load futures data
    futures_df = pd.read_csv(FUTURES_DATA_PATH, index_col=0)
    futures_df["Date"] = pd.to_datetime(futures_df["Date"])

def remove_initial_zero_returns(df, tickers, date_col="Date"):

    # Create a mask where ALL returns are nonzero
    mask = True
    for ticker in tickers:
        mask &= (df[ticker] != 0)
    
    # Find earliest date where ALL returns are nonzero
    start_date = df.loc[mask, date_col].min()

    # Keep only rows from start_date onward
    filtered_df = df[df[date_col] >= start_date].reset_index(drop=True)
    return filtered_df

if __name__ == '__main__':
    main()