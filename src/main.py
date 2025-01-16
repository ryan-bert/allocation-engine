import pandas as pd
from backtest_engine import Engine

# Define constants
ETF_DATA_PATH = '~/Documents/Financial Data/daily_etf_index_returns.csv'
FUTURES_DATA_PATH = '~/Documents/Financial Data/daily_futures_returns.csv'
MON_TO_FRI_DATA_PATH = '~/Documents/Financial Data/Utilities/mon_to_fri_dates.csv'

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

    # Generate weights
    weights_df = assets_df.copy()
    weights_df["SPY"] = 1/3
    weights_df["GC"] = 1/3
    weights_df["CO"] = 1/3

    # Create an instance of the backtest engine
    engine = Engine(weights_df, assets_df)

    # Prepare the backtest
    mon_to_fri_df = pd.read_csv(MON_TO_FRI_DATA_PATH)
    mon_to_fri_df["Date"] = pd.to_datetime(mon_to_fri_df["Date"])
    start_date = pd.to_datetime("2010-01-01")
    end_date = pd.to_datetime("2010-12-31")
    engine.prepare(start_date, end_date, mon_to_fri_df)


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