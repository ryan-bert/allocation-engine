import pandas as pd

futures_df = pd.read_csv('~/Documents/Financial Data/daily_futures_returns.csv')

gold_df = futures_df[['Date', 'GC']].copy()

# Truncate to after 2000 and befor 2024
gold_df = gold_df[gold_df["Date"] >= "2000-01-01"]
gold_df = gold_df[gold_df["Date"] <= "2024-01-01"]

gold_df["Cumulative Return"] = (1 + gold_df["GC"]).cumprod()

last_cumulative_return = gold_df["Cumulative Return"].iloc[-1]
print(last_cumulative_return)

print(gold_df)