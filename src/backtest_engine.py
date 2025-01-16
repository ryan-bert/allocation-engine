import pandas as pd

class Engine:

    def __init__(self, weights, returns, initial_capital=1):

        self.weights_df = weights
        self.returns_df = returns
        self.initial_capital = initial_capital

        self.portfolio_df = None
        self.results = None

    def prepare(self, start_date, end_date, mon_to_fri_df):

        # Filter weights and returns to the specified date range
        self.weights_df = self.weights_df[(self.weights_df["Date"] >= start_date) & (self.weights_df["Date"] <= end_date)]
        self.weights_df.reset_index(drop=True, inplace=True)

        self.returns_df = self.returns_df[(self.returns_df["Date"] >= start_date) & (self.returns_df["Date"] <= end_date)]
        self.returns_df.reset_index(drop=True, inplace=True)

        # Filter mon_to_fri_df to the specified date range
        mon_to_fri_df = mon_to_fri_df[(mon_to_fri_df["Date"] >= start_date) & (mon_to_fri_df["Date"] <= end_date)].reset_index(drop=True)

        # Ensure all DataFrames are indexed by Date
        self.weights_df.set_index("Date", inplace=True)
        self.returns_df.set_index("Date", inplace=True)
        mon_to_fri_df.set_index("Date", inplace=True)

        # Align weights with trading days (mon_to_fri_df) by forward filling
        self.weights_df = mon_to_fri_df.merge(self.weights_df, left_index=True, right_index=True, how="left")
        self.weights_df.ffill(inplace=True)

        # Align returns with trading days by filling missing values with 0
        self.returns_df = mon_to_fri_df.merge(self.returns_df, left_index=True, right_index=True, how="left")
        self.returns_df.fillna(0, inplace=True)


    def run(self, fee_rate=0):

        # Initialization
        portfolio_value = self.initial_capital
        daily_returns = []
        cumulative_returns = []
        portfolio_values = []
        transaction_costs = []

        prev_weights = None

        # Iterate over each date
        for date in self.weights_df.index:
            # Extract weights and returns for the current date
            current_weights = self.weights_df.loc[date]
            current_returns = self.returns_df.loc[date]

            # Calculate daily portfolio return
            daily_return = (current_weights * current_returns).sum()

            # Calculate transaction costs
            if prev_weights is not None:
                weight_change = (current_weights - prev_weights).abs().sum()
                cost = fee_rate * weight_change * portfolio_value
            else:
                cost = 0.0

            # Update portfolio value
            portfolio_value *= (1 + daily_return)
            portfolio_value -= cost

            # Store results
            portfolio_values.append(portfolio_value)
            daily_returns.append(daily_return)
            transaction_costs.append(cost)
            cumulative_returns.append((portfolio_value / self.initial_capital) - 1)

            # Update previous weights
            prev_weights = current_weights
        
        # Compile results into a DataFrame
        self.portfolio_df = pd.DataFrame({
            'Date': self.weights_df.index,
            'Portfolio Value': portfolio_values,
            'Daily Return': daily_returns,
            'Cumulative Return': cumulative_returns,
            'Transaction Cost': transaction_costs
        })


    def get_results(self):
        pass