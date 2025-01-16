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
        pass

    def get_results(self):
        pass