class Engine:

    def __init__(self, weights, returns, initial_capital=1):

        self.weights_df = weights
        self.returns_df = returns
        self.initial_capital = initial_capital

        self.portfolio_df = None
        self.results = None

    def prepare(self, start_date, end_date, mon_to_fri_df):

        # Filter the data to the specified date range
        self.weights_df = self.weights_df[(self.weights_df.index >= start_date) & (self.weights_df.index <= end_date)]
        self.weights_df.rweset_index(drop=True, inplace=True)
        self.returns_df = self.returns_df[(self.returns_df.index >= start_date) & (self.returns_df.index <= end_date)]
        self.returns_df.reset_index(drop=True, inplace=True)

        # Filter mon_to_fri_df to the specified date range
        mon_to_fri_df = mon_to_fri_df[(mon_to_fri_df.index >= start_date) & (mon_to_fri_df.index <= end_date)]

        # Align the dates of weights by forward filling
        self.weights_df = self.weights_df.merge(mon_to_fri_df, left_index=True, right_index=True, how="right")
        self.weights_df.fillna(method="ffill", inplace=True)

        # Align the dates of returns by filling with zeros
        self.returns_df = self.returns_df.merge(mon_to_fri_df, left_index=True, right_index=True, how="right")
        self.returns_df.fillna(0, inplace=True)


    def run(self, fee_rate=0):
        pass

    def get_results(self):
        pass