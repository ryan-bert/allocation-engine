class Engine:

    def __init__(self, weights, returns, initial_capital=1):

        self.weights_df = weights
        self.returns_df = returns
        self.initial_capital = initial_capital

        self.portfolio_df = None
        self.results = None

    def prepare(self, start_date, end_date):

        # Filter the data to the specified date range
        self.weights_df = self.weights_df[(self.weights_df.index >= start_date) & (self.weights_df.index <= end_date)]
        self.returns_df = self.returns_df[(self.returns_df.index >= start_date) & (self.returns_df.index <= end_date)]

        #! HANDLE MISSING DATA - DATES MUST BE ALIGNED
            # - Maybe drop rows with missing data
            # - Maybe forward fill weights

    def run(self, fee_rate=0):
        pass

    def get_results(self):
        pass