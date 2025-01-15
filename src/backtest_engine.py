class Engine:

    def __init__(self, weights, returns, initial_capital=1):

        self.weights_df = weights
        self.returns_df = returns
        self.initial_capital = initial_capital

        self.portfolio_df = None
        self.results = None

    def prepare(self):
        pass

    def run(self, start_date, end_date, fee_rate=0):
        pass

    def get_results(self):
        pass