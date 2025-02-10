# Backtest Engine for ETF & Futures Portfolio 👨🏻‍💻

This project implements a **backtest engine** to evaluate the performance of a **portfolio of ETFs and futures** over time. The backtester simulates portfolio performance, incorporating **daily returns, transaction costs, and key risk metrics**. 

## **Overview**
- **Data Sources**: Historical ETF and futures daily returns.  
- **Portfolio Construction**: Fixed **equal-weight allocation** across selected assets.  
- **Backtest Execution**: Simulates daily performance using **realistic trading days**.  
- **Performance Metrics**: Calculates **Sharpe Ratio, Max Drawdown, Annualized Return & Volatility**.  

## **Core Components**
### **1️⃣ Backtest Engine (`backtest_engine.py`)**
Handles the **simulation of portfolio performance**, given historical returns and weights.

#### ⚙ **Key Functions**
- **`prepare(start_date, end_date, mon_to_fri_df)`**  
  - Aligns data with **actual trading days**.
  - Ensures **weights and returns** are forward-filled properly.
- **`run(fee_rate=0)`**  
  - Iterates through each day, applying **portfolio returns and transaction costs**.
  - Tracks **daily return, cumulative return, and portfolio value**.
- **`get_results()`**  
  - Computes **Sharpe Ratio, Max Drawdown, Annualized Return & Volatility** using **Empyrical**.

### **2️⃣ Backtest Execution (`main.py`)**
- **Loads ETF & futures data** from CSV files.
- **Constructs a portfolio** with fixed allocations (`SPY`, `GC`, `CL`).
- **Runs the backtest** from **2000 to 2024**.
- **Outputs final performance metrics**.

#### 📌 **Example Output**
```
Sharpe Ratio: 0.89
Max Drawdown: -0.25
Annualized Return: 0.07
Annualized Volatility: 0.15
```

## **Workflow**
1. **Load market data** (ETF & futures historical returns).  
2. **Preprocess data** to match actual trading days.  
3. **Run the backtest** with portfolio allocations.  
4. **Compute risk-adjusted performance metrics**.  

---

This backtest engine provides a **robust and extensible framework** for evaluating systematic portfolio strategies using **ETF and futures data**. 📈🚀