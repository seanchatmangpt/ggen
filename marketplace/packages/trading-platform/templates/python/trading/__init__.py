"""
Trading Platform - Python Backtesting and Analytics
Provides strategy backtesting and performance analysis
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Dict, Optional
from decimal import Decimal
import pandas as pd
import numpy as np

# ============================================================
# Data Classes
# ============================================================

@dataclass
class OHLCV:
    """Candlestick data"""
    timestamp: datetime
    open: Decimal
    high: Decimal
    low: Decimal
    close: Decimal
    volume: Decimal

@dataclass
class Position:
    """Trading position"""
    symbol: str
    quantity: Decimal
    entry_price: Decimal
    entry_time: datetime
    current_price: Decimal
    side: str  # 'LONG' or 'SHORT'

    @property
    def unrealized_pnl(self) -> Decimal:
        if self.side == 'LONG':
            return (self.current_price - self.entry_price) * self.quantity
        else:
            return (self.entry_price - self.current_price) * self.quantity

    @property
    def unrealized_pnl_percent(self) -> Decimal:
        return (self.unrealized_pnl / (self.entry_price * self.quantity)) * 100

@dataclass
class Trade:
    """Executed trade"""
    trade_id: str
    symbol: str
    side: str
    quantity: Decimal
    price: Decimal
    timestamp: datetime
    pnl: Optional[Decimal] = None

# ============================================================
# Backtesting Engine
# ============================================================

class BacktestEngine:
    """Backtest trading strategies"""

    def __init__(self, initial_capital: Decimal = Decimal('100000')):
        self.initial_capital = initial_capital
        self.capital = initial_capital
        self.positions: Dict[str, Position] = {}
        self.trades: List[Trade] = []
        self.equity_curve: List[tuple] = []

    def run_backtest(
        self,
        data: pd.DataFrame,
        strategy_func,
        commission: Decimal = Decimal('0.001'),
    ) -> Dict:
        """
        Run backtest on historical data

        Args:
            data: DataFrame with OHLCV data
            strategy_func: Function that returns trading signals
            commission: Commission rate (0.001 = 0.1%)

        Returns:
            Dictionary with backtest results
        """
        for idx, row in data.iterrows():
            # Update positions with current price
            for symbol, pos in self.positions.items():
                pos.current_price = row['close']

            # Generate trading signal
            signal = strategy_func(data.loc[:idx], self.positions)

            # Execute signal
            if signal:
                self.execute_signal(
                    symbol=signal['symbol'],
                    side=signal['side'],
                    quantity=signal['quantity'],
                    price=row['close'],
                    timestamp=row['timestamp'],
                    commission=commission,
                )

            # Record equity
            equity = self.calculate_equity()
            self.equity_curve.append((row['timestamp'], equity))

        return self.calculate_performance_metrics()

    def execute_signal(
        self,
        symbol: str,
        side: str,
        quantity: Decimal,
        price: Decimal,
        timestamp: datetime,
        commission: Decimal,
    ):
        """Execute trading signal"""
        trade_value = quantity * price
        commission_cost = trade_value * commission

        if side in ['BUY', 'LONG']:
            # Open long position
            if self.capital < trade_value + commission_cost:
                return  # Insufficient capital

            self.capital -= (trade_value + commission_cost)

            self.positions[symbol] = Position(
                symbol=symbol,
                quantity=quantity,
                entry_price=price,
                entry_time=timestamp,
                current_price=price,
                side='LONG',
            )

            self.trades.append(Trade(
                trade_id=f"TRADE-{len(self.trades) + 1}",
                symbol=symbol,
                side='BUY',
                quantity=quantity,
                price=price,
                timestamp=timestamp,
            ))

        elif side in ['SELL', 'CLOSE_LONG']:
            # Close long position
            if symbol not in self.positions:
                return

            pos = self.positions[symbol]
            pnl = pos.unrealized_pnl - commission_cost
            self.capital += (trade_value - commission_cost)

            self.trades.append(Trade(
                trade_id=f"TRADE-{len(self.trades) + 1}",
                symbol=symbol,
                side='SELL',
                quantity=quantity,
                price=price,
                timestamp=timestamp,
                pnl=pnl,
            ))

            del self.positions[symbol]

    def calculate_equity(self) -> Decimal:
        """Calculate current account equity"""
        position_value = sum(
            pos.current_price * pos.quantity
            for pos in self.positions.values()
        )
        return self.capital + position_value

    def calculate_performance_metrics(self) -> Dict:
        """Calculate backtest performance metrics"""
        if not self.equity_curve:
            return {}

        # Convert equity curve to pandas for analysis
        df = pd.DataFrame(self.equity_curve, columns=['timestamp', 'equity'])
        df['returns'] = df['equity'].pct_change()

        # Calculate metrics
        total_return = ((df['equity'].iloc[-1] - self.initial_capital) /
                       self.initial_capital * 100)

        winning_trades = [t for t in self.trades if t.pnl and t.pnl > 0]
        losing_trades = [t for t in self.trades if t.pnl and t.pnl < 0]

        win_rate = (len(winning_trades) / len(self.trades) * 100
                   if self.trades else 0)

        avg_win = (sum(t.pnl for t in winning_trades) / len(winning_trades)
                  if winning_trades else Decimal(0))
        avg_loss = (sum(abs(t.pnl) for t in losing_trades) / len(losing_trades)
                   if losing_trades else Decimal(0))

        profit_factor = (avg_win / avg_loss if avg_loss > 0 else Decimal(0))

        # Sharpe ratio (assuming 252 trading days, risk-free rate 0)
        returns_std = df['returns'].std()
        sharpe_ratio = (df['returns'].mean() / returns_std * np.sqrt(252)
                       if returns_std > 0 else 0)

        # Maximum drawdown
        df['cummax'] = df['equity'].cummax()
        df['drawdown'] = (df['equity'] - df['cummax']) / df['cummax'] * 100
        max_drawdown = df['drawdown'].min()

        return {
            'total_return_pct': float(total_return),
            'total_trades': len(self.trades),
            'winning_trades': len(winning_trades),
            'losing_trades': len(losing_trades),
            'win_rate_pct': float(win_rate),
            'avg_win': float(avg_win),
            'avg_loss': float(avg_loss),
            'profit_factor': float(profit_factor),
            'sharpe_ratio': float(sharpe_ratio),
            'max_drawdown_pct': float(max_drawdown),
            'final_equity': float(df['equity'].iloc[-1]),
        }

# ============================================================
# Technical Indicators
# ============================================================

class TechnicalIndicators:
    """Calculate technical indicators"""

    @staticmethod
    def sma(data: pd.Series, period: int) -> pd.Series:
        """Simple Moving Average"""
        return data.rolling(window=period).mean()

    @staticmethod
    def ema(data: pd.Series, period: int) -> pd.Series:
        """Exponential Moving Average"""
        return data.ewm(span=period, adjust=False).mean()

    @staticmethod
    def rsi(data: pd.Series, period: int = 14) -> pd.Series:
        """Relative Strength Index"""
        delta = data.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=period).mean()
        rs = gain / loss
        return 100 - (100 / (1 + rs))

    @staticmethod
    def macd(data: pd.Series, fast: int = 12, slow: int = 26, signal: int = 9):
        """Moving Average Convergence Divergence"""
        ema_fast = data.ewm(span=fast, adjust=False).mean()
        ema_slow = data.ewm(span=slow, adjust=False).mean()
        macd_line = ema_fast - ema_slow
        signal_line = macd_line.ewm(span=signal, adjust=False).mean()
        histogram = macd_line - signal_line
        return macd_line, signal_line, histogram

    @staticmethod
    def bollinger_bands(data: pd.Series, period: int = 20, std_dev: float = 2.0):
        """Bollinger Bands"""
        sma = data.rolling(window=period).mean()
        std = data.rolling(window=period).std()
        upper_band = sma + (std * std_dev)
        lower_band = sma - (std * std_dev)
        return upper_band, sma, lower_band

# ============================================================
# Example Strategy
# ============================================================

def simple_ma_crossover_strategy(data: pd.DataFrame, positions: Dict) -> Optional[Dict]:
    """
    Simple moving average crossover strategy
    Buy when short MA crosses above long MA
    Sell when short MA crosses below long MA
    """
    if len(data) < 50:
        return None

    # Calculate moving averages
    data['SMA_20'] = TechnicalIndicators.sma(data['close'], 20)
    data['SMA_50'] = TechnicalIndicators.sma(data['close'], 50)

    # Get current and previous values
    curr_short = data['SMA_20'].iloc[-1]
    curr_long = data['SMA_50'].iloc[-1]
    prev_short = data['SMA_20'].iloc[-2]
    prev_long = data['SMA_50'].iloc[-2]

    symbol = 'TEST'

    # Buy signal (golden cross)
    if prev_short <= prev_long and curr_short > curr_long and symbol not in positions:
        return {
            'symbol': symbol,
            'side': 'BUY',
            'quantity': Decimal('10'),
        }

    # Sell signal (death cross)
    if prev_short >= prev_long and curr_short < curr_long and symbol in positions:
        return {
            'symbol': symbol,
            'side': 'SELL',
            'quantity': positions[symbol].quantity,
        }

    return None

# ============================================================
# Example Usage
# ============================================================

if __name__ == '__main__':
    # Example: Run backtest
    engine = BacktestEngine(initial_capital=Decimal('100000'))

    # Generate sample data
    dates = pd.date_range(start='2024-01-01', end='2024-12-31', freq='D')
    data = pd.DataFrame({
        'timestamp': dates,
        'open': 100 + np.random.randn(len(dates)).cumsum(),
        'high': 105 + np.random.randn(len(dates)).cumsum(),
        'low': 95 + np.random.randn(len(dates)).cumsum(),
        'close': 100 + np.random.randn(len(dates)).cumsum(),
        'volume': np.random.randint(1000, 10000, len(dates)),
    })

    # Run backtest
    results = engine.run_backtest(data, simple_ma_crossover_strategy)

    print("Backtest Results:")
    print(f"  Total Return: {results['total_return_pct']:.2f}%")
    print(f"  Total Trades: {results['total_trades']}")
    print(f"  Win Rate: {results['win_rate_pct']:.2f}%")
    print(f"  Sharpe Ratio: {results['sharpe_ratio']:.2f}")
    print(f"  Max Drawdown: {results['max_drawdown_pct']:.2f}%")
