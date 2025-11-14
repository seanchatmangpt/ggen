"""Risk Management Python Implementation"""
from dataclasses import dataclass
from typing import List
import numpy as np
from scipy import stats

@dataclass
class Position:
    instrument: str
    quantity: float
    market_value: float

@dataclass
class Portfolio:
    id: str
    positions: List[Position]
    market_value: float

class VaRCalculator:
    @staticmethod
    def historical_var(returns: List[float], confidence_level: float) -> float:
        sorted_returns = sorted(returns)
        idx = int((1 - confidence_level) * len(sorted_returns))
        return -sorted_returns[idx]

    @staticmethod
    def parametric_var(mean: float, std_dev: float, confidence_level: float) -> float:
        z_score = stats.norm.ppf(confidence_level)
        return -(mean - z_score * std_dev)

    @staticmethod
    def monte_carlo_var(portfolio: Portfolio, scenarios: int, confidence_level: float) -> float:
        simulated_returns = np.random.normal(0, 0.02, scenarios)
        return VaRCalculator.historical_var(simulated_returns.tolist(), confidence_level)

class GreeksCalculator:
    @staticmethod
    def delta(spot: float, strike: float, vol: float, time: float, rate: float) -> float:
        d1 = (np.log(spot / strike) + (rate + 0.5 * vol ** 2) * time) / (vol * np.sqrt(time))
        return stats.norm.cdf(d1)

    @staticmethod
    def gamma(spot: float, strike: float, vol: float, time: float, rate: float) -> float:
        d1 = (np.log(spot / strike) + (rate + 0.5 * vol ** 2) * time) / (vol * np.sqrt(time))
        return stats.norm.pdf(d1) / (spot * vol * np.sqrt(time))
