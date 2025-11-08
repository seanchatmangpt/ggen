"""
Inventory Management - Demand Forecasting (Python)
Uses historical sales data to predict future demand and optimize stock levels
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import List, Dict
import statistics


@dataclass
class SalesDataPoint:
    """Historical sales data point"""
    product_id: str
    date: datetime
    quantity_sold: int
    revenue: float


class DemandForecaster:
    """
    Demand forecasting engine for inventory optimization
    """

    def __init__(self):
        self.sales_history: Dict[str, List[SalesDataPoint]] = {}

    def record_sale(self, sale: SalesDataPoint):
        """Record a sale for historical tracking"""
        if sale.product_id not in self.sales_history:
            self.sales_history[sale.product_id] = []
        self.sales_history[sale.product_id].append(sale)

    def forecast_demand(
        self,
        product_id: str,
        forecast_days: int = 30,
    ) -> Dict[str, float]:
        """
        Forecast demand using moving average method
        """
        history = self.sales_history.get(product_id, [])
        if len(history) < 7:
            return {'daily_demand': 0.0, 'forecast_total': 0.0}

        # Calculate daily average from last 30 days
        recent_sales = [
            s for s in history
            if s.date >= datetime.now() - timedelta(days=30)
        ]

        daily_totals = {}
        for sale in recent_sales:
            date_key = sale.date.date()
            daily_totals[date_key] = daily_totals.get(date_key, 0) + sale.quantity_sold

        if not daily_totals:
            return {'daily_demand': 0.0, 'forecast_total': 0.0}

        daily_demand = statistics.mean(daily_totals.values())
        forecast_total = daily_demand * forecast_days

        return {
            'daily_demand': daily_demand,
            'forecast_total': forecast_total,
            'confidence': min(len(daily_totals) / 30.0, 1.0),  # More history = more confidence
        }

    def calculate_safety_stock(
        self,
        product_id: str,
        lead_time_days: int,
        service_level: float = 0.95,
    ) -> int:
        """
        Calculate safety stock based on demand variability
        """
        history = self.sales_history.get(product_id, [])
        if len(history) < 14:
            return 0

        # Calculate demand variability (standard deviation)
        recent_sales = [
            s for s in history
            if s.date >= datetime.now() - timedelta(days=30)
        ]

        daily_totals = {}
        for sale in recent_sales:
            date_key = sale.date.date()
            daily_totals[date_key] = daily_totals.get(date_key, 0) + sale.quantity_sold

        if len(daily_totals) < 7:
            return 0

        demand_values = list(daily_totals.values())
        std_dev = statistics.stdev(demand_values)

        # Safety stock = Z-score * std_dev * sqrt(lead_time)
        z_score = 1.65 if service_level >= 0.95 else 1.28  # Simplified
        safety_stock = z_score * std_dev * (lead_time_days ** 0.5)

        return int(safety_stock)


# Example usage
if __name__ == '__main__':
    forecaster = DemandForecaster()

    # Simulate sales history
    for day in range(30):
        forecaster.record_sale(SalesDataPoint(
            product_id='WIDGET-PRO',
            date=datetime.now() - timedelta(days=30 - day),
            quantity_sold=50 + (day % 10),
            revenue=500.0,
        ))

    # Forecast demand
    forecast = forecaster.forecast_demand('WIDGET-PRO', forecast_days=30)
    print(f"Daily Demand: {forecast['daily_demand']:.1f} units")
    print(f"30-Day Forecast: {forecast['forecast_total']:.1f} units")

    # Calculate safety stock
    safety_stock = forecaster.calculate_safety_stock('WIDGET-PRO', lead_time_days=7)
    print(f"Recommended Safety Stock: {safety_stock} units")
