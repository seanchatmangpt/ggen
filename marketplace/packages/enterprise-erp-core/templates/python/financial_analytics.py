"""
============================================================================
Enterprise ERP Core - Financial Analytics (Python)
Advanced financial analysis, forecasting, and reporting
============================================================================
"""

from dataclasses import dataclass
from datetime import date, datetime
from typing import List, Dict, Optional, Tuple
from decimal import Decimal
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression


# ----------------------------------------------------------------------------
# Data Classes
# ----------------------------------------------------------------------------

@dataclass
class FinancialRatio:
    """Financial ratio calculation result"""
    name: str
    value: float
    benchmark: Optional[float] = None
    interpretation: str = ""


@dataclass
class ForecastResult:
    """Revenue or expense forecast result"""
    period: str
    forecasted_value: float
    confidence_interval: Tuple[float, float]
    model_accuracy: float


@dataclass
class VarianceAnalysis:
    """Budget vs actual variance analysis"""
    category: str
    budgeted: Decimal
    actual: Decimal
    variance: Decimal
    variance_pct: float
    explanation: str = ""


# ----------------------------------------------------------------------------
# Financial Analytics Engine
# ----------------------------------------------------------------------------

class FinancialAnalytics:
    """Advanced financial analytics and forecasting"""

    def __init__(self):
        self.historical_data: Dict[str, pd.DataFrame] = {}

    def calculate_financial_ratios(
        self,
        total_assets: Decimal,
        total_liabilities: Decimal,
        current_assets: Decimal,
        current_liabilities: Decimal,
        revenue: Decimal,
        net_income: Decimal,
        cost_of_goods_sold: Decimal,
        inventory: Decimal
    ) -> List[FinancialRatio]:
        """Calculate key financial ratios"""

        ratios = []

        # Current Ratio
        if current_liabilities > 0:
            current_ratio = float(current_assets / current_liabilities)
            ratios.append(FinancialRatio(
                name="Current Ratio",
                value=current_ratio,
                benchmark=2.0,
                interpretation="Excellent" if current_ratio >= 2.0 else "Adequate" if current_ratio >= 1.0 else "Concerning"
            ))

        # Debt-to-Equity Ratio
        equity = total_assets - total_liabilities
        if equity > 0:
            debt_to_equity = float(total_liabilities / equity)
            ratios.append(FinancialRatio(
                name="Debt-to-Equity Ratio",
                value=debt_to_equity,
                benchmark=1.0,
                interpretation="Low leverage" if debt_to_equity < 1.0 else "Moderate leverage" if debt_to_equity < 2.0 else "High leverage"
            ))

        # Return on Assets (ROA)
        if total_assets > 0:
            roa = float(net_income / total_assets)
            ratios.append(FinancialRatio(
                name="Return on Assets",
                value=roa,
                benchmark=0.05,
                interpretation="Strong" if roa >= 0.05 else "Moderate" if roa >= 0.02 else "Weak"
            ))

        # Profit Margin
        if revenue > 0:
            profit_margin = float(net_income / revenue)
            ratios.append(FinancialRatio(
                name="Profit Margin",
                value=profit_margin,
                benchmark=0.10,
                interpretation="Excellent" if profit_margin >= 0.10 else "Good" if profit_margin >= 0.05 else "Needs improvement"
            ))

        # Inventory Turnover
        if inventory > 0:
            inventory_turnover = float(cost_of_goods_sold / inventory)
            ratios.append(FinancialRatio(
                name="Inventory Turnover",
                value=inventory_turnover,
                benchmark=6.0,
                interpretation="Efficient" if inventory_turnover >= 6.0 else "Moderate" if inventory_turnover >= 3.0 else "Slow moving"
            ))

        return ratios

    def forecast_revenue(
        self,
        historical_revenue: pd.DataFrame,
        periods_ahead: int = 12
    ) -> List[ForecastResult]:
        """Forecast revenue using linear regression"""

        # Prepare data
        df = historical_revenue.copy()
        df['period_num'] = range(len(df))

        X = df[['period_num']].values
        y = df['revenue'].values

        # Train model
        model = LinearRegression()
        model.fit(X, y)

        # Calculate R²
        r_squared = model.score(X, y)

        # Generate forecasts
        forecasts = []
        last_period = len(df)

        for i in range(periods_ahead):
            period_num = last_period + i
            forecast_value = model.predict([[period_num]])[0]

            # Simple confidence interval (±10%)
            ci_lower = forecast_value * 0.9
            ci_upper = forecast_value * 1.1

            forecasts.append(ForecastResult(
                period=f"Period {period_num + 1}",
                forecasted_value=forecast_value,
                confidence_interval=(ci_lower, ci_upper),
                model_accuracy=r_squared
            ))

        return forecasts

    def variance_analysis(
        self,
        budget_data: Dict[str, Decimal],
        actual_data: Dict[str, Decimal]
    ) -> List[VarianceAnalysis]:
        """Analyze budget vs actual variances"""

        variances = []

        for category in budget_data.keys():
            budgeted = budget_data.get(category, Decimal(0))
            actual = actual_data.get(category, Decimal(0))
            variance = actual - budgeted

            if budgeted != 0:
                variance_pct = float(variance / budgeted * 100)
            else:
                variance_pct = 0.0

            # Determine explanation
            if abs(variance_pct) < 5:
                explanation = "On target"
            elif variance_pct > 0:
                explanation = f"Over budget by {abs(variance_pct):.1f}%"
            else:
                explanation = f"Under budget by {abs(variance_pct):.1f}%"

            variances.append(VarianceAnalysis(
                category=category,
                budgeted=budgeted,
                actual=actual,
                variance=variance,
                variance_pct=variance_pct,
                explanation=explanation
            ))

        return variances

    def cash_flow_projection(
        self,
        starting_cash: Decimal,
        monthly_inflows: List[Decimal],
        monthly_outflows: List[Decimal]
    ) -> pd.DataFrame:
        """Project cash flow over time"""

        periods = len(monthly_inflows)
        cash_balance = [float(starting_cash)]

        for i in range(periods):
            inflow = float(monthly_inflows[i])
            outflow = float(monthly_outflows[i])
            new_balance = cash_balance[-1] + inflow - outflow
            cash_balance.append(new_balance)

        df = pd.DataFrame({
            'period': range(periods + 1),
            'inflows': [0] + [float(x) for x in monthly_inflows],
            'outflows': [0] + [float(x) for x in monthly_outflows],
            'cash_balance': cash_balance
        })

        return df

    def aging_analysis(
        self,
        invoices: List[Dict],
        as_of_date: date
    ) -> pd.DataFrame:
        """Analyze accounts receivable aging"""

        aging_data = []

        for invoice in invoices:
            due_date = invoice['due_date']
            amount = invoice['amount_due']

            if isinstance(due_date, str):
                due_date = datetime.strptime(due_date, '%Y-%m-%d').date()

            days_past_due = (as_of_date - due_date).days

            if days_past_due <= 0:
                bucket = 'Current'
            elif days_past_due <= 30:
                bucket = '1-30 Days'
            elif days_past_due <= 60:
                bucket = '31-60 Days'
            elif days_past_due <= 90:
                bucket = '61-90 Days'
            else:
                bucket = 'Over 90 Days'

            aging_data.append({
                'invoice_number': invoice['invoice_number'],
                'customer': invoice['customer'],
                'amount_due': amount,
                'days_past_due': days_past_due,
                'aging_bucket': bucket
            })

        df = pd.DataFrame(aging_data)

        # Add aging summary
        summary = df.groupby('aging_bucket')['amount_due'].sum().to_dict()

        return df, summary

    def break_even_analysis(
        self,
        fixed_costs: Decimal,
        variable_cost_per_unit: Decimal,
        price_per_unit: Decimal
    ) -> Dict:
        """Calculate break-even point"""

        contribution_margin = float(price_per_unit - variable_cost_per_unit)

        if contribution_margin <= 0:
            return {
                'break_even_units': None,
                'break_even_revenue': None,
                'error': 'Contribution margin must be positive'
            }

        break_even_units = float(fixed_costs) / contribution_margin
        break_even_revenue = break_even_units * float(price_per_unit)

        return {
            'break_even_units': break_even_units,
            'break_even_revenue': break_even_revenue,
            'contribution_margin': contribution_margin,
            'contribution_margin_ratio': contribution_margin / float(price_per_unit)
        }

    def depreciation_schedule(
        self,
        asset_cost: Decimal,
        salvage_value: Decimal,
        useful_life_years: int,
        method: str = 'straight_line'
    ) -> pd.DataFrame:
        """Generate depreciation schedule"""

        if method == 'straight_line':
            annual_depreciation = (asset_cost - salvage_value) / useful_life_years

            schedule = []
            accumulated = Decimal(0)

            for year in range(1, useful_life_years + 1):
                accumulated += annual_depreciation
                book_value = asset_cost - accumulated

                schedule.append({
                    'year': year,
                    'depreciation_expense': float(annual_depreciation),
                    'accumulated_depreciation': float(accumulated),
                    'book_value': float(book_value)
                })

        elif method == 'declining_balance':
            # Double declining balance
            rate = 2.0 / useful_life_years
            schedule = []
            accumulated = Decimal(0)
            book_value = asset_cost

            for year in range(1, useful_life_years + 1):
                depreciation = book_value * Decimal(rate)

                # Don't depreciate below salvage value
                if accumulated + depreciation > asset_cost - salvage_value:
                    depreciation = asset_cost - salvage_value - accumulated

                accumulated += depreciation
                book_value -= depreciation

                schedule.append({
                    'year': year,
                    'depreciation_expense': float(depreciation),
                    'accumulated_depreciation': float(accumulated),
                    'book_value': float(book_value)
                })

        return pd.DataFrame(schedule)


# ----------------------------------------------------------------------------
# Tests
# ----------------------------------------------------------------------------

def test_financial_ratios():
    """Test financial ratio calculations"""
    analytics = FinancialAnalytics()

    ratios = analytics.calculate_financial_ratios(
        total_assets=Decimal('500000'),
        total_liabilities=Decimal('200000'),
        current_assets=Decimal('150000'),
        current_liabilities=Decimal('50000'),
        revenue=Decimal('400000'),
        net_income=Decimal('50000'),
        cost_of_goods_sold=Decimal('200000'),
        inventory=Decimal('40000')
    )

    assert len(ratios) == 5
    assert any(r.name == "Current Ratio" and r.value == 3.0 for r in ratios)
    print("✓ Financial ratios calculated correctly")


def test_variance_analysis():
    """Test budget variance analysis"""
    analytics = FinancialAnalytics()

    budget = {
        'Salaries': Decimal('100000'),
        'Rent': Decimal('24000'),
        'Utilities': Decimal('6000')
    }

    actual = {
        'Salaries': Decimal('105000'),
        'Rent': Decimal('24000'),
        'Utilities': Decimal('5500')
    }

    variances = analytics.variance_analysis(budget, actual)

    assert len(variances) == 3
    assert variances[0].category == 'Salaries'
    assert variances[0].variance == Decimal('5000')
    print("✓ Variance analysis working correctly")


def test_break_even():
    """Test break-even analysis"""
    analytics = FinancialAnalytics()

    result = analytics.break_even_analysis(
        fixed_costs=Decimal('50000'),
        variable_cost_per_unit=Decimal('20'),
        price_per_unit=Decimal('50')
    )

    assert result['break_even_units'] > 0
    assert result['contribution_margin'] == 30.0
    print("✓ Break-even analysis working correctly")


if __name__ == '__main__':
    test_financial_ratios()
    test_variance_analysis()
    test_break_even()
    print("\n✅ All tests passed!")
