# Risk Management Package

## Overview

Comprehensive risk management system implementing Value at Risk (VaR), stress testing, credit risk modeling, and market risk analysis for financial institutions.

## Features

### Value at Risk (VaR)
- **Historical VaR**: Historical simulation method
- **Monte Carlo VaR**: Stochastic simulation with 10,000+ scenarios
- **Parametric VaR**: Variance-covariance approach
- **Conditional VaR (CVaR)**: Expected shortfall calculation
- **Confidence Levels**: 95%, 99%, 99.9%
- **Time Horizons**: 1-day, 10-day, 1-month

### Stress Testing
- **Scenario Analysis**: Historical and hypothetical scenarios
- **Reverse Stress Testing**: Portfolio breaking point identification
- **Sensitivity Analysis**: Greek-based parameter sensitivity
- **Concentration Risk**: Sector and counterparty limits
- **Correlation Breakdown**: Stress correlation matrices

### Credit Risk
- **Probability of Default (PD)**: Through-the-cycle and point-in-time
- **Loss Given Default (LGD)**: Recovery rate modeling
- **Exposure at Default (EAD)**: Current and potential exposure
- **Credit Valuation Adjustment (CVA)**: Counterparty risk pricing
- **Credit Migration**: Rating transition matrices

### Market Risk
- **Greeks Calculation**: Delta, Gamma, Vega, Theta, Rho
- **Duration & Convexity**: Fixed income risk metrics
- **Beta & Correlation**: Systematic risk measures
- **Volatility Analysis**: Implied and historical volatility

## Regulatory Compliance

### Basel III
- **Market Risk Capital**: Standardized and IMA approaches
- **Credit Risk Capital**: IRB and standardized approaches
- **Operational Risk**: Advanced measurement approach

### Dodd-Frank
- **Stress Testing**: CCAR and DFAST requirements
- **Margin Requirements**: Initial and variation margin
- **Reporting**: Swap data repository filing

### MiFID II / FRTB
- **Risk Factor Modeling**: Modellability assessment
- **P&L Attribution**: Backtesting and validation
- **Non-Modellable Risk Factors**: Stressed capital add-on

## API Reference

### VaR Calculation

```typescript
POST /api/v1/risk/var
{
  "portfolioId": "string",
  "method": "historical" | "monte_carlo" | "parametric",
  "confidenceLevel": 0.95 | 0.99 | 0.999,
  "timeHorizon": 1 | 10 | 30
}

Response: {
  "var": 1234567.89,
  "cvar": 1456789.01,
  "timestamp": "2025-11-08T00:00:00Z"
}
```

### Stress Testing

```typescript
POST /api/v1/risk/stress
{
  "portfolioId": "string",
  "scenario": {
    "name": "2008 Financial Crisis",
    "shocks": [
      { "factor": "equity_index", "change": -0.4 },
      { "factor": "credit_spread", "change": 0.02 }
    ]
  }
}

Response: {
  "baseValue": 10000000,
  "stressedValue": 6000000,
  "impact": -4000000,
  "impactPercent": -40.0
}
```

### Credit Risk

```typescript
POST /api/v1/risk/credit
{
  "counterpartyId": "string",
  "exposureAmount": 1000000,
  "tenor": 5
}

Response: {
  "pd": 0.02,
  "lgd": 0.45,
  "ead": 1000000,
  "expectedLoss": 9000,
  "unexpectedLoss": 45000,
  "creditVaR": 54000
}
```

## SPARQL Queries

15 pre-built templates for:
- Portfolio VaR calculation
- Risk factor exposure
- Options Greeks aggregation
- Stress test scenarios
- Credit risk metrics
- Limit breach detection
- Historical VaR backtesting

## Risk Metrics Formulas

### Historical VaR
```
VaR(α) = -X_{(1-α)n}
where X is sorted P&L distribution
```

### Monte Carlo VaR
```
ΔP = Σ(∂P/∂r_i × Δr_i)
where Δr_i ~ N(μ,Σ) or other distribution
```

### Expected Loss (EL)
```
EL = PD × LGD × EAD
```

### Greeks
```
Delta (Δ) = ∂V/∂S
Gamma (Γ) = ∂²V/∂S²
Vega (ν) = ∂V/∂σ
Theta (Θ) = ∂V/∂t
Rho (ρ) = ∂V/∂r
```

## Implementation Examples

### Rust
```rust
use risk_management::*;

let portfolio = Portfolio::load("portfolio-1")?;
let var = portfolio.calculate_var(
    VaRMethod::MonteCarlo,
    0.99,
    10
)?;
println!("10-day 99% VaR: ${:.2}", var);
```

### TypeScript
```typescript
import { Portfolio, VaRCalculator } from '@ggen/risk-management';

const portfolio = await Portfolio.load('portfolio-1');
const var = await VaRCalculator.calculate({
  portfolio,
  method: 'historical',
  confidence: 0.95,
  horizon: 1
});
```

### Python
```python
from risk_management import Portfolio, VaRCalculator

portfolio = Portfolio.load('portfolio-1')
var = VaRCalculator.calculate(
    portfolio,
    method='parametric',
    confidence=0.99,
    horizon=10
)
```

## Testing

Chicago TDD test suite with:
- Unit tests (VaR methods, Greeks)
- Integration tests (end-to-end risk)
- Security tests (data validation)
- Performance tests (large portfolios)

## Best Practices

1. **Daily VaR Backtesting**: Compare VaR to actual P&L
2. **Model Validation**: Independent validation quarterly
3. **Limit Monitoring**: Real-time breach alerts
4. **Stress Testing**: Monthly scenario analysis
5. **Documentation**: Model documentation and change logs

## License

MIT License
