/// Risk Management Core Implementation
use std::collections::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Portfolio {
    pub id: String,
    pub positions: Vec<Position>,
    pub market_value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub instrument: String,
    pub quantity: f64,
    pub market_value: f64,
}

pub struct VaRCalculator;

impl VaRCalculator {
    pub fn historical_var(returns: &[f64], confidence_level: f64) -> f64 {
        let mut sorted = returns.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).expect("NaN in comparison"));
        let idx = ((1.0 - confidence_level) * sorted.len() as f64) as usize;
        -sorted[idx]
    }

    pub fn parametric_var(mean: f64, std_dev: f64, confidence_level: f64) -> f64 {
        let z_score = match confidence_level {
            c if c >= 0.99 => 2.33,
            c if c >= 0.95 => 1.65,
            _ => 1.28,
        };
        -(mean - z_score * std_dev)
    }

    pub fn monte_carlo_var(portfolio: &Portfolio, scenarios: usize, confidence_level: f64) -> f64 {
        let mut simulated_returns = Vec::new();
        for _ in 0..scenarios {
            simulated_returns.push(Self::simulate_return(portfolio));
        }
        Self::historical_var(&simulated_returns, confidence_level)
    }

    fn simulate_return(_portfolio: &Portfolio) -> f64 {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        rng.gen_range(-0.1..0.1)
    }
}

pub struct GreeksCalculator;

impl GreeksCalculator {
    pub fn delta(spot: f64, strike: f64, vol: f64, time: f64, rate: f64) -> f64 {
        let d1 = ((spot / strike).ln() + (rate + 0.5 * vol.powi(2)) * time) / (vol * time.sqrt());
        Self::normal_cdf(d1)
    }

    pub fn gamma(spot: f64, strike: f64, vol: f64, time: f64, rate: f64) -> f64 {
        let d1 = ((spot / strike).ln() + (rate + 0.5 * vol.powi(2)) * time) / (vol * time.sqrt());
        Self::normal_pdf(d1) / (spot * vol * time.sqrt())
    }

    fn normal_cdf(x: f64) -> f64 {
        0.5 * (1.0 + libm::erf(x / std::f64::consts::SQRT_2))
    }

    fn normal_pdf(x: f64) -> f64 {
        (-0.5 * x.powi(2)).exp() / (2.0 * std::f64::consts::PI).sqrt()
    }
}
