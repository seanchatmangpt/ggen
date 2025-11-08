// CRM Backend - Rust implementation for customer relationship tracking
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, NaiveDate};
use rust_decimal::Decimal;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OpportunityStage {
    Prospecting,
    Qualification,
    NeedsAnalysis,
    Proposal,
    Negotiation,
    ClosedWon,
    ClosedLost,
}

impl OpportunityStage {
    pub fn probability(&self) -> u8 {
        match self {
            Self::Prospecting => 10,
            Self::Qualification => 25,
            Self::NeedsAnalysis => 40,
            Self::Proposal => 60,
            Self::Negotiation => 80,
            Self::ClosedWon => 100,
            Self::ClosedLost => 0,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Opportunity {
    pub id: String,
    pub name: String,
    pub account_id: String,
    pub amount: Decimal,
    pub stage: OpportunityStage,
    pub close_date: NaiveDate,
    pub owner_id: String,
    pub created_at: DateTime<Utc>,
}

impl Opportunity {
    pub fn expected_revenue(&self) -> Decimal {
        self.amount * Decimal::from(self.stage.probability()) / Decimal::from(100)
    }

    pub fn is_closed(&self) -> bool {
        matches!(self.stage, OpportunityStage::ClosedWon | OpportunityStage::ClosedLost)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pipeline {
    pub opportunities: Vec<Opportunity>,
}

impl Pipeline {
    pub fn new() -> Self {
        Self { opportunities: Vec::new() }
    }

    pub fn add(&mut self, opp: Opportunity) {
        self.opportunities.push(opp);
    }

    pub fn total_value(&self) -> Decimal {
        self.opportunities.iter().map(|o| o.amount).sum()
    }

    pub fn weighted_value(&self) -> Decimal {
        self.opportunities.iter().map(|o| o.expected_revenue()).sum()
    }

    pub fn win_rate(&self) -> f64 {
        let closed: Vec<_> = self.opportunities.iter().filter(|o| o.is_closed()).collect();
        if closed.is_empty() { return 0.0; }
        let won = closed.iter().filter(|o| matches!(o.stage, OpportunityStage::ClosedWon)).count();
        (won as f64 / closed.len() as f64) * 100.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expected_revenue() {
        let opp = Opportunity {
            id: "1".to_string(),
            name: "Test".to_string(),
            account_id: "A1".to_string(),
            amount: Decimal::from(10000),
            stage: OpportunityStage::Proposal,
            close_date: NaiveDate::from_ymd_opt(2025, 3, 1).unwrap(),
            owner_id: "U1".to_string(),
            created_at: Utc::now(),
        };

        assert_eq!(opp.expected_revenue(), Decimal::from(6000)); // 60% of 10000
    }

    #[test]
    fn test_pipeline_metrics() {
        let mut pipeline = Pipeline::new();

        pipeline.add(Opportunity {
            id: "1".to_string(),
            name: "Deal 1".to_string(),
            account_id: "A1".to_string(),
            amount: Decimal::from(10000),
            stage: OpportunityStage::ClosedWon,
            close_date: NaiveDate::from_ymd_opt(2025, 2, 1).unwrap(),
            owner_id: "U1".to_string(),
            created_at: Utc::now(),
        });

        pipeline.add(Opportunity {
            id: "2".to_string(),
            name: "Deal 2".to_string(),
            account_id: "A2".to_string(),
            amount: Decimal::from(5000),
            stage: OpportunityStage::ClosedLost,
            close_date: NaiveDate::from_ymd_opt(2025, 2, 15).unwrap(),
            owner_id: "U1".to_string(),
            created_at: Utc::now(),
        });

        assert_eq!(pipeline.win_rate(), 50.0);
        assert_eq!(pipeline.total_value(), Decimal::from(15000));
    }
}
