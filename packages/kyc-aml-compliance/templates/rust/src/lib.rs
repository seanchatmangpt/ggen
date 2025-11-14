// KYC/AML Compliance - Rust Sanctions Screening Engine
// High-performance fuzzy matching and screening

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc, NaiveDate};

// ============================================================
// Core Types
// ============================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Customer {
    pub customer_id: String,
    pub full_name: String,
    pub date_of_birth: Option<NaiveDate>,
    pub nationality: Option<String>,
    pub country_of_residence: String,
    pub customer_type: CustomerType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CustomerType {
    Individual,
    Entity,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SanctionsListEntry {
    pub entry_id: String,
    pub full_name: String,
    pub aliases: Vec<String>,
    pub date_of_birth: Option<NaiveDate>,
    pub nationality: Option<String>,
    pub list_type: SanctionsListType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SanctionsListType {
    OFAC,
    EU,
    UN,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SanctionsMatch {
    pub customer_id: String,
    pub entry_id: String,
    pub match_score: f64,
    pub match_type: MatchType,
    pub list_type: SanctionsListType,
    pub matched_field: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MatchType {
    Exact,
    Fuzzy,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskAssessment {
    pub customer_id: String,
    pub risk_score: u32,
    pub risk_level: RiskLevel,
    pub risk_factors: Vec<RiskFactor>,
    pub assessment_date: DateTime<Utc>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RiskLevel {
    Low,
    Medium,
    High,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskFactor {
    pub factor_type: RiskFactorType,
    pub score: u32,
    pub description: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RiskFactorType {
    Geographic,
    Industry,
    Transaction,
    PEP,
    Sanctions,
}

// ============================================================
// Sanctions Screening Engine
// ============================================================

pub struct SanctionsScreeningEngine {
    sanctions_lists: HashMap<SanctionsListType, Vec<SanctionsListEntry>>,
    exact_match_threshold: f64,
    fuzzy_match_threshold: f64,
}

impl SanctionsScreeningEngine {
    pub fn new() -> Self {
        Self {
            sanctions_lists: HashMap::new(),
            exact_match_threshold: 1.0,
            fuzzy_match_threshold: 0.85,
        }
    }

    pub fn load_sanctions_list(
        &mut self,
        list_type: SanctionsListType,
        entries: Vec<SanctionsListEntry>,
    ) {
        self.sanctions_lists.insert(list_type, entries);
    }

    pub fn screen_customer(&self, customer: &Customer) -> Vec<SanctionsMatch> {
        let mut matches = Vec::new();

        for (list_type, entries) in &self.sanctions_lists {
            for entry in entries {
                // Check exact name match
                if self.exact_match(&customer.full_name, &entry.full_name) {
                    matches.push(SanctionsMatch {
                        customer_id: customer.customer_id.clone(),
                        entry_id: entry.entry_id.clone(),
                        match_score: 1.0,
                        match_type: MatchType::Exact,
                        list_type: *list_type,
                        matched_field: "full_name".to_string(),
                    });
                    continue;
                }

                // Check fuzzy name match
                let score = self.fuzzy_match(&customer.full_name, &entry.full_name);
                if score >= self.fuzzy_match_threshold {
                    matches.push(SanctionsMatch {
                        customer_id: customer.customer_id.clone(),
                        entry_id: entry.entry_id.clone(),
                        match_score: score,
                        match_type: MatchType::Fuzzy,
                        list_type: *list_type,
                        matched_field: "full_name".to_string(),
                    });
                }

                // Check aliases
                for alias in &entry.aliases {
                    let score = self.fuzzy_match(&customer.full_name, alias);
                    if score >= self.fuzzy_match_threshold {
                        matches.push(SanctionsMatch {
                            customer_id: customer.customer_id.clone(),
                            entry_id: entry.entry_id.clone(),
                            match_score: score,
                            match_type: MatchType::Fuzzy,
                            list_type: *list_type,
                            matched_field: "alias".to_string(),
                        });
                    }
                }
            }
        }

        matches
    }

    fn exact_match(&self, name1: &str, name2: &str) -> bool {
        self.normalize_name(name1) == self.normalize_name(name2)
    }

    fn fuzzy_match(&self, name1: &str, name2: &str) -> f64 {
        // Use Jaro-Winkler distance for fuzzy matching
        let normalized1 = self.normalize_name(name1);
        let normalized2 = self.normalize_name(name2);

        strsim::jaro_winkler(&normalized1, &normalized2)
    }

    fn normalize_name(&self, name: &str) -> String {
        name.to_uppercase()
            .chars()
            .filter(|c| c.is_alphanumeric() || c.is_whitespace())
            .collect::<String>()
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
    }
}

// ============================================================
// Risk Scoring Engine
// ============================================================

pub struct RiskScoringEngine {
    high_risk_countries: Vec<String>,
    medium_risk_countries: Vec<String>,
}

impl RiskScoringEngine {
    pub fn new() -> Self {
        Self {
            high_risk_countries: vec![
                "AF".to_string(), "BY".to_string(), "CD".to_string(),
                "CU".to_string(), "IR".to_string(), "IQ".to_string(),
                "LB".to_string(), "LY".to_string(), "MM".to_string(),
                "KP".to_string(), "SO".to_string(), "SD".to_string(),
                "SY".to_string(), "VE".to_string(), "YE".to_string(),
                "ZW".to_string(),
            ],
            medium_risk_countries: vec![
                "AL".to_string(), "BS".to_string(), "BW".to_string(),
                "GH".to_string(), "JM".to_string(), "MU".to_string(),
                "PA".to_string(), "RS".to_string(), "TT".to_string(),
                "VU".to_string(),
            ],
        }
    }

    pub fn assess_risk(&self, customer: &Customer) -> RiskAssessment {
        let mut risk_factors = Vec::new();
        let mut total_score = 0u32;

        // Geographic risk
        let geo_score = if self.high_risk_countries.contains(&customer.country_of_residence) {
            30
        } else if self.medium_risk_countries.contains(&customer.country_of_residence) {
            20
        } else {
            10
        };

        total_score += geo_score;
        risk_factors.push(RiskFactor {
            factor_type: RiskFactorType::Geographic,
            score: geo_score,
            description: format!(
                "Geographic risk from country: {}",
                customer.country_of_residence
            ),
        });

        // Determine risk level
        let risk_level = if total_score >= 50 {
            RiskLevel::High
        } else if total_score >= 25 {
            RiskLevel::Medium
        } else {
            RiskLevel::Low
        };

        RiskAssessment {
            customer_id: customer.customer_id.clone(),
            risk_score: total_score,
            risk_level,
            risk_factors,
            assessment_date: Utc::now(),
        }
    }
}

// ============================================================
// Tests
// ============================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_name_match() {
        let engine = SanctionsScreeningEngine::new();
        assert!(engine.exact_match("John Smith", "JOHN SMITH"));
        assert!(engine.exact_match("John Smith", "john smith"));
        assert!(!engine.exact_match("John Smith", "Jane Smith"));
    }

    #[test]
    fn test_fuzzy_name_match() {
        let engine = SanctionsScreeningEngine::new();
        let score = engine.fuzzy_match("John Smith", "Jon Smith");
        assert!(score > 0.9);
    }

    #[test]
    fn test_sanctions_screening() {
        let mut engine = SanctionsScreeningEngine::new();

        engine.load_sanctions_list(
            SanctionsListType::OFAC,
            vec![SanctionsListEntry {
                entry_id: "OFAC-001".to_string(),
                full_name: "John Doe".to_string(),
                aliases: vec!["Johnny Doe".to_string()],
                date_of_birth: None,
                nationality: None,
                list_type: SanctionsListType::OFAC,
            }],
        );

        let customer = Customer {
            customer_id: "CUST-001".to_string(),
            full_name: "John Doe".to_string(),
            date_of_birth: None,
            nationality: None,
            country_of_residence: "US".to_string(),
            customer_type: CustomerType::Individual,
        };

        let matches = engine.screen_customer(&customer);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].match_type, MatchType::Exact);
    }

    #[test]
    fn test_risk_assessment() {
        let engine = RiskScoringEngine::new();

        let high_risk_customer = Customer {
            customer_id: "CUST-002".to_string(),
            full_name: "Test User".to_string(),
            date_of_birth: None,
            nationality: None,
            country_of_residence: "IR".to_string(),
            customer_type: CustomerType::Individual,
        };

        let assessment = engine.assess_risk(&high_risk_customer);
        assert_eq!(assessment.risk_level, RiskLevel::High);
        assert!(assessment.risk_score >= 30);
    }
}
