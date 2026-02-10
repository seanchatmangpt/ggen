use crate::{QualityMetrics, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct QualityScore {
    pub defect_rate: f64,
    pub incomplete_packets_rate: f64,
    pub churn_rate: f64,
    pub urgency_inflation_rate: f64,
    pub coordination_dumping_rate: f64,
    pub is_high_quality: bool,
}

impl QualityScore {
    const HIGH_QUALITY_THRESHOLD: f64 = 20.0;

    pub fn calculate(metrics: &QualityMetrics) -> Result<Self> {
        metrics.validate()?;

        if metrics.total_deliveries == 0 {
            return Ok(Self::zero());
        }

        let total = metrics.total_deliveries as f64;
        let defect_rate = (metrics.total_defects() as f64 / total) * 100.0;
        let incomplete_packets_rate = (metrics.incomplete_packets as f64 / total) * 100.0;
        let churn_rate = (metrics.churn as f64 / total) * 100.0;
        let urgency_inflation_rate = (metrics.urgency_inflation as f64 / total) * 100.0;
        let coordination_dumping_rate = (metrics.coordination_dumping as f64 / total) * 100.0;

        Ok(Self {
            defect_rate,
            incomplete_packets_rate,
            churn_rate,
            urgency_inflation_rate,
            coordination_dumping_rate,
            is_high_quality: defect_rate <= Self::HIGH_QUALITY_THRESHOLD,
        })
    }

    pub fn zero() -> Self {
        Self {
            defect_rate: 0.0,
            incomplete_packets_rate: 0.0,
            churn_rate: 0.0,
            urgency_inflation_rate: 0.0,
            coordination_dumping_rate: 0.0,
            is_high_quality: true,
        }
    }

    pub fn should_rate_limit(&self) -> bool {
        !self.is_high_quality
    }
}

pub struct QualityScorer;

impl QualityScorer {
    pub fn score(metrics: &QualityMetrics) -> Result<QualityScore> {
        QualityScore::calculate(metrics)
    }

    pub fn assess_supplier(metrics: &QualityMetrics) -> Result<SupplierAssessment> {
        let score = Self::score(metrics)?;

        let risk_level = if score.defect_rate == 0.0 {
            RiskLevel::Excellent
        } else if score.defect_rate <= 5.0 {
            RiskLevel::Low
        } else if score.defect_rate <= 10.0 {
            RiskLevel::Medium
        } else if score.defect_rate <= 20.0 {
            RiskLevel::High
        } else {
            RiskLevel::Critical
        };

        let recommendations = Self::generate_recommendations(&score);

        Ok(SupplierAssessment {
            score,
            risk_level,
            recommendations,
        })
    }

    fn generate_recommendations(score: &QualityScore) -> Vec<String> {
        let mut recommendations = Vec::new();

        if score.incomplete_packets_rate > 5.0 {
            recommendations.push(format!(
                "High incomplete packet rate ({:.2}%). Review delivery completeness.",
                score.incomplete_packets_rate
            ));
        }

        if score.churn_rate > 5.0 {
            recommendations.push(format!(
                "High churn rate ({:.2}%). Investigate work stability.",
                score.churn_rate
            ));
        }

        if score.urgency_inflation_rate > 5.0 {
            recommendations.push(format!(
                "High urgency inflation rate ({:.2}%). Review priority management.",
                score.urgency_inflation_rate
            ));
        }

        if score.coordination_dumping_rate > 5.0 {
            recommendations.push(format!(
                "High coordination dumping rate ({:.2}%). Improve coordination practices.",
                score.coordination_dumping_rate
            ));
        }

        if score.should_rate_limit() {
            recommendations.push(format!(
                "CRITICAL: Defect rate ({:.2}%) exceeds 20% threshold. Rate limiting recommended.",
                score.defect_rate
            ));
        }

        recommendations
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RiskLevel {
    Excellent,
    Low,
    Medium,
    High,
    Critical,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplierAssessment {
    pub score: QualityScore,
    pub risk_level: RiskLevel,
    pub recommendations: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quality_score_zero() {
        let score = QualityScore::zero();
        assert_eq!(score.defect_rate, 0.0);
        assert!(score.is_high_quality);
        assert!(!score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_no_defects() {
        let metrics = QualityMetrics::new(0, 0, 0, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 0.0);
        assert!(score.is_high_quality);
        assert!(!score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_low_defects() {
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 10.0);
        assert_eq!(score.incomplete_packets_rate, 5.0);
        assert_eq!(score.churn_rate, 3.0);
        assert_eq!(score.urgency_inflation_rate, 2.0);
        assert!(score.is_high_quality);
        assert!(!score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_high_defects() {
        let metrics = QualityMetrics::new(15, 10, 5, 5, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 35.0);
        assert!(!score.is_high_quality);
        assert!(score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_threshold() {
        let metrics = QualityMetrics::new(20, 0, 0, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 20.0);
        assert!(score.is_high_quality);
        assert!(!score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_above_threshold() {
        let metrics = QualityMetrics::new(21, 0, 0, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 21.0);
        assert!(!score.is_high_quality);
        assert!(score.should_rate_limit());
    }

    #[test]
    fn test_quality_score_calculate_zero_deliveries() {
        let metrics = QualityMetrics::default();
        let score = QualityScore::calculate(&metrics).unwrap();

        assert_eq!(score.defect_rate, 0.0);
        assert!(score.is_high_quality);
    }

    #[test]
    fn test_quality_scorer_score() {
        let metrics = QualityMetrics::new(10, 5, 3, 2, 100).unwrap();
        let score = QualityScorer::score(&metrics).unwrap();

        assert_eq!(score.defect_rate, 20.0);
        assert!(score.is_high_quality);
    }

    #[test]
    fn test_quality_scorer_assess_excellent() {
        let metrics = QualityMetrics::new(0, 0, 0, 0, 100).unwrap();
        let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

        assert_eq!(assessment.risk_level, RiskLevel::Excellent);
        assert!(assessment.recommendations.is_empty());
    }

    #[test]
    fn test_quality_scorer_assess_low() {
        let metrics = QualityMetrics::new(3, 1, 1, 0, 100).unwrap();
        let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

        assert_eq!(assessment.risk_level, RiskLevel::Low);
    }

    #[test]
    fn test_quality_scorer_assess_medium() {
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

        assert_eq!(assessment.risk_level, RiskLevel::Medium);
    }

    #[test]
    fn test_quality_scorer_assess_high() {
        let metrics = QualityMetrics::new(10, 5, 3, 2, 100).unwrap();
        let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

        assert_eq!(assessment.risk_level, RiskLevel::High);
        assert!(!assessment.recommendations.is_empty());
    }

    #[test]
    fn test_quality_scorer_assess_critical() {
        let metrics = QualityMetrics::new(15, 10, 5, 5, 100).unwrap();
        let assessment = QualityScorer::assess_supplier(&metrics).unwrap();

        assert_eq!(assessment.risk_level, RiskLevel::Critical);
        assert!(assessment.recommendations.iter().any(|r| r.contains("CRITICAL")));
    }

    #[test]
    fn test_generate_recommendations_incomplete_packets() {
        let metrics = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();
        let recommendations = QualityScorer::generate_recommendations(&score);

        assert!(recommendations.iter().any(|r| r.contains("incomplete packet")));
    }

    #[test]
    fn test_generate_recommendations_churn() {
        let metrics = QualityMetrics::new(0, 10, 0, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();
        let recommendations = QualityScorer::generate_recommendations(&score);

        assert!(recommendations.iter().any(|r| r.contains("churn")));
    }

    #[test]
    fn test_generate_recommendations_urgency_inflation() {
        let metrics = QualityMetrics::new(0, 0, 10, 0, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();
        let recommendations = QualityScorer::generate_recommendations(&score);

        assert!(recommendations.iter().any(|r| r.contains("urgency inflation")));
    }

    #[test]
    fn test_generate_recommendations_coordination_dumping() {
        let metrics = QualityMetrics::new(0, 0, 0, 10, 100).unwrap();
        let score = QualityScore::calculate(&metrics).unwrap();
        let recommendations = QualityScorer::generate_recommendations(&score);

        assert!(recommendations.iter().any(|r| r.contains("coordination dumping")));
    }
}
