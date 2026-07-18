//! Health Score Metrics
//!
//! Compute and track domain health scores

use serde::{Deserialize, Serialize};

/// Health score calculation
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct HealthScore {
    pub score: f64,
    pub is_balanced: bool,
    pub trend: ScoreTrend,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ScoreTrend {
    Improving,
    Declining,
    Stable,
}

impl HealthScore {
    /// Create health score from value
    pub fn from_value(score: f64) -> Self {
        let is_balanced = score >= 0.65;
        let trend = if score > 0.7 {
            ScoreTrend::Improving
        } else if score < 0.5 {
            ScoreTrend::Declining
        } else {
            ScoreTrend::Stable
        };
        
        Self {
            score: score.max(0.0).min(1.0),
            is_balanced,
            trend,
        }
    }

    /// Calculate composite health score from multiple metrics
    pub fn from_metrics(metrics: &[f64]) -> Self {
        if metrics.is_empty() {
            return Self::from_value(0.0);
        }
        
        let total: f64 = metrics.iter().sum();
        let average = total / metrics.len() as f64;
        Self::from_value(average)
    }

    /// Check if domain needs urgent attention
    pub fn needs_urgent_attention(&self) -> bool {
        self.score < 0.5
    }

    /// Get recommendation based on score
    pub fn get_recommendation(&self) -> String {
        match self.score {
            s if s >= 0.9 => "Excellent! Maintain current practices.".to_string(),
            s if s >= 0.75 => "Good progress. Consider optimization.".to_string(),
            s if s >= 0.6 => "Acceptable. Plan for improvement.".to_string(),
            s if s >= 0.4 => "Needs attention. Develop action plan.".to_string(),
            _ => "Urgent action required!".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_health_score_high() {
        let score = HealthScore::from_value(0.85);
        assert!(score.is_balanced);
        assert!(matches!(score.trend, ScoreTrend::Improving));
    }

    #[test]
    fn test_health_score_low() {
        let score = HealthScore::from_value(0.4);
        assert!(!score.is_balanced);
        assert!(score.needs_urgent_attention());
    }

    #[test]
    fn test_from_metrics() {
        let metrics = vec![0.6, 0.8, 0.7];
        let score = HealthScore::from_metrics(&metrics);
        let expected = (0.6 + 0.8 + 0.7) / 3.0;
        assert!((score.score - expected).abs() < 0.01);
    }
}
