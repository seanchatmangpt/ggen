use crate::{Assessment, Dimension, DimensionAssessment, MaturityLevel};
use std::collections::HashMap;

/// Advanced assessment engine with custom scoring rules
pub struct AssessmentEngine;

impl AssessmentEngine {
    /// Perform weighted assessment across dimensions
    pub fn weighted_assessment(
        metrics: &HashMap<Dimension, f32>,
        weights: &HashMap<Dimension, f32>,
    ) -> f32 {
        let total_weight: f32 = weights.values().sum();
        if total_weight == 0.0 {
            return 0.0;
        }

        let weighted_sum: f32 = metrics
            .iter()
            .map(|(dim, value)| {
                let weight = weights.get(dim).copied().unwrap_or(1.0);
                value * weight
            })
            .sum();

        weighted_sum / total_weight
    }

    /// Detect anomalies in assessment patterns
    pub fn detect_anomalies(
        current: &Assessment,
        previous: Option<&Assessment>,
    ) -> Vec<AnomalyDetection> {
        let mut anomalies = Vec::new();

        if let Some(prev) = previous {
            for (&dim, current_dim) in &current.dimensions {
                if let Some(prev_dim) = prev.dimensions.get(&dim) {
                    let delta = (current_dim.score - prev_dim.score).abs();
                    if delta > 2.0 {
                        anomalies.push(AnomalyDetection {
                            dimension: dim,
                            anomaly_type: AnomalyType::LargeSwing,
                            severity: delta / 4.0, // Normalize to 0-1
                            previous_score: prev_dim.score,
                            current_score: current_dim.score,
                        });
                    }
                }
            }
        }

        // Check for missing dimensions
        let all_dims = vec![
            Dimension::CodeQuality,
            Dimension::Performance,
            Dimension::Reliability,
            Dimension::Operations,
            Dimension::Security,
        ];
        for dim in all_dims {
            if !current.dimensions.contains_key(&dim) {
                anomalies.push(AnomalyDetection {
                    dimension: dim,
                    anomaly_type: AnomalyType::MissingDimension,
                    severity: 0.5,
                    previous_score: 0.0,
                    current_score: 0.0,
                });
            }
        }

        anomalies
    }

    /// Compare two assessments
    pub fn compare_assessments(
        assessment1: &Assessment,
        assessment2: &Assessment,
    ) -> AssessmentComparison {
        let mut dimension_changes = HashMap::new();
        let mut improved_count = 0;
        let mut degraded_count = 0;

        for (&dim, a1) in &assessment1.dimensions {
            if let Some(a2) = assessment2.dimensions.get(&dim) {
                let change = a2.score - a1.score;
                if change > 0.0 {
                    improved_count += 1;
                } else if change < 0.0 {
                    degraded_count += 1;
                }
                dimension_changes.insert(dim, change);
            }
        }

        AssessmentComparison {
            score_delta: assessment2.overall_score - assessment1.overall_score,
            improved_dimensions: improved_count,
            degraded_dimensions: degraded_count,
            unchanged_dimensions: assessment1.dimensions.len() - improved_count - degraded_count,
            dimension_changes,
        }
    }

    /// Generate assessment confidence score based on evidence quality
    pub fn assess_confidence(assessment: &Assessment) -> ConfidenceScore {
        let mut evidence_scores = Vec::new();

        for (_, dim_assess) in &assessment.dimensions {
            let evidence_length = dim_assess.evidence.len() as f32;
            let confidence = (evidence_length / 100.0).min(1.0);
            evidence_scores.push(confidence);
        }

        let avg_confidence = if evidence_scores.is_empty() {
            0.0
        } else {
            evidence_scores.iter().sum::<f32>() / evidence_scores.len() as f32
        };

        ConfidenceScore {
            overall_confidence: avg_confidence,
            dimension_count: assessment.dimensions.len(),
            evidence_quality: Self::calculate_evidence_quality(assessment),
        }
    }

    fn calculate_evidence_quality(assessment: &Assessment) -> EvidenceQuality {
        let mut detailed_count = 0;
        let mut vague_count = 0;

        for (_, dim_assess) in &assessment.dimensions {
            if dim_assess.evidence.len() > 50 {
                detailed_count += 1;
            } else if dim_assess.evidence.len() < 10 {
                vague_count += 1;
            }
        }

        if detailed_count >= assessment.dimensions.len() / 2 {
            EvidenceQuality::Detailed
        } else if vague_count > assessment.dimensions.len() / 2 {
            EvidenceQuality::Vague
        } else {
            EvidenceQuality::Adequate
        }
    }
}

/// Anomaly detected in assessment
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnomalyDetection {
    pub dimension: Dimension,
    pub anomaly_type: AnomalyType,
    pub severity: f32,
    pub previous_score: f32,
    pub current_score: f32,
}

/// Types of anomalies
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum AnomalyType {
    LargeSwing,
    MissingDimension,
    OutlierValue,
}

/// Comparison between two assessments
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AssessmentComparison {
    pub score_delta: f32,
    pub improved_dimensions: usize,
    pub degraded_dimensions: usize,
    pub unchanged_dimensions: usize,
    pub dimension_changes: HashMap<Dimension, f32>,
}

/// Confidence in assessment results
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ConfidenceScore {
    pub overall_confidence: f32,
    pub dimension_count: usize,
    pub evidence_quality: EvidenceQuality,
}

/// Quality of evidence provided
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum EvidenceQuality {
    Detailed,
    Adequate,
    Vague,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::DimensionAssessment;

    #[test]
    fn test_weighted_assessment() {
        let mut metrics = HashMap::new();
        metrics.insert(Dimension::CodeQuality, 3.0);
        metrics.insert(Dimension::Performance, 4.0);

        let mut weights = HashMap::new();
        weights.insert(Dimension::CodeQuality, 1.0);
        weights.insert(Dimension::Performance, 1.0);

        let result = AssessmentEngine::weighted_assessment(&metrics, &weights);
        assert_eq!(result, 3.5);
    }

    #[test]
    fn test_weighted_assessment_with_custom_weights() {
        let mut metrics = HashMap::new();
        metrics.insert(Dimension::CodeQuality, 2.0);
        metrics.insert(Dimension::Performance, 4.0);

        let mut weights = HashMap::new();
        weights.insert(Dimension::CodeQuality, 1.0);
        weights.insert(Dimension::Performance, 3.0);

        let result = AssessmentEngine::weighted_assessment(&metrics, &weights);
        assert_eq!(result, 3.5); // (2*1 + 4*3) / 4
    }

    #[test]
    fn test_detect_large_score_swing() {
        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Optimizing,
            "ev".to_string(),
        ));

        let anomalies = AssessmentEngine::detect_anomalies(&a2, Some(&a1));
        assert!(!anomalies.is_empty());
        assert!(anomalies[0].severity > 0.0);
    }

    #[test]
    fn test_detect_missing_dimension() {
        let a1 = Assessment::new("empty".to_string());
        let anomalies = AssessmentEngine::detect_anomalies(&a1, None);
        assert!(!anomalies.is_empty());
        let has_missing = anomalies
            .iter()
            .any(|a| matches!(a.anomaly_type, AnomalyType::MissingDimension));
        assert!(has_missing);
    }

    #[test]
    fn test_compare_assessments_improvement() {
        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "ev".to_string(),
        ));

        let comparison = AssessmentEngine::compare_assessments(&a1, &a2);
        assert!(comparison.score_delta > 0.0);
        assert_eq!(comparison.improved_dimensions, 1);
        assert_eq!(comparison.degraded_dimensions, 0);
    }

    #[test]
    fn test_compare_assessments_degradation() {
        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "ev".to_string(),
        ));

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Repeatable,
            "ev".to_string(),
        ));

        let comparison = AssessmentEngine::compare_assessments(&a1, &a2);
        assert!(comparison.score_delta < 0.0);
        assert_eq!(comparison.degraded_dimensions, 1);
    }

    #[test]
    fn test_assess_confidence_with_detailed_evidence() {
        let mut assessment = Assessment::new("test".to_string());
        let detailed_evidence = "a".repeat(100);
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            detailed_evidence,
        ));

        let confidence = AssessmentEngine::assess_confidence(&assessment);
        assert!(confidence.overall_confidence > 0.8);
        assert_eq!(confidence.evidence_quality, EvidenceQuality::Detailed);
    }

    #[test]
    fn test_assess_confidence_with_vague_evidence() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "short".to_string(),
        ));

        let confidence = AssessmentEngine::assess_confidence(&assessment);
        assert!(confidence.overall_confidence < 1.0);
    }

    #[test]
    fn test_assessment_engine_weighted_zero_weight() {
        let metrics = HashMap::new();
        let weights = HashMap::new();
        let result = AssessmentEngine::weighted_assessment(&metrics, &weights);
        assert_eq!(result, 0.0);
    }
}
