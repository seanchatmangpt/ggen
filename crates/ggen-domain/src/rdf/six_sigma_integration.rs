//! Integration of Six Sigma methodology with RDF validation
//!
//! This module extends the existing RDF validation framework with
//! Six Sigma quality management capabilities.

use crate::rdf::validation::{ValidationReport, Validator};
use crate::six_sigma::{
    DefectTracker, DpmoCalculator, OpportunityType, ProcessCapability, QualityAgent, SpcChart,
    TransformationStage,
};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};

/// Six Sigma enhanced RDF validator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SixSigmaValidator {
    /// Base SHACL validator
    pub base_validator: Validator,
    /// DPMO defect tracker
    pub defect_tracker: DefectTracker,
    /// SPC charts for monitoring
    pub spc_charts: Vec<SpcChart>,
    /// Process capability assessments
    pub capability_history: Vec<ProcessCapability>,
    /// Quality agent for AI-driven analysis
    pub quality_agent: Option<QualityAgent>,
}

/// Six Sigma enhanced validation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SixSigmaValidationReport {
    /// Base validation report
    pub base_report: ValidationReport,
    /// DPMO metrics
    pub dpmo_metrics: DpmoMetrics,
    /// Process capability
    pub capability: Option<ProcessCapability>,
    /// Control chart violations
    pub spc_violations: Vec<String>,
    /// Quality recommendations
    pub recommendations: Vec<QualityRecommendation>,
}

/// DPMO metrics for validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DpmoMetrics {
    pub total_opportunities: u64,
    pub total_defects: u64,
    pub dpmo: f64,
    pub sigma_level: f64,
    pub by_opportunity_type: Vec<(OpportunityType, u64, f64)>,
}

/// Quality recommendation from Six Sigma analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityRecommendation {
    pub priority: RecommendationPriority,
    pub category: String,
    pub description: String,
    pub expected_improvement: Option<f64>,
    pub implementation_effort: ImplementationEffort,
}

/// Priority of recommendation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum RecommendationPriority {
    Low = 1,
    Medium = 2,
    High = 3,
    Critical = 4,
}

/// Implementation effort level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ImplementationEffort {
    Trivial,  // < 1 hour
    Easy,     // 1-4 hours
    Moderate, // 4-16 hours
    Complex,  // 16+ hours
}

impl SixSigmaValidator {
    /// Create new Six Sigma validator
    pub fn new() -> Self {
        Self {
            base_validator: Validator::new(),
            defect_tracker: DefectTracker::with_industry_targets(),
            spc_charts: Vec::new(),
            capability_history: Vec::new(),
            quality_agent: None,
        }
    }

    /// Validate with Six Sigma quality tracking
    pub fn validate(&mut self, turtle: &str, template_id: &str) -> Result<SixSigmaValidationReport> {
        // Perform base validation
        let base_report = self.base_validator.validate_turtle(turtle, template_id)?;

        // Count opportunities by type
        let mut opportunity_counts: std::collections::HashMap<OpportunityType, (u64, u64)> =
            std::collections::HashMap::new();

        // Count syntax opportunities
        let syntax_opps = 1; // One opportunity to have valid syntax
        let syntax_defects = if base_report.errors.iter().any(|e| e.path.contains("parse")) {
            1
        } else {
            0
        };
        opportunity_counts.insert(OpportunityType::Syntax, (syntax_opps, syntax_defects));

        // Count schema opportunities (one per required field)
        let required_fields = vec!["templateName", "templateVersion", "stability"];
        let schema_opps = required_fields.len() as u64;
        let schema_defects = base_report
            .errors
            .iter()
            .filter(|e| required_fields.iter().any(|f| e.path.contains(f)))
            .count() as u64;
        opportunity_counts.insert(OpportunityType::Schema, (schema_opps, schema_defects));

        // Count data quality opportunities
        let dq_opps = base_report.errors.len() as u64 + base_report.warnings.len() as u64;
        let dq_defects = base_report.errors.len() as u64;
        opportunity_counts.insert(OpportunityType::DataQuality, (dq_opps.max(1), dq_defects));

        // Calculate total DPMO
        let total_opportunities: u64 = opportunity_counts.values().map(|(o, _)| o).sum();
        let total_defects: u64 = opportunity_counts.values().map(|(_, d)| d).sum();

        let dpmo = if total_opportunities > 0 {
            (total_defects as f64 / total_opportunities as f64) * 1_000_000.0
        } else {
            0.0
        };

        let sigma_level = DpmoCalculator::dpmo_to_sigma(dpmo);

        // Track defects
        let mut by_opportunity_type = Vec::new();
        for (opp_type, (opps, defects)) in opportunity_counts {
            let type_dpmo = if opps > 0 {
                (defects as f64 / opps as f64) * 1_000_000.0
            } else {
                0.0
            };
            by_opportunity_type.push((opp_type, defects, type_dpmo));

            // Record in tracker
            self.defect_tracker
                .track_defects(
                    template_id.to_string(),
                    TransformationStage::Validation,
                    opp_type,
                    1, // 1 template
                    opps,
                    defects,
                )
                .ok();
        }

        let dpmo_metrics = DpmoMetrics {
            total_opportunities,
            total_defects,
            dpmo,
            sigma_level,
            by_opportunity_type,
        };

        // Generate recommendations
        let recommendations = self.generate_recommendations(&base_report, &dpmo_metrics);

        Ok(SixSigmaValidationReport {
            base_report,
            dpmo_metrics,
            capability: None, // Will be calculated from historical data
            spc_violations: Vec::new(),
            recommendations,
        })
    }

    /// Generate quality recommendations
    fn generate_recommendations(
        &self,
        report: &ValidationReport,
        metrics: &DpmoMetrics,
    ) -> Vec<QualityRecommendation> {
        let mut recommendations = Vec::new();

        // Recommendation based on sigma level
        if metrics.sigma_level < 3.0 {
            recommendations.push(QualityRecommendation {
                priority: RecommendationPriority::Critical,
                category: "Process Improvement".to_string(),
                description: format!(
                    "Current sigma level ({:.2}σ) is below 3σ. Immediate quality improvement needed.",
                    metrics.sigma_level
                ),
                expected_improvement: Some(50.0),
                implementation_effort: ImplementationEffort::Complex,
            });
        } else if metrics.sigma_level < 4.0 {
            recommendations.push(QualityRecommendation {
                priority: RecommendationPriority::High,
                category: "Process Improvement".to_string(),
                description: format!(
                    "Current sigma level ({:.2}σ) is below industry standard (4σ). Quality improvement recommended.",
                    metrics.sigma_level
                ),
                expected_improvement: Some(30.0),
                implementation_effort: ImplementationEffort::Moderate,
            });
        }

        // Recommendations based on specific defects
        if report.errors.iter().any(|e| e.path.contains("templateName")) {
            recommendations.push(QualityRecommendation {
                priority: RecommendationPriority::Critical,
                category: "Required Fields".to_string(),
                description: "Template name is missing or invalid. This is a required field."
                    .to_string(),
                expected_improvement: Some(10.0),
                implementation_effort: ImplementationEffort::Trivial,
            });
        }

        if report
            .warnings
            .iter()
            .any(|w| w.path.contains("templateVersion"))
        {
            recommendations.push(QualityRecommendation {
                priority: RecommendationPriority::Medium,
                category: "Best Practices".to_string(),
                description: "Use semantic versioning (e.g., 1.0.0) for template versions."
                    .to_string(),
                expected_improvement: Some(5.0),
                implementation_effort: ImplementationEffort::Easy,
            });
        }

        if report.info.iter().any(|i| i.path.contains("description")) {
            recommendations.push(QualityRecommendation {
                priority: RecommendationPriority::Low,
                category: "Documentation".to_string(),
                description: "Add template description for better discoverability.".to_string(),
                expected_improvement: Some(2.0),
                implementation_effort: ImplementationEffort::Trivial,
            });
        }

        recommendations
    }

    /// Calculate process capability from historical validations
    pub fn calculate_capability(
        &mut self,
        samples: &[f64],
        spec_limits: crate::six_sigma::capability::SpecificationLimits,
    ) -> Result<ProcessCapability> {
        let capability = ProcessCapability::from_samples(
            "RDF Validation".to_string(),
            samples,
            spec_limits,
        )?;

        self.capability_history.push(capability.clone());

        Ok(capability)
    }

    /// Get overall quality status
    pub fn get_quality_status(&self) -> QualityStatus {
        let status = self.defect_tracker.get_status();
        match status {
            crate::six_sigma::dpmo::QualityStatus::OnTarget => QualityStatus::Excellent,
            crate::six_sigma::dpmo::QualityStatus::Alert => QualityStatus::Good,
            crate::six_sigma::dpmo::QualityStatus::Critical => QualityStatus::NeedsImprovement,
            crate::six_sigma::dpmo::QualityStatus::Unacceptable => QualityStatus::Poor,
        }
    }
}

/// Overall quality status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum QualityStatus {
    Excellent,
    Good,
    NeedsImprovement,
    Poor,
}

impl Default for SixSigmaValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_six_sigma_validation() {
        let mut validator = SixSigmaValidator::new();

        let turtle = r#"
@prefix ggen: <http://ggen.dev/ontology#> .

<http://example.org/test> a ggen:Template ;
  ggen:templateName "Test Template" ;
  ggen:templateVersion "1.0.0" .
"#;

        let result = validator.validate(turtle, "http://example.org/test");

        assert!(result.is_ok());
        let report = result.unwrap();
        assert!(report.dpmo_metrics.dpmo >= 0.0);
        assert!(report.dpmo_metrics.sigma_level >= 0.0);
    }

    #[test]
    fn test_recommendation_generation() {
        let validator = SixSigmaValidator::new();

        let turtle = r#"
@prefix ggen: <http://ggen.dev/ontology#> .

<http://example.org/test> a ggen:Template ;
  ggen:templateName "" .
"#;

        let base_report = validator
            .base_validator
            .validate_turtle(turtle, "http://example.org/test")
            .unwrap();

        let dpmo_metrics = DpmoMetrics {
            total_opportunities: 10,
            total_defects: 5,
            dpmo: 500_000.0,
            sigma_level: 2.0,
            by_opportunity_type: Vec::new(),
        };

        let recommendations = validator.generate_recommendations(&base_report, &dpmo_metrics);

        // Should have recommendations due to low sigma level
        assert!(!recommendations.is_empty());

        // Should have critical priority recommendation for low sigma
        assert!(recommendations
            .iter()
            .any(|r| r.priority == RecommendationPriority::Critical));
    }
}
