//! # Suggestion Engine - Pain Point Mining
//!
//! Analyzes developer feedback, usage patterns, and error logs to identify
//! improvement opportunities and generate actionable suggestions.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::{debug, info};

use crate::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PainPoint {
    pub id: String,
    pub category: PainPointCategory,
    pub description: String,
    pub frequency: usize,
    pub severity: Severity,
    pub first_observed: DateTime<Utc>,
    pub last_observed: DateTime<Utc>,
    pub affected_components: Vec<String>,
    pub evidence: Vec<Evidence>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PainPointCategory {
    /// Performance issues
    Performance,
    /// Usability problems
    Usability,
    /// Documentation gaps
    Documentation,
    /// Error-prone patterns
    ErrorProne,
    /// Unclear semantics
    SemanticClarity,
    /// Namespace confusion
    NamespaceClutter,
    /// Template limitations
    TemplateLimitation,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Low = 1,
    Medium = 2,
    High = 3,
    Critical = 4,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub source: EvidenceSource,
    pub description: String,
    pub timestamp: DateTime<Utc>,
    pub metadata: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EvidenceSource {
    /// User feedback from telemetry
    UserFeedback,
    /// Error logs
    ErrorLog,
    /// Performance metrics
    PerformanceMetrics,
    /// Usage patterns
    UsagePattern,
    /// Code analysis
    CodeAnalysis,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Suggestion {
    pub id: String,
    pub priority: Priority,
    pub title: String,
    pub description: String,
    pub rationale: String,
    pub estimated_impact: ImpactEstimate,
    pub implementation_steps: Vec<String>,
    pub related_pain_points: Vec<String>,
    pub created_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    Low = 1,
    Medium = 2,
    High = 3,
    Critical = 4,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactEstimate {
    /// Expected improvement in quality (0.0 - 1.0)
    pub quality_improvement: f64,
    /// Expected reduction in errors (0.0 - 1.0)
    pub error_reduction: f64,
    /// Expected performance gain (0.0 - 1.0)
    pub performance_gain: f64,
    /// Expected improvement in developer experience (0.0 - 1.0)
    pub dx_improvement: f64,
    /// Estimated effort (person-hours)
    pub effort_hours: f64,
}

impl ImpactEstimate {
    /// Calculate overall impact score (0.0 - 1.0)
    pub fn overall_score(&self) -> f64 {
        (self.quality_improvement * 0.3
            + self.error_reduction * 0.25
            + self.performance_gain * 0.25
            + self.dx_improvement * 0.2)
            .min(1.0)
    }

    /// Calculate ROI (impact / effort)
    pub fn roi(&self) -> f64 {
        if self.effort_hours > 0.0 {
            self.overall_score() / self.effort_hours
        } else {
            0.0
        }
    }
}

pub struct SuggestionEngine {
    pain_points: HashMap<String, PainPoint>,
    suggestions: HashMap<String, Suggestion>,
    thresholds: SuggestionThresholds,
}

#[derive(Debug, Clone)]
pub struct SuggestionThresholds {
    /// Minimum frequency to consider a pain point
    pub min_frequency: usize,
    /// Minimum severity to generate suggestions
    pub min_severity: Severity,
    /// Minimum ROI to prioritize suggestion
    pub min_roi: f64,
}

impl Default for SuggestionThresholds {
    fn default() -> Self {
        Self {
            min_frequency: 3,
            min_severity: Severity::Medium,
            min_roi: 0.1,
        }
    }
}

impl SuggestionEngine {
    pub fn new(thresholds: SuggestionThresholds) -> Self {
        Self {
            pain_points: HashMap::new(),
            suggestions: HashMap::new(),
            thresholds,
        }
    }

    /// Record a new pain point or update existing one
    pub fn record_pain_point(&mut self, mut pain_point: PainPoint) {
        if let Some(existing) = self.pain_points.get_mut(&pain_point.id) {
            // Update existing pain point
            existing.frequency += pain_point.frequency;
            existing.last_observed = pain_point.last_observed;
            existing.evidence.append(&mut pain_point.evidence);
            existing.affected_components.extend(pain_point.affected_components);
            existing.affected_components.sort();
            existing.affected_components.dedup();

            debug!(
                pain_point_id = %existing.id,
                frequency = existing.frequency,
                "Updated existing pain point"
            );
        } else {
            info!(
                pain_point_id = %pain_point.id,
                category = ?pain_point.category,
                severity = ?pain_point.severity,
                "Recorded new pain point"
            );
            self.pain_points.insert(pain_point.id.clone(), pain_point);
        }
    }

    /// Analyze pain points and generate suggestions
    pub fn generate_suggestions(&mut self) -> Result<Vec<Suggestion>> {
        let mut new_suggestions = Vec::new();

        for pain_point in self.pain_points.values() {
            // Skip if below thresholds
            if pain_point.frequency < self.thresholds.min_frequency
                || pain_point.severity < self.thresholds.min_severity
            {
                continue;
            }

            // Generate suggestion based on pain point category
            if let Some(suggestion) = self.create_suggestion_for_pain_point(pain_point) {
                // Check ROI threshold
                if suggestion.estimated_impact.roi() >= self.thresholds.min_roi {
                    info!(
                        suggestion_id = %suggestion.id,
                        priority = ?suggestion.priority,
                        roi = suggestion.estimated_impact.roi(),
                        "Generated new suggestion"
                    );
                    new_suggestions.push(suggestion.clone());
                    self.suggestions.insert(suggestion.id.clone(), suggestion);
                }
            }
        }

        Ok(new_suggestions)
    }

    fn create_suggestion_for_pain_point(&self, pain_point: &PainPoint) -> Option<Suggestion> {
        let (title, description, steps, impact) = match &pain_point.category {
            PainPointCategory::Performance => (
                format!("Optimize performance for {}", pain_point.description),
                "Improve query performance and reduce latency".to_string(),
                vec![
                    "Profile current performance".to_string(),
                    "Identify bottlenecks".to_string(),
                    "Optimize SPARQL queries".to_string(),
                    "Add appropriate indexes".to_string(),
                    "Validate improvements".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.3,
                    error_reduction: 0.1,
                    performance_gain: 0.8,
                    dx_improvement: 0.5,
                    effort_hours: 8.0,
                },
            ),

            PainPointCategory::SemanticClarity => (
                format!("Clarify semantics: {}", pain_point.description),
                "Improve ontology clarity and reduce ambiguity".to_string(),
                vec![
                    "Analyze current semantic structure".to_string(),
                    "Identify ambiguous terms".to_string(),
                    "Add clarifying annotations".to_string(),
                    "Update documentation".to_string(),
                    "Add usage examples".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.7,
                    error_reduction: 0.4,
                    performance_gain: 0.0,
                    dx_improvement: 0.8,
                    effort_hours: 6.0,
                },
            ),

            PainPointCategory::NamespaceClutter => (
                format!("Reorganize namespace: {}", pain_point.description),
                "Apply 5S methodology to organize RDF namespaces".to_string(),
                vec![
                    "Sort: Categorize namespace elements".to_string(),
                    "Set in order: Create logical hierarchy".to_string(),
                    "Shine: Remove deprecated elements".to_string(),
                    "Standardize: Apply naming conventions".to_string(),
                    "Sustain: Document organization rules".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.6,
                    error_reduction: 0.3,
                    performance_gain: 0.2,
                    dx_improvement: 0.7,
                    effort_hours: 10.0,
                },
            ),

            PainPointCategory::Documentation => (
                format!("Document: {}", pain_point.description),
                "Generate comprehensive documentation from best practices".to_string(),
                vec![
                    "Extract best practices from code".to_string(),
                    "Generate standard work documentation".to_string(),
                    "Add usage examples".to_string(),
                    "Create decision guides".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.5,
                    error_reduction: 0.3,
                    performance_gain: 0.0,
                    dx_improvement: 0.9,
                    effort_hours: 4.0,
                },
            ),

            PainPointCategory::ErrorProne => (
                format!("Reduce errors in: {}", pain_point.description),
                "Add validation and safeguards to prevent common errors".to_string(),
                vec![
                    "Analyze error patterns".to_string(),
                    "Add SHACL validation rules".to_string(),
                    "Implement guardrails".to_string(),
                    "Add error recovery logic".to_string(),
                    "Update error messages".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.7,
                    error_reduction: 0.9,
                    performance_gain: 0.0,
                    dx_improvement: 0.6,
                    effort_hours: 12.0,
                },
            ),

            PainPointCategory::TemplateLimitation => (
                format!("Enhance template: {}", pain_point.description),
                "Extend template capabilities to support additional use cases".to_string(),
                vec![
                    "Analyze template usage patterns".to_string(),
                    "Design template extensions".to_string(),
                    "Implement new features".to_string(),
                    "Add examples".to_string(),
                    "Update documentation".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.6,
                    error_reduction: 0.2,
                    performance_gain: 0.1,
                    dx_improvement: 0.8,
                    effort_hours: 8.0,
                },
            ),

            PainPointCategory::Usability => (
                format!("Improve usability: {}", pain_point.description),
                "Enhance developer experience and ease of use".to_string(),
                vec![
                    "Analyze usability issues".to_string(),
                    "Design improved interface".to_string(),
                    "Implement enhancements".to_string(),
                    "Add helpful error messages".to_string(),
                    "Create usage guides".to_string(),
                ],
                ImpactEstimate {
                    quality_improvement: 0.4,
                    error_reduction: 0.3,
                    performance_gain: 0.0,
                    dx_improvement: 0.9,
                    effort_hours: 6.0,
                },
            ),
        };

        let priority = match pain_point.severity {
            Severity::Critical => Priority::Critical,
            Severity::High => Priority::High,
            Severity::Medium => Priority::Medium,
            Severity::Low => Priority::Low,
        };

        Some(Suggestion {
            id: uuid::Uuid::new_v4().to_string(),
            priority,
            title,
            description,
            rationale: format!(
                "Observed {} times across {} components. Severity: {:?}",
                pain_point.frequency,
                pain_point.affected_components.len(),
                pain_point.severity
            ),
            estimated_impact: impact,
            implementation_steps: steps,
            related_pain_points: vec![pain_point.id.clone()],
            created_at: Utc::now(),
        })
    }

    /// Get all suggestions sorted by priority and ROI
    pub fn get_prioritized_suggestions(&self) -> Vec<&Suggestion> {
        let mut suggestions: Vec<&Suggestion> = self.suggestions.values().collect();
        suggestions.sort_by(|a, b| {
            b.priority
                .cmp(&a.priority)
                .then_with(|| {
                    b.estimated_impact
                        .roi()
                        .partial_cmp(&a.estimated_impact.roi())
                        .unwrap_or(std::cmp::Ordering::Equal)
                })
        });
        suggestions
    }

    /// Get pain points by category
    pub fn get_pain_points_by_category(
        &self,
        category: PainPointCategory,
    ) -> Vec<&PainPoint> {
        self.pain_points
            .values()
            .filter(|pp| pp.category == category)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_impact_estimate_overall_score() {
        let impact = ImpactEstimate {
            quality_improvement: 0.8,
            error_reduction: 0.6,
            performance_gain: 0.4,
            dx_improvement: 0.9,
            effort_hours: 10.0,
        };

        let score = impact.overall_score();
        assert!(score > 0.0 && score <= 1.0);
    }

    #[test]
    fn test_impact_estimate_roi() {
        let impact = ImpactEstimate {
            quality_improvement: 0.8,
            error_reduction: 0.6,
            performance_gain: 0.4,
            dx_improvement: 0.9,
            effort_hours: 10.0,
        };

        let roi = impact.roi();
        assert!(roi > 0.0);
    }

    #[test]
    fn test_suggestion_engine_record_pain_point() {
        let mut engine = SuggestionEngine::new(SuggestionThresholds::default());

        let pain_point = PainPoint {
            id: "pp-1".to_string(),
            category: PainPointCategory::Performance,
            description: "Slow SPARQL query".to_string(),
            frequency: 5,
            severity: Severity::High,
            first_observed: Utc::now(),
            last_observed: Utc::now(),
            affected_components: vec!["query-engine".to_string()],
            evidence: vec![],
        };

        engine.record_pain_point(pain_point.clone());
        assert_eq!(engine.pain_points.len(), 1);

        // Record same pain point again
        engine.record_pain_point(pain_point);
        assert_eq!(engine.pain_points.len(), 1);
        assert_eq!(engine.pain_points.get("pp-1").unwrap().frequency, 10);
    }

    #[test]
    fn test_suggestion_generation() {
        let mut engine = SuggestionEngine::new(SuggestionThresholds {
            min_frequency: 1,
            min_severity: Severity::Low,
            min_roi: 0.0,
        });

        let pain_point = PainPoint {
            id: "pp-1".to_string(),
            category: PainPointCategory::SemanticClarity,
            description: "Ambiguous property definitions".to_string(),
            frequency: 10,
            severity: Severity::High,
            first_observed: Utc::now(),
            last_observed: Utc::now(),
            affected_components: vec!["ontology".to_string()],
            evidence: vec![],
        };

        engine.record_pain_point(pain_point);
        let suggestions = engine.generate_suggestions().unwrap();

        assert!(!suggestions.is_empty());
        assert_eq!(suggestions[0].priority, Priority::High);
    }
}
