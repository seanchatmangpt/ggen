//! # Semantic Refinement Tracker
//!
//! Tracks incremental semantic improvements to ontologies over time,
//! demonstrating the compound effect of small, continuous refinements.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::info;

use crate::Result;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SemanticRefinement {
    pub id: String,
    pub refinement_type: RefinementType,
    pub description: String,
    pub applied_at: DateTime<Utc>,
    pub applied_by: String,
    pub target: RefinementTarget,
    pub impact: RefinementImpact,
    pub compound_score: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RefinementType {
    /// Add clarifying annotation
    Annotation,
    /// Improve property definition
    PropertyRefinement,
    /// Enhance class hierarchy
    HierarchyImprovement,
    /// Add constraint or validation
    ConstraintAddition,
    /// Improve naming consistency
    NamingConsistency,
    /// Add examples or documentation
    DocumentationEnhancement,
    /// Optimize query pattern
    QueryOptimization,
    /// Remove redundancy
    RedundancyRemoval,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RefinementTarget {
    /// Ontology class
    Class { class_uri: String },
    /// Ontology property
    Property { property_uri: String },
    /// Namespace
    Namespace { namespace_uri: String },
    /// SPARQL query
    Query { query_id: String },
    /// Template
    Template { template_id: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefinementImpact {
    /// Clarity improvement (0.0 - 1.0)
    pub clarity_delta: f64,
    /// Consistency improvement (0.0 - 1.0)
    pub consistency_delta: f64,
    /// Performance improvement (0.0 - 1.0)
    pub performance_delta: f64,
    /// Usability improvement (0.0 - 1.0)
    pub usability_delta: f64,
}

impl RefinementImpact {
    /// Calculate overall impact score
    pub fn overall_impact(&self) -> f64 {
        (self.clarity_delta * 0.3
            + self.consistency_delta * 0.3
            + self.performance_delta * 0.2
            + self.usability_delta * 0.2)
            .min(1.0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompoundImpact {
    pub total_refinements: usize,
    pub cumulative_clarity: f64,
    pub cumulative_consistency: f64,
    pub cumulative_performance: f64,
    pub cumulative_usability: f64,
    pub compound_multiplier: f64,
}

impl CompoundImpact {
    /// Calculate the compound effect of multiple refinements
    pub fn overall_compound_score(&self) -> f64 {
        let base_score = (self.cumulative_clarity * 0.3
            + self.cumulative_consistency * 0.3
            + self.cumulative_performance * 0.2
            + self.cumulative_usability * 0.2)
            .min(1.0);

        // Apply compound multiplier (small improvements compound over time)
        base_score * self.compound_multiplier
    }
}

pub struct RefinementTracker {
    refinements: Vec<SemanticRefinement>,
    target_index: HashMap<String, Vec<usize>>,
    compound_factor: f64,
}

impl RefinementTracker {
    /// Create a new refinement tracker
    ///
    /// # Arguments
    /// * `compound_factor` - Factor by which improvements compound (typically 1.05 = 5% compounding)
    pub fn new(compound_factor: f64) -> Self {
        Self {
            refinements: Vec::new(),
            target_index: HashMap::new(),
            compound_factor,
        }
    }

    /// Track a new semantic refinement
    pub fn track_refinement(&mut self, refinement: SemanticRefinement) -> Result<()> {
        info!(
            refinement_id = %refinement.id,
            refinement_type = ?refinement.refinement_type,
            impact = refinement.impact.overall_impact(),
            "Tracking semantic refinement"
        );

        let index = self.refinements.len();
        let target_key = self.target_key(&refinement.target);

        // Add to target index for efficient lookup
        self.target_index
            .entry(target_key)
            .or_insert_with(Vec::new)
            .push(index);

        self.refinements.push(refinement);

        Ok(())
    }

    /// Get all refinements for a specific target
    pub fn get_refinements_for_target(&self, target: &RefinementTarget) -> Vec<&SemanticRefinement> {
        let target_key = self.target_key(target);

        if let Some(indices) = self.target_index.get(&target_key) {
            indices
                .iter()
                .filter_map(|&idx| self.refinements.get(idx))
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Calculate the compound impact for a specific target
    pub fn calculate_compound_impact(&self, target: &RefinementTarget) -> CompoundImpact {
        let refinements = self.get_refinements_for_target(target);
        let total_refinements = refinements.len();

        if total_refinements == 0 {
            return CompoundImpact {
                total_refinements: 0,
                cumulative_clarity: 0.0,
                cumulative_consistency: 0.0,
                cumulative_performance: 0.0,
                cumulative_usability: 0.0,
                compound_multiplier: 1.0,
            };
        }

        let mut cumulative_clarity = 0.0;
        let mut cumulative_consistency = 0.0;
        let mut cumulative_performance = 0.0;
        let mut cumulative_usability = 0.0;

        // Calculate cumulative improvements with compounding
        for (idx, refinement) in refinements.iter().enumerate() {
            let compound_factor = self.compound_factor.powi(idx as i32);

            cumulative_clarity += refinement.impact.clarity_delta * compound_factor;
            cumulative_consistency += refinement.impact.consistency_delta * compound_factor;
            cumulative_performance += refinement.impact.performance_delta * compound_factor;
            cumulative_usability += refinement.impact.usability_delta * compound_factor;
        }

        let compound_multiplier = self.compound_factor.powi(total_refinements as i32);

        CompoundImpact {
            total_refinements,
            cumulative_clarity,
            cumulative_consistency,
            cumulative_performance,
            cumulative_usability,
            compound_multiplier,
        }
    }

    /// Get refinement trends over time
    pub fn get_refinement_trends(&self, days: i64) -> HashMap<RefinementType, usize> {
        let cutoff = Utc::now() - chrono::Duration::days(days);
        let mut trends = HashMap::new();

        for refinement in &self.refinements {
            if refinement.applied_at >= cutoff {
                *trends.entry(refinement.refinement_type.clone()).or_insert(0) += 1;
            }
        }

        trends
    }

    /// Get total number of refinements
    pub fn total_refinements(&self) -> usize {
        self.refinements.len()
    }

    /// Get refinements by type
    pub fn get_refinements_by_type(&self, refinement_type: &RefinementType) -> Vec<&SemanticRefinement> {
        self.refinements
            .iter()
            .filter(|r| &r.refinement_type == refinement_type)
            .collect()
    }

    /// Get recent refinements
    pub fn get_recent_refinements(&self, limit: usize) -> Vec<&SemanticRefinement> {
        let mut refinements: Vec<&SemanticRefinement> = self.refinements.iter().collect();
        refinements.sort_by(|a, b| b.applied_at.cmp(&a.applied_at));
        refinements.into_iter().take(limit).collect()
    }

    /// Calculate overall ontology quality score based on all refinements
    pub fn calculate_ontology_quality_score(&self) -> f64 {
        if self.refinements.is_empty() {
            return 0.5; // Baseline score
        }

        let mut total_impact = 0.0;

        for refinement in &self.refinements {
            total_impact += refinement.impact.overall_impact();
        }

        let avg_impact = total_impact / self.refinements.len() as f64;
        let compound_multiplier = self.compound_factor.powi(self.refinements.len() as i32);

        // Quality score starts at 0.5 and improves with refinements
        (0.5 + (avg_impact * compound_multiplier * 0.5)).min(1.0)
    }

    fn target_key(&self, target: &RefinementTarget) -> String {
        match target {
            RefinementTarget::Class { class_uri } => format!("class:{}", class_uri),
            RefinementTarget::Property { property_uri } => format!("property:{}", property_uri),
            RefinementTarget::Namespace { namespace_uri } => format!("namespace:{}", namespace_uri),
            RefinementTarget::Query { query_id } => format!("query:{}", query_id),
            RefinementTarget::Template { template_id } => format!("template:{}", template_id),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_refinement(clarity: f64, consistency: f64) -> SemanticRefinement {
        SemanticRefinement {
            id: uuid::Uuid::new_v4().to_string(),
            refinement_type: RefinementType::Annotation,
            description: "Test refinement".to_string(),
            applied_at: Utc::now(),
            applied_by: "test".to_string(),
            target: RefinementTarget::Class {
                class_uri: "http://example.org/Class1".to_string(),
            },
            impact: RefinementImpact {
                clarity_delta: clarity,
                consistency_delta: consistency,
                performance_delta: 0.0,
                usability_delta: 0.0,
            },
            compound_score: 0.0,
        }
    }

    #[test]
    fn test_refinement_tracking() {
        let mut tracker = RefinementTracker::new(1.05);

        let refinement = create_test_refinement(0.1, 0.1);
        tracker.track_refinement(refinement).unwrap();

        assert_eq!(tracker.total_refinements(), 1);
    }

    #[test]
    fn test_compound_impact() {
        let mut tracker = RefinementTracker::new(1.05);

        let target = RefinementTarget::Class {
            class_uri: "http://example.org/Class1".to_string(),
        };

        // Add multiple refinements
        for _ in 0..5 {
            let mut refinement = create_test_refinement(0.1, 0.1);
            refinement.target = target.clone();
            tracker.track_refinement(refinement).unwrap();
        }

        let impact = tracker.calculate_compound_impact(&target);
        assert_eq!(impact.total_refinements, 5);
        assert!(impact.compound_multiplier > 1.0);
        assert!(impact.cumulative_clarity > 0.5); // Compounded effect
    }

    #[test]
    fn test_quality_score_improvement() {
        let mut tracker = RefinementTracker::new(1.05);

        let initial_score = tracker.calculate_ontology_quality_score();
        assert_eq!(initial_score, 0.5); // Baseline

        // Add refinements
        for _ in 0..10 {
            tracker.track_refinement(create_test_refinement(0.2, 0.2)).unwrap();
        }

        let improved_score = tracker.calculate_ontology_quality_score();
        assert!(improved_score > initial_score);
        assert!(improved_score > 0.5);
    }

    #[test]
    fn test_refinement_trends() {
        let mut tracker = RefinementTracker::new(1.05);

        for _ in 0..3 {
            tracker.track_refinement(create_test_refinement(0.1, 0.1)).unwrap();
        }

        let trends = tracker.get_refinement_trends(30);
        assert_eq!(*trends.get(&RefinementType::Annotation).unwrap(), 3);
    }
}
