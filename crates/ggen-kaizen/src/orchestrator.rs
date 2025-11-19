//! # Kaizen Orchestrator
//!
//! Central coordinator for continuous improvement cycles, integrating
//! PDCA cycles, suggestion mining, semantic refinement, namespace organization,
//! and standard work documentation.

use chrono::Utc;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{info, warn};

use crate::{
    documentation::{PracticeCategory, StandardWorkGenerator},
    namespace::{FiveSReport, NamespaceElement, NamespaceOrganizer, NamingConventions},
    pdca::{Action, ActionType, ImprovementTarget, PDCACycle},
    refinement::{RefinementTracker, RefinementType, SemanticRefinement},
    suggestion::{PainPoint, PainPointCategory, Severity, Suggestion, SuggestionEngine, SuggestionThresholds},
    Result,
};

/// Configuration for the Kaizen engine
#[derive(Debug, Clone)]
pub struct KaizenConfig {
    /// How often to run improvement cycles (in seconds)
    pub cycle_interval_secs: u64,
    /// Minimum improvement threshold to apply changes
    pub min_improvement_threshold: f64,
    /// Auto-apply improvements below risk threshold
    pub auto_apply_threshold: f64,
    /// Compound factor for refinement tracking
    pub compound_factor: f64,
    /// Suggestion engine thresholds
    pub suggestion_thresholds: SuggestionThresholds,
}

impl Default for KaizenConfig {
    fn default() -> Self {
        Self {
            cycle_interval_secs: 3600, // 1 hour
            min_improvement_threshold: 0.05, // 5% minimum improvement
            auto_apply_threshold: 0.3, // Auto-apply if risk < 30%
            compound_factor: 1.05, // 5% compounding
            suggestion_thresholds: SuggestionThresholds::default(),
        }
    }
}

/// The main Kaizen orchestrator
pub struct KaizenOrchestrator {
    config: KaizenConfig,
    active_cycles: Arc<RwLock<Vec<PDCACycle>>>,
    suggestion_engine: Arc<RwLock<SuggestionEngine>>,
    refinement_tracker: Arc<RwLock<RefinementTracker>>,
    namespace_organizer: Arc<RwLock<NamespaceOrganizer>>,
    doc_generator: Arc<RwLock<StandardWorkGenerator>>,
}

impl KaizenOrchestrator {
    /// Create a new Kaizen orchestrator
    pub fn new(config: KaizenConfig) -> Result<Self> {
        Ok(Self {
            config: config.clone(),
            active_cycles: Arc::new(RwLock::new(Vec::new())),
            suggestion_engine: Arc::new(RwLock::new(SuggestionEngine::new(
                config.suggestion_thresholds,
            ))),
            refinement_tracker: Arc::new(RwLock::new(RefinementTracker::new(config.compound_factor))),
            namespace_organizer: Arc::new(RwLock::new(NamespaceOrganizer::new())),
            doc_generator: Arc::new(RwLock::new(StandardWorkGenerator::new()?)),
        })
    }

    /// Start a continuous improvement cycle
    pub async fn start_cycle(&self, target: ImprovementTarget) -> Result<String> {
        let mut cycle = PDCACycle::new(target);
        let cycle_id = cycle.id.clone();

        info!(
            cycle_id = %cycle_id,
            target = ?cycle.target,
            "Starting Kaizen improvement cycle"
        );

        // Execute the full PDCA cycle
        self.execute_pdca_cycle(&mut cycle).await?;

        // Store completed cycle
        let mut cycles = self.active_cycles.write().await;
        cycles.push(cycle);

        Ok(cycle_id)
    }

    /// Execute a complete PDCA cycle
    async fn execute_pdca_cycle(&self, cycle: &mut PDCACycle) -> Result<()> {
        // PLAN phase
        info!(cycle_id = %cycle.id, "PLAN: Analyzing improvement opportunities");
        self.plan_improvements(cycle).await?;
        cycle.advance_phase()?;

        // DO phase
        info!(cycle_id = %cycle.id, "DO: Executing planned improvements");
        cycle.execute_actions().await?;
        cycle.advance_phase()?;

        // CHECK phase
        info!(cycle_id = %cycle.id, "CHECK: Validating improvements");
        let success = cycle.validate_results()?;
        cycle.advance_phase()?;

        // ACT phase
        info!(cycle_id = %cycle.id, "ACT: Standardizing or rolling back");
        if success {
            self.standardize_improvements(cycle).await?;
            cycle.complete(true)?;
        } else {
            warn!(cycle_id = %cycle.id, "Improvements did not meet targets, rolling back");
            cycle.complete(false)?;
        }

        Ok(())
    }

    /// Plan improvements based on suggestions and pain points
    async fn plan_improvements(&self, cycle: &mut PDCACycle) -> Result<()> {
        // Generate suggestions from pain points
        let mut engine = self.suggestion_engine.write().await;
        let suggestions = engine.generate_suggestions()?;

        info!(
            cycle_id = %cycle.id,
            suggestion_count = suggestions.len(),
            "Generated improvement suggestions"
        );

        // Convert top suggestions into actions
        for suggestion in suggestions.iter().take(3) {
            // Limit to top 3 suggestions per cycle
            let action = self.suggestion_to_action(suggestion, cycle)?;
            cycle.add_action(action);
        }

        Ok(())
    }

    /// Standardize successful improvements
    async fn standardize_improvements(&self, cycle: &PDCACycle) -> Result<()> {
        // Track refinements
        let mut tracker = self.refinement_tracker.write().await;

        for action in &cycle.actions {
            if let Some(result) = &action.result {
                if result.success {
                    let refinement = self.action_to_refinement(action)?;
                    tracker.track_refinement(refinement)?;
                }
            }
        }

        // Extract best practices
        let mut doc_gen = self.doc_generator.write().await;
        if let Some(improvement) = cycle.total_improvement() {
            if improvement > self.config.min_improvement_threshold {
                let _practice = doc_gen.extract_best_practice(
                    format!("Improvement for {:?}", cycle.target),
                    self.target_to_practice_category(&cycle.target),
                    cycle.actions.len(),
                    improvement,
                );
            }
        }

        Ok(())
    }

    /// Record a pain point for future improvement
    pub async fn record_pain_point(&self, pain_point: PainPoint) -> Result<()> {
        let mut engine = self.suggestion_engine.write().await;
        engine.record_pain_point(pain_point);
        Ok(())
    }

    /// Get prioritized improvement suggestions
    pub async fn get_suggestions(&self) -> Vec<Suggestion> {
        let engine = self.suggestion_engine.read().await;
        engine
            .get_prioritized_suggestions()
            .into_iter()
            .cloned()
            .collect()
    }

    /// Organize a namespace using 5S methodology
    pub async fn organize_namespace(
        &self,
        namespace_uri: String,
        elements: Vec<NamespaceElement>,
        conventions: NamingConventions,
    ) -> Result<FiveSReport> {
        let mut organizer = self.namespace_organizer.write().await;
        organizer
            .organize_namespace(namespace_uri, elements, conventions)
            .await
    }

    /// Get ontology quality score
    pub async fn get_quality_score(&self) -> f64 {
        let tracker = self.refinement_tracker.read().await;
        tracker.calculate_ontology_quality_score()
    }

    /// Generate standard work documentation
    pub async fn generate_documentation(
        &self,
        title: String,
        purpose: String,
        scope: String,
    ) -> Result<String> {
        let doc_gen = self.doc_generator.read().await;
        let document = doc_gen.generate_documentation(title, purpose, scope)?;
        doc_gen.render_to_markdown(&document)
    }

    /// Get compound impact for a specific refinement target
    pub async fn get_compound_impact(
        &self,
        target: &crate::refinement::RefinementTarget,
    ) -> crate::refinement::CompoundImpact {
        let tracker = self.refinement_tracker.read().await;
        tracker.calculate_compound_impact(target)
    }

    /// Run continuous improvement monitoring
    pub async fn run_continuous_improvement(&self) -> Result<()> {
        info!("Starting continuous improvement monitoring");

        loop {
            // Generate suggestions
            let suggestions = {
                let mut engine = self.suggestion_engine.write().await;
                engine.generate_suggestions()?
            };

            // Start cycles for high-priority suggestions
            for suggestion in suggestions.iter().take(1) {
                // One cycle at a time
                if suggestion.estimated_impact.roi() >= self.config.auto_apply_threshold {
                    // Auto-start cycle for high-ROI improvements
                    let target = ImprovementTarget::OntologyStructure {
                        ontology_uri: "auto".to_string(),
                        focus_area: suggestion.title.clone(),
                    };

                    self.start_cycle(target).await?;
                }
            }

            // Wait for next interval
            tokio::time::sleep(tokio::time::Duration::from_secs(
                self.config.cycle_interval_secs,
            ))
            .await;
        }
    }

    // Helper methods

    fn suggestion_to_action(&self, suggestion: &Suggestion, cycle: &PDCACycle) -> Result<Action> {
        let action_type = match &cycle.target {
            ImprovementTarget::OntologyStructure { .. } => ActionType::RefactorOntology {
                query: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
            },
            ImprovementTarget::NamespaceOrganization { namespace } => {
                ActionType::ReorganizeNamespace {
                    from: namespace.clone(),
                    to: format!("{}/organized", namespace),
                }
            }
            ImprovementTarget::TemplateQuality { template_id } => ActionType::UpdateTemplate {
                template_id: template_id.clone(),
                changes: suggestion.description.clone(),
            },
            ImprovementTarget::GenerationPerformance { component } => ActionType::OptimizeQuery {
                original: component.clone(),
                optimized: format!("{}_optimized", component),
            },
        };

        Ok(Action {
            id: uuid::Uuid::new_v4().to_string(),
            description: suggestion.title.clone(),
            action_type,
            executed_at: None,
            result: None,
        })
    }

    fn action_to_refinement(&self, action: &Action) -> Result<SemanticRefinement> {
        let refinement_type = match &action.action_type {
            ActionType::RefactorOntology { .. } => RefinementType::HierarchyImprovement,
            ActionType::ReorganizeNamespace { .. } => RefinementType::NamingConsistency,
            ActionType::UpdateTemplate { .. } => RefinementType::DocumentationEnhancement,
            ActionType::OptimizeQuery { .. } => RefinementType::QueryOptimization,
            ActionType::UpdateDocumentation { .. } => RefinementType::DocumentationEnhancement,
        };

        Ok(SemanticRefinement {
            id: uuid::Uuid::new_v4().to_string(),
            refinement_type,
            description: action.description.clone(),
            applied_at: action.executed_at.unwrap_or_else(Utc::now),
            applied_by: "kaizen-orchestrator".to_string(),
            target: crate::refinement::RefinementTarget::Class {
                class_uri: "auto".to_string(),
            },
            impact: crate::refinement::RefinementImpact {
                clarity_delta: 0.1,
                consistency_delta: 0.1,
                performance_delta: 0.1,
                usability_delta: 0.1,
            },
            compound_score: 0.0,
        })
    }

    fn target_to_practice_category(&self, target: &ImprovementTarget) -> PracticeCategory {
        match target {
            ImprovementTarget::OntologyStructure { .. } => PracticeCategory::OntologyDesign,
            ImprovementTarget::NamespaceOrganization { .. } => PracticeCategory::NamingConvention,
            ImprovementTarget::TemplateQuality { .. } => PracticeCategory::Documentation,
            ImprovementTarget::GenerationPerformance { .. } => PracticeCategory::Performance,
        }
    }
}

impl Default for KaizenOrchestrator {
    fn default() -> Self {
        Self::new(KaizenConfig::default()).expect("Failed to create KaizenOrchestrator")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let orchestrator = KaizenOrchestrator::new(KaizenConfig::default()).unwrap();
        let quality_score = orchestrator.get_quality_score().await;
        assert_eq!(quality_score, 0.5); // Baseline
    }

    #[tokio::test]
    async fn test_pain_point_recording() {
        let orchestrator = KaizenOrchestrator::new(KaizenConfig::default()).unwrap();

        let pain_point = PainPoint {
            id: "test-1".to_string(),
            category: PainPointCategory::Performance,
            description: "Slow queries".to_string(),
            frequency: 10,
            severity: crate::suggestion::Severity::High,
            first_observed: Utc::now(),
            last_observed: Utc::now(),
            affected_components: vec!["query-engine".to_string()],
            evidence: vec![],
        };

        orchestrator.record_pain_point(pain_point).await.unwrap();
        let suggestions = orchestrator.get_suggestions().await;

        // With default thresholds, this should generate suggestions
        assert!(!suggestions.is_empty());
    }

    #[tokio::test]
    async fn test_cycle_execution() {
        let orchestrator = KaizenOrchestrator::new(KaizenConfig::default()).unwrap();

        let target = ImprovementTarget::OntologyStructure {
            ontology_uri: "http://example.org/test".to_string(),
            focus_area: "test".to_string(),
        };

        let cycle_id = orchestrator.start_cycle(target).await.unwrap();
        assert!(!cycle_id.is_empty());
    }
}
