//! Coordinator for mutation-based validation
//!
//! Orchestrates 7 agents through two phases:
//! PHASE 1: Inject mutations (Agents 1-3)
//! PHASE 2: Detect and analyze (Agents 4-7)

use crate::agents::MutationAgent;
use crate::mutations::{MutationReport, MutationResult, TestSuggestion};
use anyhow::Result;
use std::collections::HashMap;
use tracing::{info, warn};

/// Coordinator for mutation-based validation
pub struct MutationCoordinator {
    /// Map of agent ID to agent
    agents: HashMap<u8, MutationAgent>,
}

impl MutationCoordinator {
    /// Create a new mutation coordinator
    pub async fn new() -> Result<Self> {
        let agents = MutationAgent::create_all()
            .into_iter()
            .map(|agent| (agent.id, agent))
            .collect();

        Ok(Self { agents })
    }

    /// Run complete mutation testing workflow
    ///
    /// # Arguments
    /// * `package_path` - Path to ggen-generated package
    /// * `original_code` - Original code to mutate
    /// * `test_command` - Command to run tests
    ///
    /// # Returns
    /// Complete mutation report with score and suggestions
    pub async fn run_mutation_testing(
        &self,
        package_path: &str,
        original_code: &str,
        test_command: &str,
    ) -> Result<MutationReport, Box<dyn std::error::Error>> {
        info!("=== Starting Mutation-Based Validation ===");
        info!("Package: {}", package_path);

        // PHASE 1: Inject mutations (Agents 1-3)
        info!("\n📍 PHASE 1: Inject Mutations (Agents 1-3)");
        let all_mutations = self.inject_all_mutations(package_path, original_code).await?;

        info!("Total mutations injected: {}", all_mutations.len());

        // PHASE 2: Detect mutations (Agents 4-5)
        info!("\n📍 PHASE 2: Detect Mutations (Agents 4-5)");
        let detected_mutations = self.detect_all_mutations(all_mutations, test_command).await?;

        // PHASE 2: Calculate score (Agent 6)
        info!("\n📍 PHASE 2: Calculate Score (Agent 6)");
        let result = self.calculate_mutation_score(detected_mutations).await?;

        info!(
            "Mutation Score: {:.1}% ({}/{} killed)",
            result.mutation_score * 100.0,
            result.killed,
            result.total_mutations
        );

        // PHASE 2: Suggest improvements (Agent 7)
        info!("\n📍 PHASE 2: Suggest Improvements (Agent 7)");
        let suggestions = self.suggest_test_improvements(&result).await?;

        info!("Test suggestions generated: {}", suggestions.len());

        // Generate final report
        let report = self.generate_report(result, suggestions).await?;

        info!("\n=== Mutation Testing Complete ===");

        Ok(report)
    }

    /// PHASE 1: Inject mutations using Agents 1-3
    async fn inject_all_mutations(
        &self,
        package_path: &str,
        original_code: &str,
    ) -> Result<Vec<crate::mutations::Mutation>> {
        let mut all_mutations = Vec::new();

        // Agents 1-3 inject different mutation types
        for agent_id in 1..=3 {
            if let Some(agent) = self.agents.get(&agent_id) {
                let mutations = agent.inject_mutations(package_path, original_code).await?;
                all_mutations.extend(mutations);
            }
        }

        Ok(all_mutations)
    }

    /// PHASE 2: Detect mutations using Agents 4-5
    async fn detect_all_mutations(
        &self,
        mutations: Vec<crate::mutations::Mutation>,
        test_command: &str,
    ) -> Result<Vec<crate::mutations::Mutation>> {
        let mut detected_mutations = mutations;

        // Agents 4-5 run different test suites
        for agent_id in 4..=5 {
            if let Some(agent) = self.agents.get(&agent_id) {
                detected_mutations = agent.detect_mutations(detected_mutations, test_command).await?;
            }
        }

        Ok(detected_mutations)
    }

    /// PHASE 2: Calculate mutation score using Agent 6
    async fn calculate_mutation_score(
        &self,
        mutations: Vec<crate::mutations::Mutation>,
    ) -> Result<MutationResult> {
        if let Some(agent) = self.agents.get(&6) {
            agent.calculate_score(mutations).await
        } else {
            warn!("Agent 6 not found");
            Ok(MutationResult {
                mutations,
                mutation_score: 0.0,
                total_mutations: 0,
                killed: 0,
                survived: 0,
                errors: 0,
                duration_ms: 0,
            })
        }
    }

    /// PHASE 2: Suggest test improvements using Agent 7
    async fn suggest_test_improvements(
        &self,
        result: &MutationResult,
    ) -> Result<Vec<TestSuggestion>> {
        if let Some(agent) = self.agents.get(&7) {
            agent.suggest_improvements(result).await
        } else {
            warn!("Agent 7 not found");
            Ok(vec![])
        }
    }

    /// Generate final report with assessment
    async fn generate_report(
        &self,
        result: MutationResult,
        suggestions: Vec<TestSuggestion>,
    ) -> Result<MutationReport> {
        let assessment = if result.mutation_score >= 0.8 {
            "Excellent: Strong test suite with good mutation coverage".to_string()
        } else if result.mutation_score >= 0.5 {
            "Good: Tests are catching most mutations, but room for improvement".to_string()
        } else if result.mutation_score >= 0.3 {
            "Fair: Significant test gaps detected. Consider adding more tests.".to_string()
        } else {
            "Poor: Critical test weaknesses. Mutation testing revealed many surviving mutants.".to_string()
        };

        Ok(MutationReport {
            result,
            suggestions,
            assessment,
            timestamp: chrono::Utc::now(),
        })
    }

    /// Get agent by ID
    pub fn get_agent(&self, agent_id: u8) -> Option<&MutationAgent> {
        self.agents.get(&agent_id)
    }

    /// Get all agents
    pub fn agents(&self) -> Vec<&MutationAgent> {
        self.agents.values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_coordinator_creation() {
        let coordinator = MutationCoordinator::new().await.unwrap();
        assert_eq!(coordinator.agents().len(), 7);
    }

    #[tokio::test]
    async fn test_get_agent_by_id() {
        let coordinator = MutationCoordinator::new().await.unwrap();
        let agent = coordinator.get_agent(1);
        assert!(agent.is_some());
        assert_eq!(agent.unwrap().id, 1);
    }
}
