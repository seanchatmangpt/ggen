//! Coordinator for fuzzing-based validation
//!
//! Orchestrates 7 agents through comprehensive fuzzing:
//! Each agent generates fuzzed inputs and tests system robustness

use crate::agents::FuzzingAgent;
use crate::fuzzers::{FuzzReport, FuzzResult, FuzzStatus, RobustnessRecommendation};
use anyhow::Result;
use std::collections::HashMap;
use tracing::{info, warn};

/// Coordinator for fuzzing-based validation
pub struct FuzzingCoordinator {
    /// Map of agent ID to agent
    agents: HashMap<u8, FuzzingAgent>,
}

impl FuzzingCoordinator {
    /// Create a new fuzzing coordinator
    pub async fn new() -> Result<Self> {
        let agents = FuzzingAgent::create_all()
            .into_iter()
            .map(|agent| (agent.id, agent))
            .collect();

        Ok(Self { agents })
    }

    /// Run complete fuzzing campaign
    ///
    /// # Arguments
    /// * `base_input` - Base input to fuzz
    /// * `inputs_per_agent` - Number of fuzzed inputs per agent
    ///
    /// # Returns
    /// Complete fuzzing report with crashes and recommendations
    pub async fn run_fuzzing_campaign(
        &self,
        base_input: &str,
        inputs_per_agent: usize,
    ) -> Result<FuzzReport, Box<dyn std::error::Error>> {
        info!("=== Starting Fuzzing-Based Validation ===");
        info!("Base input: {}", base_input);
        info!("Inputs per agent: {}", inputs_per_agent);

        // Phase 1: Generate fuzzed inputs from all 7 agents
        info!("\n📍 PHASE 1: Generate Fuzzed Inputs (7 Agents)");
        let all_inputs = self.generate_all_fuzzes(base_input, inputs_per_agent).await?;

        info!("Total fuzzed inputs generated: {}", all_inputs.len());

        // Phase 2: Test all inputs
        info!("\n📍 PHASE 2: Test Fuzzed Inputs (7 Agents)");
        let tested_inputs = self.test_all_inputs(all_inputs).await?;

        // Phase 3: Analyze results
        info!("\n📍 PHASE 3: Analyze Results");
        let result = self.analyze_results(tested_inputs).await?;

        info!(
            "Error Rate: {:.1}% ({}/{} failed)",
            result.error_rate() * 100.0,
            result.crashes + result.hangs + result.incorrect + result.errors,
            result.total_inputs
        );

        info!(
            "Crash Rate: {:.1}% ({} crashes)",
            result.crash_rate() * 100.0,
            result.crashes
        );

        // Phase 4: Generate recommendations
        info!("\n📍 PHASE 4: Generate Recommendations");
        let recommendations = self.generate_recommendations(&result).await?;

        info!("Recommendations generated: {}", recommendations.len());

        // Generate final report
        let report = self.generate_report(result, recommendations).await?;

        info!("\n=== Fuzzing Campaign Complete ===");

        Ok(report)
    }

    /// PHASE 1: Generate fuzzed inputs from all 7 agents
    async fn generate_all_fuzzes(
        &self,
        base_input: &str,
        inputs_per_agent: usize,
    ) -> Result<Vec<crate::fuzzers::FuzzInput>> {
        let mut all_inputs = Vec::new();

        // All 7 agents generate fuzzed inputs
        for agent_id in 1..=7 {
            if let Some(agent) = self.agents.get(&agent_id) {
                let inputs = agent.generate_inputs(inputs_per_agent, base_input).await?;
                all_inputs.extend(inputs);
            }
        }

        Ok(all_inputs)
    }

    /// PHASE 2: Test all fuzzed inputs
    async fn test_all_inputs(
        &self,
        inputs: Vec<crate::fuzzers::FuzzInput>,
    ) -> Result<Vec<crate::fuzzers::FuzzInput>> {
        let mut tested_inputs = inputs;

        // Group inputs by agent and test
        let mut agent_inputs: HashMap<u8, Vec<_>> = HashMap::new();
        for input in tested_inputs {
            agent_inputs.entry(input.agent_id).or_default().push(input);
        }

        let mut results = Vec::new();

        for (agent_id, inputs) in agent_inputs {
            if let Some(agent) = self.agents.get(&agent_id) {
                let tested = agent.test_inputs(inputs).await?;
                results.extend(tested);
            }
        }

        Ok(results)
    }

    /// PHASE 3: Analyze fuzzing results
    async fn analyze_results(
        &self,
        inputs: Vec<crate::fuzzers::FuzzInput>,
    ) -> Result<FuzzResult> {
        let total = inputs.len();
        let passed = inputs.iter().filter(|i| i.status == FuzzStatus::Passed).count();
        let crashes = inputs.iter().filter(|i| i.status == FuzzStatus::Crash).count();
        let hangs = inputs.iter().filter(|i| i.status == FuzzStatus::Hang).count();
        let incorrect = inputs.iter().filter(|i| i.status == FuzzStatus::Incorrect).count();
        let errors = inputs.iter().filter(|i| i.status == FuzzStatus::Error).count();

        // Count unique crashes (by input hash)
        let unique_inputs: std::collections::HashSet<_> = inputs
            .iter()
            .filter(|i| i.status == FuzzStatus::Crash)
            .map(|i| &i.input)
            .collect();

        let unique_crashes = unique_inputs.len();

        let result = FuzzResult {
            inputs,
            total_inputs: total,
            passed,
            crashes,
            hangs,
            incorrect,
            errors,
            duration_ms: 0, // TODO: Measure actual time
            unique_crashes,
        };

        info!(
            "Analysis: {} passed, {} crashes, {} hangs, {} incorrect, {} errors",
            passed, crashes, hangs, incorrect, errors
        );

        Ok(result)
    }

    /// PHASE 4: Generate robustness recommendations
    async fn generate_recommendations(
        &self,
        result: &FuzzResult,
    ) -> Result<Vec<RobustnessRecommendation>> {
        let mut all_recommendations = Vec::new();

        // Get recommendations from each agent for failed inputs
        let failed_by_agent: HashMap<u8, Vec<_>> = result
            .inputs
            .iter()
            .filter(|i| !matches!(i.status, FuzzStatus::Passed))
            .fold(HashMap::new(), |mut acc, input| {
                acc.entry(input.agent_id).or_default().push(input.clone());
                acc
            });

        for (agent_id, inputs) in failed_by_agent {
            if let Some(agent) = self.agents.get(&agent_id) {
                let recommendations = agent.generate_recommendations(&inputs).await?;
                all_recommendations.extend(recommendations);
            }
        }

        // Deduplicate recommendations
        all_recommendations.sort_by(|a, b| a.triggering_input.cmp(&b.triggering_input));
        all_recommendations.dedup_by(|a, b| a.triggering_input == b.triggering_input);

        Ok(all_recommendations)
    }

    /// Generate final report with assessment
    async fn generate_report(
        &self,
        result: FuzzResult,
        recommendations: Vec<RobustnessRecommendation>,
    ) -> Result<FuzzReport> {
        let assessment = if result.error_rate() == 0.0 {
            "Excellent: No crashes or errors found. System is robust.".to_string()
        } else if result.error_rate() < 0.05 {
            format!(
                "Good: Low error rate ({:.1}%). {} issues found.",
                result.error_rate() * 100.0,
                result.crashes + result.hangs + result.incorrect + result.errors
            )
        } else if result.error_rate() < 0.15 {
            format!(
                "Fair: Moderate error rate ({:.1}%). {} issues require attention.",
                result.error_rate() * 100.0,
                result.crashes + result.hangs + result.incorrect + result.errors
            )
        } else {
            format!(
                "Poor: High error rate ({:.1}%). System is fragile. {} critical issues found.",
                result.error_rate() * 100.0,
                result.crashes + result.hangs + result.incorrect + result.errors
            )
        };

        Ok(FuzzReport {
            result,
            recommendations,
            assessment,
            timestamp: chrono::Utc::now(),
        })
    }

    /// Get agent by ID
    pub fn get_agent(&self, agent_id: u8) -> Option<&FuzzingAgent> {
        self.agents.get(&agent_id)
    }

    /// Get all agents
    pub fn agents(&self) -> Vec<&FuzzingAgent> {
        self.agents.values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_coordinator_creation() {
        let coordinator = FuzzingCoordinator::new().await.unwrap();
        assert_eq!(coordinator.agents().len(), 7);
    }

    #[tokio::test]
    async fn test_get_agent_by_id() {
        let coordinator = FuzzingCoordinator::new().await.unwrap();
        let agent = coordinator.get_agent(1);
        assert!(agent.is_some());
        assert_eq!(agent.unwrap().id, 1);
    }
}
