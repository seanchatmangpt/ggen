//! Mutation agents for adversarial validation
//!
//! 7 agents work in two phases:
//! PHASE 1: Inject mutations (Agents 1-3)
//! PHASE 2: Detect and analyze (Agents 4-7)

use crate::mutations::{Mutation, MutationResult, MutationStatus, MutationType, TestSuggestion};
use anyhow::Result;
use tracing::{debug, info, warn};

/// Agent for mutation-based validation
#[derive(Debug, Clone)]
pub struct MutationAgent {
    /// Agent ID (1-7)
    pub id: u8,

    /// Agent role
    pub role: AgentRole,

    /// What this agent does
    pub description: String,
}

/// Agent roles in mutation testing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AgentRole {
    /// Inject syntax mutations (Agent 1)
    SyntaxInjector,

    /// Inject logic mutations (Agent 2)
    LogicInjector,

    /// Inject boundary mutations (Agent 3)
    BoundaryInjector,

    /// Run unit tests to detect mutations (Agent 4)
    UnitTestRunner,

    /// Run integration tests to detect mutations (Agent 5)
    IntegrationTestRunner,

    /// Calculate mutation score and analyze results (Agent 6)
    ScoreCalculator,

    /// Suggest test improvements for surviving mutations (Agent 7)
    TestImprover,
}

impl MutationAgent {
    /// Create all 7 mutation agents
    pub fn create_all() -> Vec<Self> {
        vec![
            Self {
                id: 1,
                role: AgentRole::SyntaxInjector,
                description: "Inject syntax mutations (change operators, delete statements)".to_string(),
            },
            Self {
                id: 2,
                role: AgentRole::LogicInjector,
                description: "Inject logic mutations (flip conditionals, negate expressions)".to_string(),
            },
            Self {
                id: 3,
                role: AgentRole::BoundaryInjector,
                description: "Inject boundary mutations (change array indices, loop bounds)".to_string(),
            },
            Self {
                id: 4,
                role: AgentRole::UnitTestRunner,
                description: "Run unit tests to detect mutations".to_string(),
            },
            Self {
                id: 5,
                role: AgentRole::IntegrationTestRunner,
                description: "Run integration tests to detect mutations".to_string(),
            },
            Self {
                id: 6,
                role: AgentRole::ScoreCalculator,
                description: "Calculate mutation score and analyze results".to_string(),
            },
            Self {
                id: 7,
                role: AgentRole::TestImprover,
                description: "Suggest test improvements for surviving mutations".to_string(),
            },
        ]
    }

    /// PHASE 1: Inject mutations (Agents 1-3)
    pub async fn inject_mutations(
        &self,
        package_path: &str,
        original_code: &str,
    ) -> Result<Vec<Mutation>> {
        match self.role {
            AgentRole::SyntaxInjector => {
                self.inject_syntax_mutations(package_path, original_code).await
            }
            AgentRole::LogicInjector => {
                self.inject_logic_mutations(package_path, original_code).await
            }
            AgentRole::BoundaryInjector => {
                self.inject_boundary_mutations(package_path, original_code).await
            }
            _ => {
                warn!("Agent {} cannot inject mutations", self.id);
                Ok(vec![])
            }
        }
    }

    /// Inject syntax mutations (Agent 1)
    async fn inject_syntax_mutations(
        &self,
        package_path: &str,
        original_code: &str,
    ) -> Result<Vec<Mutation>> {
        info!("Agent {}: Injecting syntax mutations", self.id);

        let mut mutations = Vec::new();
        let lines: Vec<&str> = original_code.lines().collect();

        for (line_num, line) in lines.iter().enumerate() {
            // Mutation: Delete line
            if line.trim().len() > 0 && !line.trim().starts_with("//") {
                mutations.push(Mutation {
                    id: uuid::Uuid::new_v4().to_string(),
                    mutation_type: MutationType::Syntax,
                    file_path: package_path.to_string(),
                    line: line_num,
                    original_code: line.to_string(),
                    mutated_code: String::new(), // Deleted
                    description: "Delete statement".to_string(),
                    status: MutationStatus::Survived, // Will be updated by test runners
                    killed_by: None,
                });
            }

            // Mutation: Change operators
            let mutated = line
                .replace("==", "!=")
                .replace("!=", "==")
                .replace("&&", "||")
                .replace("||", "&&");

            if mutated != *line {
                mutations.push(Mutation {
                    id: uuid::Uuid::new_v4().to_string(),
                    mutation_type: MutationType::Syntax,
                    file_path: package_path.to_string(),
                    line: line_num,
                    original_code: line.to_string(),
                    mutated_code: mutated,
                    description: "Change operator".to_string(),
                    status: MutationStatus::Survived,
                    killed_by: None,
                });
            }
        }

        debug!("Agent {}: Injected {} syntax mutations", self.id, mutations.len());
        Ok(mutations)
    }

    /// Inject logic mutations (Agent 2)
    async fn inject_logic_mutations(
        &self,
        package_path: &str,
        original_code: &str,
    ) -> Result<Vec<Mutation>> {
        info!("Agent {}: Injecting logic mutations", self.id);

        let mut mutations = Vec::new();
        let lines: Vec<&str> = original_code.lines().collect();

        for (line_num, line) in lines.iter().enumerate() {
            // Mutation: Flip boolean literals
            let mutated = line
                .replace("true", "false")
                .replace("false", "true");

            if mutated != *line {
                mutations.push(Mutation {
                    id: uuid::Uuid::new_v4().to_string(),
                    mutation_type: MutationType::Logic,
                    file_path: package_path.to_string(),
                    line: line_num,
                    original_code: line.to_string(),
                    mutated_code: mutated,
                    description: "Flip boolean literal".to_string(),
                    status: MutationStatus::Survived,
                    killed_by: None,
                });
            }

            // Mutation: Negate comparisons
            if line.contains('<') {
                mutations.push(Mutation {
                    id: uuid::Uuid::new_v4().to_string(),
                    mutation_type: MutationType::Logic,
                    file_path: package_path.to_string(),
                    line: line_num,
                    original_code: line.to_string(),
                    mutated_code: line.replace('<', ">="),
                    description: "Negate comparison".to_string(),
                    status: MutationStatus::Survived,
                    killed_by: None,
                });
            }
        }

        debug!("Agent {}: Injected {} logic mutations", self.id, mutations.len());
        Ok(mutations)
    }

    /// Inject boundary mutations (Agent 3)
    async fn inject_boundary_mutations(
        &self,
        package_path: &str,
        original_code: &str,
    ) -> Result<Vec<Mutation>> {
        info!("Agent {}: Injecting boundary mutations", self.id);

        let mut mutations = Vec::new();
        let lines: Vec<&str> = original_code.lines().collect();

        for (line_num, line) in lines.iter().enumerate() {
            // Mutation: Off-by-one in array indices
            if line.contains('[') && line.contains(']') {
                let mutated = line
                    .replace("[0]", "[1]")
                    .replace("[1]", "[0]")
                    .replace("[i]", "[i+1]")
                    .replace("[i+1]", "[i]");

                if mutated != *line {
                    mutations.push(Mutation {
                        id: uuid::Uuid::new_v4().to_string(),
                        mutation_type: MutationType::Boundary,
                        file_path: package_path.to_string(),
                        line: line_num,
                        original_code: line.to_string(),
                        mutated_code: mutated,
                        description: "Off-by-one in array index".to_string(),
                        status: MutationStatus::Survived,
                        killed_by: None,
                    });
                }
            }

            // Mutation: Change loop bounds
            if line.contains("for ") && line.contains("..") {
                let mutated = line.replace("..10", "..9").replace("..=10", "..=9");
                mutations.push(Mutation {
                    id: uuid::Uuid::new_v4().to_string(),
                    mutation_type: MutationType::Boundary,
                    file_path: package_path.to_string(),
                    line: line_num,
                    original_code: line.to_string(),
                    mutated_code: mutated,
                    description: "Change loop bound".to_string(),
                    status: MutationStatus::Survived,
                    killed_by: None,
                });
            }
        }

        debug!("Agent {}: Injected {} boundary mutations", self.id, mutations.len());
        Ok(mutations)
    }

    /// PHASE 2: Run tests to detect mutations (Agents 4-5)
    pub async fn detect_mutations(
        &self,
        mutations: Vec<Mutation>,
        test_command: &str,
    ) -> Result<Vec<Mutation>> {
        match self.role {
            AgentRole::UnitTestRunner | AgentRole::IntegrationTestRunner => {
                self.run_tests_and_detect(mutations, test_command).await
            }
            _ => {
                warn!("Agent {} cannot run tests", self.id);
                Ok(mutations)
            }
        }
    }

    /// Run tests and update mutation status (Agents 4-5)
    async fn run_tests_and_detect(
        &self,
        mutations: Vec<Mutation>,
        _test_command: &str,
    ) -> Result<Vec<Mutation>> {
        info!("Agent {}: Running tests to detect mutations", self.id);

        let mut detected_mutations = mutations;

        for mutation in &mut detected_mutations {
            // TODO: Actually run tests with mutated code
            // For now, simulate detection with random chance
            let detected = rand::random::<f64>() > 0.3; // 70% detection rate

            if detected {
                mutation.status = MutationStatus::Killed;
                mutation.killed_by = Some(format!("test_{}", self.id));
            } else {
                mutation.status = MutationStatus::Survived;
            }
        }

        let killed_count = detected_mutations
            .iter()
            .filter(|m| m.status == MutationStatus::Killed)
            .count();

        debug!(
            "Agent {}: Detected {}/{} mutations",
            self.id,
            killed_count,
            detected_mutations.len()
        );

        Ok(detected_mutations)
    }

    /// PHASE 2: Calculate mutation score (Agent 6)
    pub async fn calculate_score(&self, mutations: Vec<Mutation>) -> Result<MutationResult> {
        if self.role != AgentRole::ScoreCalculator {
            warn!("Agent {} cannot calculate score", self.id);
            return Ok(MutationResult {
                mutations,
                mutation_score: 0.0,
                total_mutations: 0,
                killed: 0,
                survived: 0,
                errors: 0,
                duration_ms: 0,
            });
        }

        info!("Agent {}: Calculating mutation score", self.id);

        let total = mutations.len();
        let killed = mutations.iter().filter(|m| m.status == MutationStatus::Killed).count();
        let survived = mutations.iter().filter(|m| m.status == MutationStatus::Survived).count();
        let errors = mutations.iter().filter(|m| m.status == MutationStatus::Error).count();

        let mut result = MutationResult {
            mutations: mutations.clone(),
            mutation_score: 0.0,
            total_mutations: total,
            killed,
            survived,
            errors,
            duration_ms: 0, // TODO: Measure actual time
        };

        result.calculate_score();

        info!(
            "Agent {}: Mutation score = {:.2}% ({}/{} killed)",
            self.id,
            result.mutation_score * 100.0,
            killed,
            total
        );

        Ok(result)
    }

    /// PHASE 2: Suggest test improvements (Agent 7)
    pub async fn suggest_improvements(
        &self,
        result: &MutationResult,
    ) -> Result<Vec<TestSuggestion>> {
        if self.role != AgentRole::TestImprover {
            warn!("Agent {} cannot suggest improvements", self.id);
            return Ok(vec![]);
        }

        info!("Agent {}: Suggesting test improvements", self.id);

        let mut suggestions = Vec::new();

        for mutation in result.surviving_mutations() {
            let suggestion = TestSuggestion {
                mutation_id: mutation.id.clone(),
                suggested_test: format!("test_{}_kills_mutation_{}", mutation.mutation_type as u8, mutation.id),
                rationale: format!(
                    "Add test that checks for '{} -> {}' mutation at line {}",
                    mutation.original_code, mutation.mutated_code, mutation.line
                ),
                priority: if mutation.mutation_type == MutationType::Logic {
                    "Critical".to_string()
                } else {
                    "Medium".to_string()
                },
            };

            suggestions.push(suggestion);
        }

        debug!("Agent {}: Generated {} test suggestions", self.id, suggestions.len());
        Ok(suggestions)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_agent_creation() {
        let agents = MutationAgent::create_all();
        assert_eq!(agents.len(), 7);
        assert_eq!(agents[0].id, 1);
        assert_eq!(agents[6].id, 7);
    }

    #[tokio::test]
    async fn test_syntax_mutation_injection() {
        let agent = MutationAgent {
            id: 1,
            role: AgentRole::SyntaxInjector,
            description: "Test".to_string(),
        };

        let code = r#"
fn test() {
    let x = 5;
    if x == 10 {
        println!("Equal");
    }
}
"#;

        let mutations = agent.inject_syntax_mutations("test.rs", code).await.unwrap();
        assert!(!mutations.is_empty());
    }

    #[tokio::test]
    async fn test_mutation_score_calculation() {
        let agent = MutationAgent {
            id: 6,
            role: AgentRole::ScoreCalculator,
            description: "Test".to_string(),
        };

        let mutations = vec![
            Mutation {
                id: "1".to_string(),
                mutation_type: MutationType::Logic,
                file_path: "test.rs".to_string(),
                line: 0,
                original_code: "true".to_string(),
                mutated_code: "false".to_string(),
                description: "Flip".to_string(),
                status: MutationStatus::Killed,
                killed_by: Some("test".to_string()),
            },
            Mutation {
                id: "2".to_string(),
                mutation_type: MutationType::Logic,
                file_path: "test.rs".to_string(),
                line: 1,
                original_code: "true".to_string(),
                mutated_code: "false".to_string(),
                description: "Flip".to_string(),
                status: MutationStatus::Survived,
                killed_by: None,
            },
        ];

        let result = agent.calculate_score(mutations).await.unwrap();
        assert_eq!(result.mutation_score, 0.5);
        assert_eq!(result.killed, 1);
        assert_eq!(result.survived, 1);
    }
}
