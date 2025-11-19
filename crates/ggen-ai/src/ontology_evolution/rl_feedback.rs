//! Reinforcement Learning Feedback Loop for Self-Modifying RDF Schemas
//!
//! This module implements a sophisticated RL system that allows RDF schemas
//! to evolve based on code generation quality, compilation success, and
//! runtime performance feedback.

use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::sync::Arc;
use tokio::sync::RwLock;

/// Feedback signals from code generation and execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FeedbackSignal {
    /// Code compiled successfully
    CompilationSuccess { language: String, time_ms: u64 },

    /// Code failed to compile
    CompilationFailure { language: String, error: String },

    /// Tests passed
    TestSuccess { coverage: f64, time_ms: u64 },

    /// Tests failed
    TestFailure { failed_count: usize, error: String },

    /// Runtime performance metrics
    RuntimeMetrics {
        execution_time_ms: u64,
        memory_mb: f64,
        cpu_usage: f64,
    },

    /// Code quality metrics
    CodeQuality {
        complexity: f64,
        maintainability: f64,
        duplication: f64,
    },

    /// User satisfaction rating
    UserRating { score: f64 },

    /// Custom metric
    Custom { name: String, value: f64 },
}

impl FeedbackSignal {
    /// Convert feedback signal to reward value
    pub fn to_reward(&self) -> f64 {
        match self {
            FeedbackSignal::CompilationSuccess { time_ms, .. } => {
                // Fast compilation = higher reward
                let time_factor = 1.0 - (*time_ms as f64 / 10000.0).min(0.9);
                1.0 + time_factor
            }
            FeedbackSignal::CompilationFailure { .. } => -1.0,
            FeedbackSignal::TestSuccess { coverage, .. } => {
                // Higher coverage = higher reward
                0.5 + coverage
            }
            FeedbackSignal::TestFailure { failed_count, .. } => {
                -0.1 * (*failed_count as f64)
            }
            FeedbackSignal::RuntimeMetrics {
                execution_time_ms,
                memory_mb,
                cpu_usage,
            } => {
                // Better performance = higher reward
                let time_score = 1.0 - (*execution_time_ms as f64 / 5000.0).min(0.9);
                let memory_score = 1.0 - (*memory_mb / 1024.0).min(0.9);
                let cpu_score = 1.0 - cpu_usage.min(0.9);
                (time_score + memory_score + cpu_score) / 3.0
            }
            FeedbackSignal::CodeQuality {
                complexity,
                maintainability,
                duplication,
            } => {
                // Lower complexity/duplication, higher maintainability = higher reward
                let complexity_score = 1.0 - complexity.min(1.0);
                let maintainability_score = *maintainability;
                let duplication_score = 1.0 - duplication.min(1.0);
                (complexity_score + maintainability_score + duplication_score) / 3.0
            }
            FeedbackSignal::UserRating { score } => *score,
            FeedbackSignal::Custom { value, .. } => *value,
        }
    }
}

/// Schema evolution action
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SchemaEvolution {
    /// Add a new RDF class
    AddClass { name: String, parent: Option<String> },

    /// Add a new RDF property
    AddProperty {
        name: String,
        domain: String,
        range: String,
    },

    /// Remove an RDF class
    RemoveClass { name: String },

    /// Remove an RDF property
    RemoveProperty { name: String },

    /// Modify property cardinality
    ModifyCardinality {
        property: String,
        min: u32,
        max: Option<u32>,
    },

    /// Add constraint/validation rule
    AddConstraint { property: String, rule: String },

    /// Add inference rule
    AddInferenceRule { rule: String },

    /// Refactor class hierarchy
    RefactorHierarchy {
        class: String,
        new_parent: String,
    },
}

/// Q-value for state-action pairs
#[derive(Debug, Clone, Serialize, Deserialize)]
struct QValue {
    value: f64,
    visit_count: usize,
}

/// Reinforcement Learning Feedback Loop
pub struct RLFeedbackLoop {
    /// Q-table mapping (state, action) -> Q-value
    q_table: Arc<RwLock<HashMap<(String, SchemaEvolution), QValue>>>,

    /// Learning rate (alpha)
    learning_rate: f64,

    /// Discount factor (gamma)
    discount_factor: f64,

    /// Exploration rate (epsilon)
    exploration_rate: f64,

    /// Replay buffer for experience replay
    replay_buffer: Arc<RwLock<VecDeque<Experience>>>,

    /// Maximum replay buffer size
    max_buffer_size: usize,

    /// Evolution history
    history: Arc<RwLock<Vec<EvolutionEvent>>>,
}

/// Experience tuple for replay learning
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Experience {
    state: String,
    action: SchemaEvolution,
    reward: f64,
    next_state: String,
}

/// Evolution event for tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvolutionEvent {
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub state: String,
    pub action: SchemaEvolution,
    pub reward: f64,
    pub q_value: f64,
}

impl RLFeedbackLoop {
    /// Create a new RL feedback loop
    pub fn new(learning_rate: f64, discount_factor: f64, exploration_rate: f64) -> Self {
        Self {
            q_table: Arc::new(RwLock::new(HashMap::new())),
            learning_rate,
            discount_factor,
            exploration_rate,
            replay_buffer: Arc::new(RwLock::new(VecDeque::new())),
            max_buffer_size: 10000,
            history: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Select action using epsilon-greedy policy
    pub async fn select_action(
        &self,
        state: &str,
        available_actions: &[SchemaEvolution],
    ) -> Result<SchemaEvolution> {
        // Epsilon-greedy: explore with probability epsilon
        if rand::random::<f64>() < self.exploration_rate {
            // Explore: random action
            let idx = rand::random::<usize>() % available_actions.len();
            Ok(available_actions[idx].clone())
        } else {
            // Exploit: best action based on Q-values
            let q_table = self.q_table.read().await;
            let mut best_action = available_actions[0].clone();
            let mut best_q = f64::NEG_INFINITY;

            for action in available_actions {
                let key = (state.to_string(), action.clone());
                let q = q_table
                    .get(&key)
                    .map(|qv| qv.value)
                    .unwrap_or(0.0);

                if q > best_q {
                    best_q = q;
                    best_action = action.clone();
                }
            }

            Ok(best_action)
        }
    }

    /// Update Q-value based on feedback
    pub async fn update_q_value(
        &self,
        state: &str,
        action: &SchemaEvolution,
        reward: f64,
        next_state: &str,
        next_actions: &[SchemaEvolution],
    ) -> Result<()> {
        // Store experience in replay buffer
        let experience = Experience {
            state: state.to_string(),
            action: action.clone(),
            reward,
            next_state: next_state.to_string(),
        };

        {
            let mut buffer = self.replay_buffer.write().await;
            buffer.push_back(experience);
            if buffer.len() > self.max_buffer_size {
                buffer.pop_front();
            }
        }

        // Q-learning update: Q(s,a) = Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
        let mut q_table = self.q_table.write().await;

        // Get current Q-value
        let key = (state.to_string(), action.clone());
        let current_q = q_table
            .get(&key)
            .map(|qv| qv.value)
            .unwrap_or(0.0);

        // Get max Q-value for next state
        let max_next_q = next_actions
            .iter()
            .map(|a| {
                let next_key = (next_state.to_string(), a.clone());
                q_table
                    .get(&next_key)
                    .map(|qv| qv.value)
                    .unwrap_or(0.0)
            })
            .fold(f64::NEG_INFINITY, f64::max);

        // TD error
        let td_error = reward + self.discount_factor * max_next_q - current_q;

        // Update Q-value
        let new_q = current_q + self.learning_rate * td_error;

        // Update Q-table
        q_table
            .entry(key.clone())
            .and_modify(|qv| {
                qv.value = new_q;
                qv.visit_count += 1;
            })
            .or_insert(QValue {
                value: new_q,
                visit_count: 1,
            });

        // Log evolution event
        let mut history = self.history.write().await;
        history.push(EvolutionEvent {
            timestamp: chrono::Utc::now(),
            state: state.to_string(),
            action: action.clone(),
            reward,
            q_value: new_q,
        });

        Ok(())
    }

    /// Process feedback signal and update schema
    pub async fn process_feedback(
        &self,
        state: &str,
        action: &SchemaEvolution,
        feedback: FeedbackSignal,
        next_state: &str,
        next_actions: &[SchemaEvolution],
    ) -> Result<()> {
        let reward = feedback.to_reward();
        self.update_q_value(state, action, reward, next_state, next_actions)
            .await
    }

    /// Perform experience replay for batch learning
    pub async fn experience_replay(&self, batch_size: usize) -> Result<()> {
        let buffer = self.replay_buffer.read().await;
        if buffer.len() < batch_size {
            return Ok(());
        }

        // Sample random batch
        let mut experiences = Vec::new();
        for _ in 0..batch_size {
            let idx = rand::random::<usize>() % buffer.len();
            experiences.push(buffer[idx].clone());
        }
        drop(buffer);

        // Update Q-values for batch
        for exp in experiences {
            // For replay, we don't have next_actions, so we approximate
            // by updating without the max_next_q term (simplified)
            let mut q_table = self.q_table.write().await;
            let key = (exp.state.clone(), exp.action.clone());
            let current_q = q_table
                .get(&key)
                .map(|qv| qv.value)
                .unwrap_or(0.0);

            let new_q = current_q + self.learning_rate * (exp.reward - current_q);

            q_table
                .entry(key)
                .and_modify(|qv| {
                    qv.value = new_q;
                    qv.visit_count += 1;
                })
                .or_insert(QValue {
                    value: new_q,
                    visit_count: 1,
                });
        }

        Ok(())
    }

    /// Get evolution history
    pub async fn get_history(&self) -> Vec<EvolutionEvent> {
        self.history.read().await.clone()
    }

    /// Export Q-table for analysis
    pub async fn export_q_table(&self) -> HashMap<(String, SchemaEvolution), f64> {
        self.q_table
            .read()
            .await
            .iter()
            .map(|(k, v)| (k.clone(), v.value))
            .collect()
    }

    /// Get statistics about learning progress
    pub async fn get_statistics(&self) -> RLStatistics {
        let q_table = self.q_table.read().await;
        let history = self.history.read().await;

        let total_updates = q_table.values().map(|qv| qv.visit_count).sum();
        let avg_q_value = if !q_table.is_empty() {
            q_table.values().map(|qv| qv.value).sum::<f64>() / q_table.len() as f64
        } else {
            0.0
        };

        let avg_reward = if !history.is_empty() {
            history.iter().map(|e| e.reward).sum::<f64>() / history.len() as f64
        } else {
            0.0
        };

        RLStatistics {
            total_states: q_table.keys().map(|(s, _)| s).collect::<std::collections::HashSet<_>>().len(),
            total_actions: q_table.len(),
            total_updates,
            avg_q_value,
            avg_reward,
            total_experiences: self.replay_buffer.read().await.len(),
        }
    }
}

/// Statistics about RL learning progress
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RLStatistics {
    pub total_states: usize,
    pub total_actions: usize,
    pub total_updates: usize,
    pub avg_q_value: f64,
    pub avg_reward: f64,
    pub total_experiences: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_feedback_signal_reward() {
        let signal = FeedbackSignal::CompilationSuccess {
            language: "rust".to_string(),
            time_ms: 1000,
        };
        assert!(signal.to_reward() > 0.0);

        let signal = FeedbackSignal::CompilationFailure {
            language: "rust".to_string(),
            error: "syntax error".to_string(),
        };
        assert!(signal.to_reward() < 0.0);
    }

    #[tokio::test]
    async fn test_rl_feedback_loop() {
        let rl = RLFeedbackLoop::new(0.1, 0.95, 0.1);

        let actions = vec![
            SchemaEvolution::AddClass {
                name: "TestClass".to_string(),
                parent: None,
            },
            SchemaEvolution::AddProperty {
                name: "testProp".to_string(),
                domain: "TestClass".to_string(),
                range: "String".to_string(),
            },
        ];

        // Select action
        let action = rl.select_action("initial", &actions).await.unwrap();
        assert!(actions.contains(&action));

        // Process feedback
        let feedback = FeedbackSignal::CompilationSuccess {
            language: "rust".to_string(),
            time_ms: 500,
        };

        rl.process_feedback("initial", &action, feedback, "next", &actions)
            .await
            .unwrap();

        // Check statistics
        let stats = rl.get_statistics().await;
        assert_eq!(stats.total_states, 1);
        assert!(stats.avg_reward > 0.0);
    }
}
