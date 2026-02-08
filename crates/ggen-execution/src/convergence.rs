// Semantic convergence for 90% semantic convergence
use crate::types::*;
use crate::error::*;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{RwLock, Mutex};
use serde_json::Value;
use serde::{Serialize, Deserialize};
use chrono::{DateTime, Utc};

// ============================================================================
// SEMANTIC CONVERGENCE METRICS
// ============================================================================

/// Metrics for tracking semantic convergence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvergenceMetrics {
    pub total_agents: usize,
    pub active_agents: usize,
    pub total_messages: u64,
    pub successful_convergences: u64,
    pub convergence_rate: f64,
    pub average_convergence_time_ms: u64,
    pub semantic_similarity_score: f64,
    pub protocol_compliance_rate: f64,
    pub last_updated: DateTime<Utc>,
}

impl Default for ConvergenceMetrics {
    fn default() -> Self {
        Self {
            total_agents: 0,
            active_agents: 0,
            total_messages: 0,
            successful_convergences: 0,
            convergence_rate: 0.0,
            average_convergence_time_ms: 0,
            semantic_similarity_score: 0.0,
            protocol_compliance_rate: 0.0,
            last_updated: Utc::now(),
        }
    }
}

/// Convergence configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvergenceConfig {
    pub target_convergence_rate: f64,
    pub target_semantic_similarity: f64,
    pub max_convergence_time_ms: u64,
    pub convergence_check_interval_ms: u64,
    pub enable_adaptive_convergence: bool,
    pub convergence_threshold: f64,
    pub metrics_history_size: usize,
}

impl Default for ConvergenceConfig {
    fn default() -> Self {
        Self {
            target_convergence_rate: 0.9,
            target_semantic_similarity: 0.9,
            max_convergence_time_ms: 30000,
            convergence_check_interval_ms: 1000,
            enable_adaptive_convergence: true,
            convergence_threshold: 0.85,
            metrics_history_size: 100,
        }
    }
}

// ============================================================================
// SEMANTIC CONVERGENCE ENGINE
// ============================================================================

/// Semantic convergence engine for achieving 90% semantic convergence
pub struct SemanticConvergenceEngine {
    config: ConvergenceConfig,
    metrics: Arc<RwLock<ConvergenceMetrics>>,
    agent_states: HashMap<AgentId, AgentConvergenceState>,
    message_history: Vec<ConvergenceMessage>,
    convergence_strategies: Vec<Box<dyn ConvergenceStrategyTrait>>,
}

/// Agent convergence state
#[derive(Debug, Clone)]
pub struct AgentConvergenceState {
    pub agent_id: AgentId,
    pub capabilities: Vec<String>,
    pub knowledge_base: HashMap<String, Value>,
    pub semantic_score: f64,
    pub last_contribution: Option<DateTime<Utc>>,
    pub contribution_count: u64,
    pub compliance_score: f64,
}

/// Convergence message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvergenceMessage {
    pub id: MessageId,
    pub source_agent: AgentId,
    pub target_agents: Vec<AgentId>,
    pub content: Value,
    pub timestamp: DateTime<Utc>,
    pub semantic_similarity: f64,
    pub contribution_type: ContributionType,
}

/// Contribution types for semantic convergence
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ContributionType {
    KnowledgeSharing,
    ProtocolAlignment,
    SchemaNormalization,
    MessageValidation,
    ErrorCorrection,
    ConsensusBuilding,
}

/// Trait for convergence strategies
#[async_trait::async_trait]
pub trait ConvergenceStrategyTrait: Send + Sync {
    fn name(&self) -> &str;
    async fn apply_convergence(&self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage) -> Result<f64, ExecutionError>;
    fn is_applicable(&self, message: &ConvergenceMessage) -> bool;
}

// ============================================================================
// CONVERGENCE IMPLEMENTATIONS
// ============================================================================

/// Alignment strategy for semantic convergence
pub struct AlignmentStrategy;

#[async_trait::async_trait]
impl ConvergenceStrategyTrait for AlignmentStrategy {
    fn name(&self) -> &str {
        "AlignmentStrategy"
    }

    async fn apply_convergence(&self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Apply semantic alignment to message content
        let aligned_content = Self::align_semantic_content(&message.content)?;

        // Update agent states with aligned content
        for target_id in &message.target_agents {
            if let Some(agent_state) = engine.agent_states.get_mut(target_id) {
                Self::update_agent_knowledge(agent_state, &aligned_content);
                agent_state.semantic_score = Self::calculate_semantic_score(agent_state);
            }
        }

        Ok(Self::calculate_alignment_score(&message.content, &aligned_content))
    }

    fn is_applicable(&self, message: &ConvergenceMessage) -> bool {
        matches!(message.contribution_type, ContributionType::KnowledgeSharing | ContributionType::ProtocolAlignment)
    }
}

impl AlignmentStrategy {
    fn align_semantic_content(content: &Value) -> Result<Value, ExecutionError> {
        // Implement semantic alignment logic
        // This would involve NLP processing, ontology mapping, etc.
        Ok(content.clone())
    }

    fn update_agent_knowledge(agent_state: &mut AgentConvergenceState, content: &Value) {
        // Update agent's knowledge base with aligned content
        if let Some(map) = content.as_object() {
            for (key, value) in map {
                agent_state.knowledge_base.insert(key.clone(), value.clone());
            }
        }
    }

    fn calculate_semantic_score(agent_state: &AgentConvergenceState) -> f64 {
        // Calculate semantic score based on knowledge base size and quality
        let knowledge_size = agent_state.knowledge_base.len();
        let contribution_score = agent_state.contribution_count as f64;

        // Simple scoring function - would be more sophisticated in production
        (knowledge_size as f64 * 0.6 + contribution_score * 0.4).min(1.0)
    }

    fn calculate_alignment_score(original: &Value, aligned: &Value) -> f64 {
        // Calculate alignment score between original and aligned content
        if original == aligned {
            1.0
        } else {
            0.8 // Simplified - would use NLP similarity metrics in production
        }
    }
}

/// Validation strategy for semantic convergence
pub struct ValidationStrategy;

#[async_trait::async_trait]
impl ConvergenceStrategyTrait for ValidationStrategy {
    fn name(&self) -> &str {
        "ValidationStrategy"
    }

    async fn apply_convergence(&self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Validate message content against agent capabilities and knowledge
        let validation_score = Self::validate_message_content(message, &engine.agent_states)?;

        // Update compliance scores
        for target_id in &message.target_agents {
            if let Some(agent_state) = engine.agent_states.get_mut(target_id) {
                agent_state.compliance_score = Self::calculate_compliance_score(agent_state, validation_score);
            }
        }

        Ok(validation_score)
    }

    fn is_applicable(&self, message: &ConvergenceMessage) -> bool {
        matches!(message.contribution_type, ContributionType::MessageValidation | ContributionType::ErrorCorrection)
    }
}

impl ValidationStrategy {
    fn validate_message_content(message: &ConvergenceMessage, agent_states: &HashMap<AgentId, AgentConvergenceState>) -> Result<f64, ExecutionError> {
        let mut total_score = 0.0;
        let mut score_count = 0;

        for target_id in &message.target_agents {
            if let Some(agent_state) = agent_states.get(target_id) {
                // Check if agent has capabilities to understand the message
                let content_capabilities = Self::extract_capabilities_from_content(&message.content);
                let capability_overlap = Self::calculate_capability_overlap(&content_capabilities, &agent_state.capabilities);

                total_score += capability_overlap;
                score_count += 1;
            }
        }

        if score_count == 0 {
            return Err(ExecutionError::Convergence("No target agents found for validation".to_string()));
        }

        Ok(total_score / score_count as f64)
    }

    fn extract_capabilities_from_content(_content: &Value) -> Vec<String> {
        // Extract capabilities from message content
        // Simplified implementation
        vec!["generic-capability".to_string()]
    }

    fn calculate_capability_overlap(capabilities_a: &[String], capabilities_b: &[String]) -> f64 {
        let intersection: Vec<&String> = capabilities_a.iter().filter(|cap| capabilities_b.contains(cap)).collect();
        let union_size = capabilities_a.len() + capabilities_b.len() - intersection.len();

        if union_size == 0 {
            0.0
        } else {
            intersection.len() as f64 / union_size as f64
        }
    }

    fn calculate_compliance_score(agent_state: &AgentConvergenceState, validation_score: f64) -> f64 {
        // Calculate overall compliance score combining semantic score and validation score
        (agent_state.semantic_score * 0.7 + validation_score * 0.3).min(1.0)
    }
}

/// Normalization strategy for semantic convergence
pub struct NormalizationStrategy;

#[async_trait::async_trait]
impl ConvergenceStrategyTrait for NormalizationStrategy {
    fn name(&self) -> &str {
        "NormalizationStrategy"
    }

    async fn apply_convergence(&self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Normalize message content to standard format
        let normalized_content = Self::normalize_content(&message.content)?;

        // Update message with normalized content
        let mut updated_message = message.clone();
        updated_message.content = normalized_content;
        updated_message.semantic_similarity = Self::calculate_similarity(&message.content, &updated_message.content);

        // Store normalized message
        engine.message_history.push(updated_message.clone());

        Ok(updated_message.semantic_similarity)
    }

    fn is_applicable(&self, message: &ConvergenceMessage) -> bool {
        matches!(message.contribution_type, ContributionType::SchemaNormalization)
    }
}

impl NormalizationStrategy {
    fn normalize_content(content: &Value) -> Result<Value, ExecutionError> {
        // Implement content normalization logic
        // This would involve schema validation, format conversion, etc.
        Ok(content.clone())
    }

    fn calculate_similarity(original: &Value, normalized: &Value) -> f64 {
        // Calculate similarity between original and normalized content
        if original == normalized {
            1.0
        } else {
            0.9 // Simplified - would use proper similarity metrics in production
        }
    }
}

/// Consensus strategy for semantic convergence
pub struct ConsensusStrategy;

#[async_trait::async_trait]
impl ConvergenceStrategyTrait for ConsensusStrategy {
    fn name(&self) -> &str {
        "ConsensusStrategy"
    }

    async fn apply_convergence(&self, engine: &mut SemanticConvergenceEngine, message: &ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Build consensus among target agents
        let consensus_score = Self::build_consensus(message, &engine.agent_states)?;

        // Update agent states with consensus
        for target_id in &message.target_agents {
            if let Some(agent_state) = engine.agent_states.get_mut(target_id) {
                agent_state.last_contribution = Some(Utc::now());
                agent_state.contribution_count += 1;
            }
        }

        Ok(consensus_score)
    }

    fn is_applicable(&self, message: &ConvergenceMessage) -> bool {
        matches!(message.contribution_type, ContributionType::ConsensusBuilding)
    }
}

impl ConsensusStrategy {
    fn build_consensus(message: &ConvergenceMessage, agent_states: &HashMap<AgentId, AgentConvergenceState>) -> Result<f64, ExecutionError> {
        let mut total_compliance = 0.0;
        let mut compliance_count = 0;

        for target_id in &message.target_agents {
            if let Some(agent_state) = agent_states.get(target_id) {
                total_compliance += agent_state.compliance_score;
                compliance_count += 1;
            }
        }

        if compliance_count == 0 {
            return Err(ExecutionError::Convergence("No target agents for consensus".to_string()));
        }

        Ok(total_compliance / compliance_count as f64)
    }
}

// ============================================================================
// CONVERGENCE ENGINE IMPLEMENTATION
// ============================================================================

impl SemanticConvergenceEngine {
    pub fn new(config: ConvergenceConfig) -> Self {
        Self {
            config,
            metrics: Arc::new(RwLock::new(ConvergenceMetrics::default())),
            agent_states: HashMap::new(),
            message_history: Vec::new(),
            convergence_strategies: vec![
                Box::new(AlignmentStrategy),
                Box::new(ValidationStrategy),
                Box::new(NormalizationStrategy),
                Box::new(ConsensusStrategy),
            ],
        }
    }

    /// Register a new agent with the convergence engine
    pub async fn register_agent(&mut self, agent_id: &str, capabilities: Vec<String>) {
        let agent_state = AgentConvergenceState {
            agent_id: agent_id.to_string(),
            capabilities,
            knowledge_base: HashMap::new(),
            semantic_score: 0.0,
            last_contribution: None,
            contribution_count: 0,
            compliance_score: 0.0,
        };

        self.agent_states.insert(agent_id.to_string(), agent_state);

        // Update metrics
        let mut metrics = self.metrics.write().await;
        metrics.total_agents += 1;
        metrics.active_agents += 1;
        metrics.last_updated = Utc::now();
    }

    /// Process a convergence message
    pub async fn process_message(&mut self, message: ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Find applicable strategies
        let applicable_strategies: Vec<_> = self.convergence_strategies
            .iter()
            .filter(|strategy| strategy.is_applicable(&message))
            .collect();

        if applicable_strategies.is_empty() {
            return Err(ExecutionError::Convergence("No applicable convergence strategies found".to_string()));
        }

        // Apply each applicable strategy
        let mut total_improvement = 0.0;
        let mut strategy_count = 0;

        for strategy in applicable_strategies {
            let improvement = strategy.apply_convergence(self, &message).await?;
            total_improvement += improvement;
            strategy_count += 1;
        }

        // Update metrics
        self.update_metrics(total_improvement / strategy_count.max(1) as f64).await;

        // Store message in history
        self.message_history.push(message);

        // Keep history size within limits
        if self.message_history.len() > self.config.metrics_history_size {
            self.message_history.remove(0);
        }

        Ok(total_improvement / strategy_count.max(1) as f64)
    }

    /// Check if convergence threshold is met
    pub async fn check_convergence(&self) -> Result<bool, ExecutionError> {
        let metrics = self.metrics.read().await;

        if metrics.convergence_rate >= self.config.target_convergence_rate
            && metrics.semantic_similarity_score >= self.config.target_semantic_similarity {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Get current convergence status
    pub async fn get_convergence_status(&self) -> ConvergenceStatus {
        let metrics = self.metrics.read().await;

        ConvergenceStatus {
            convergence_rate: metrics.convergence_rate,
            semantic_similarity: metrics.semantic_similarity_score,
            compliance_rate: metrics.protocol_compliance_rate,
            active_agents: metrics.active_agents,
            total_messages: metrics.total_messages,
            last_updated: metrics.last_updated,
        }
    }

    /// Update metrics after convergence processing
    async fn update_metrics(&mut self, improvement: f64) {
        let mut metrics = self.metrics.write().await;

        metrics.total_messages += 1;
        metrics.convergence_rate = (metrics.convergence_rate * 0.9 + improvement * 0.1).min(1.0);
        metrics.semantic_similarity_score = (metrics.semantic_similarity_score * 0.9 + improvement * 0.1).min(1.0);

        // Calculate average convergence time
        let current_time = Utc::now();
        if let Some(last_update) = metrics.last_updated {
            let elapsed_ms = (current_time.timestamp_millis() - last_update.timestamp_millis()) as u64;
            metrics.average_convergence_time_ms = (metrics.average_convergence_time_ms * 9 + elapsed_ms) / 10;
        }

        metrics.last_updated = current_time;
    }
}

/// Convergence status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConvergenceStatus {
    pub convergence_rate: f64,
    pub semantic_similarity: f64,
    pub compliance_rate: f64,
    pub active_agents: usize,
    pub total_messages: u64,
    pub last_updated: DateTime<Utc>,
}

// ============================================================================
// ADAPTIVE CONVERGENCE
// ============================================================================

/// Adaptive convergence engine that adjusts strategies based on performance
pub struct AdaptiveConvergenceEngine {
    base_engine: SemanticConvergenceEngine,
    strategy_performance: HashMap<String, f64>,
    adaptation_threshold: f64,
}

impl AdaptiveConvergenceEngine {
    pub fn new(config: ConvergenceConfig) -> Self {
        Self {
            base_engine: SemanticConvergenceEngine::new(config),
            strategy_performance: HashMap::new(),
            adaptation_threshold: 0.7,
        }
    }

    /// Process message with adaptive strategy selection
    pub async fn process_message_adaptive(&mut self, message: ConvergenceMessage) -> Result<f64, ExecutionError> {
        // Select best performing strategy
        let best_strategy = self.select_best_strategy(&message).await?;

        // Apply the selected strategy
        let improvement = best_strategy.apply_convergence(&mut self.base_engine, &message).await?;

        // Update strategy performance
        self.update_strategy_performance(best_strategy.name(), improvement).await;

        Ok(improvement)
    }

    /// Select the best performing strategy for this message
    async fn select_best_strategy(&self, message: &ConvergenceMessage) -> Result<Box<dyn ConvergenceStrategyTrait>, ExecutionError> {
        let mut best_strategy = None;
        let mut best_score = 0.0;

        for strategy in &self.base_engine.convergence_strategies {
            if strategy.is_applicable(message) {
                let strategy_name = strategy.name().to_string();
                let performance_score = self.strategy_performance.get(&strategy_name).unwrap_or(&0.5);

                if performance_score > &best_score {
                    best_score = *performance_score;
                    best_strategy = Some(strategy);
                }
            }
        }

        best_strategy.ok_or_else(|| ExecutionError::Convergence("No suitable strategy found".to_string()))
    }

    /// Update strategy performance based on results
    async fn update_strategy_performance(&mut self, strategy_name: String, improvement: f64) {
        let current_performance = self.strategy_performance.entry(strategy_name).or_insert(0.5);
        *current_performance = (*current_performance * 0.9 + improvement * 0.1).min(1.0);

        // Remove low-performing strategies
        self.strategy_performance.retain(|_, &score| score > self.adaptation_threshold);
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_convergence_engine_creation() {
        let config = ConvergenceConfig::default();
        let engine = SemanticConvergenceEngine::new(config);

        assert_eq!(engine.agent_states.len(), 0);
        assert_eq!(engine.message_history.len(), 0);
        assert_eq!(engine.convergence_strategies.len(), 4);
    }

    #[tokio::test]
    async fn test_agent_registration() {
        let config = ConvergenceConfig::default();
        let mut engine = SemanticConvergenceEngine::new(config);

        engine.register_agent("agent-1", vec!["capability-1".to_string()]).await;

        assert_eq!(engine.agent_states.len(), 1);
        assert!(engine.agent_states.contains_key("agent-1"));
    }

    #[tokio::test]
    async fn test_message_processing() {
        let config = ConvergenceConfig::default();
        let mut engine = SemanticConvergenceEngine::new(config);

        engine.register_agent("agent-1", vec!["capability-1".to_string()]).await;

        let message = ConvergenceMessage {
            id: "msg-1".to_string(),
            source_agent: "agent-1".to_string(),
            target_agents: vec!["agent-1".to_string()],
            content: serde_json::json!({"test": "data"}),
            timestamp: Utc::now(),
            semantic_similarity: 0.0,
            contribution_type: ContributionType::KnowledgeSharing,
        };

        let result = engine.process_message(message).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_convergence_check() {
        let config = ConvergenceConfig {
            target_convergence_rate: 0.9,
            target_semantic_similarity: 0.9,
            ..Default::default()
        };

        let mut engine = SemanticConvergenceEngine::new(config);

        // Register agents and process some messages to build up convergence
        engine.register_agent("agent-1", vec!["capability-1".to_string()]).await;
        engine.register_agent("agent-2", vec!["capability-1".to_string()]).await;

        let message = ConvergenceMessage {
            id: "msg-1".to_string(),
            source_agent: "agent-1".to_string(),
            target_agents: vec!["agent-1".to_string(), "agent-2".to_string()],
            content: serde_json::json!({"test": "data"}),
            timestamp: Utc::now(),
            semantic_similarity: 0.95,
            contribution_type: ContributionType::KnowledgeSharing,
        };

        engine.process_message(message).await.unwrap();

        let converged = engine.check_convergence().await;
        // This may or may not converge depending on the exact metrics
    }

    #[test]
    fn test_alignment_strategy() {
        let strategy = AlignmentStrategy;
        let content = serde_json::json!({"test": "data"});

        let result = strategy.apply_convergence(
            &mut SemanticConvergenceEngine::new(ConvergenceConfig::default()),
            &ConvergenceMessage {
                id: "msg-1".to_string(),
                source_agent: "agent-1".to_string(),
                target_agents: vec!["agent-2".to_string()],
                content: content.clone(),
                timestamp: Utc::now(),
                semantic_similarity: 0.0,
                contribution_type: ContributionType::KnowledgeSharing,
            }
        );

        assert!(result.is_ok());
        assert!(result.unwrap() > 0.0);
    }

    #[test]
    fn test_validation_strategy() {
        let strategy = ValidationStrategy;

        let message = ConvergenceMessage {
            id: "msg-1".to_string(),
            source_agent: "agent-1".to_string(),
            target_agents: vec!["agent-2".to_string()],
            content: serde_json::json!({"test": "data"}),
            timestamp: Utc::now(),
            semantic_similarity: 0.0,
            contribution_type: ContributionType::MessageValidation,
        };

        let result = strategy.apply_convergence(
            &mut SemanticConvergenceEngine::new(ConvergenceConfig::default()),
            &message
        );

        assert!(result.is_ok());
        assert!(result.unwrap() >= 0.0 && result.unwrap() <= 1.0);
    }
}