/// Autonomous AI Agents for Swarms
/// LLM-powered agents with goals, constraints, and autonomous decision-making

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// AGENT GOALS & OBJECTIVES
// ============================================================================

/// Agent's objective in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentGoal {
    pub id: Uuid,
    pub agent_id: Uuid,
    pub goal_type: GoalType,
    pub description: String,
    pub priority: u8,                       // 0-255, higher = more important
    pub constraints: Vec<GoalConstraint>,
    pub success_criteria: SuccessCriteria,
    pub created_at: DateTime<Utc>,
    pub deadline: Option<DateTime<Utc>>,
    pub status: GoalStatus,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum GoalType {
    Issuing,                                // Issue credentials
    Verifying,                              // Verify credentials
    DataExchange,                           // Exchange data with other agents
    Consensus,                              // Reach consensus
    Learning,                               // Learn from interactions
    Compliance,                             // Maintain compliance
    RiskAssessment,                         // Assess risks
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GoalConstraint {
    pub constraint_type: String,            // CEL expression
    pub enforce_hard: bool,                 // Must not be violated
    pub penalty_if_violated: u32,          // Reputation penalty
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SuccessCriteria {
    pub metric: String,
    pub threshold: f64,
    pub measurement_period_secs: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum GoalStatus {
    Pending,
    Active,
    OnTrack,
    AtRisk,
    Completed,
    Failed,
    Abandoned,
}

// ============================================================================
// AGENT REASONING & DECISION-MAKING
// ============================================================================

/// Agent's reasoning process
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentReasoning {
    pub reasoning_id: Uuid,
    pub agent_id: Uuid,
    pub reasoning_type: ReasoningType,
    pub prompt: String,                     // What the agent is reasoning about
    pub context: serde_json::Value,        // Available facts/data
    pub reasoning_chain: Vec<ReasoningStep>,
    pub conclusion: String,                 // Final decision
    pub confidence: f64,                    // 0.0-1.0 confidence
    pub execution_time_ms: u32,
    pub model_used: String,                // "GPT-4", "Claude-3", etc
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ReasoningType {
    ChainOfThought,                        // Step-by-step reasoning
    TreeOfThoughts,                        // Multiple reasoning paths
    ReflectionCritique,                    // Self-evaluation
    CredentialVerification,                // Verify credentials
    RiskAssessment,                        // Assess transaction risk
    ConflictResolution,                    // Resolve disputes
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReasoningStep {
    pub step_number: u32,
    pub thought: String,
    pub action: Option<String>,
    pub observation: Option<String>,
}

/// Agent's decision
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentDecision {
    pub decision_id: Uuid,
    pub agent_id: Uuid,
    pub reasoning_id: Uuid,
    pub decision_type: DecisionType,
    pub choice: String,                     // What was decided
    pub reasoning: String,                  // Why this decision
    pub alternatives_considered: Vec<String>,
    pub expected_impact: serde_json::Value,
    pub confidence: f64,
    pub reversible: bool,                   // Can it be undone?
    pub created_at: DateTime<Utc>,
    pub executed: bool,
    pub executed_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DecisionType {
    IssueCredential,
    ApproveTransaction,
    InitiateVote,
    EscalateToHuman,
    RequestMoreInfo,
    RevokeCredential,
    RejectCredential,
}

// ============================================================================
// AGENT MEMORY & LEARNING
// ============================================================================

/// Agent's episodic memory (what happened)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentMemory {
    pub memory_id: Uuid,
    pub agent_id: Uuid,
    pub memory_type: MemoryType,
    pub event: String,                      // What happened
    pub context: serde_json::Value,        // When/where/why/with whom
    pub emotional_weight: f64,              // Importance (0-1)
    pub learned_from: bool,                 // Did agent learn from this?
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum MemoryType {
    Success,                                // Successfully completed task
    Failure,                                // Failed attempt
    Interaction,                            // Interaction with other agent
    Observation,                            // Observed something
    Teaching,                               // Learned from other agent
    Mistake,                                // Made an error
}

/// Agent's semantic memory (facts learned)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LearnedFact {
    pub fact_id: Uuid,
    pub agent_id: Uuid,
    pub fact: String,                       // CEL expression representing fact
    pub confidence: f64,                    // How sure agent is (0-1)
    pub source: FactSource,
    pub learned_at: DateTime<Utc>,
    pub used_count: u32,                   // Times this fact was used
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FactSource {
    Direct,                                 // Agent observed directly
    FromOtherAgent,                        // Another agent told it
    Deduced,                               // Agent deduced it
    Training,                              // From initial training
}

// ============================================================================
// AGENT COMMUNICATION & COLLABORATION
// ============================================================================

/// Request from one agent to another
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentRequest {
    pub request_id: Uuid,
    pub requester_id: Uuid,
    pub recipient_id: Uuid,
    pub request_type: RequestType,
    pub payload: serde_json::Value,
    pub deadline: Option<DateTime<Utc>>,
    pub priority: u8,
    pub response_required: bool,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RequestType {
    VerifyCredential,
    IssueCredential,
    ProvideAttestation,
    QueryData,
    ReachConsensus,
    EscalateDecision,
}

/// Response from agent to request
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentResponse {
    pub response_id: Uuid,
    pub request_id: Uuid,
    pub responder_id: Uuid,
    pub result: ResponseResult,
    pub data: serde_json::Value,
    pub reasoning: Option<String>,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ResponseResult {
    Success,
    Partial,
    Declined,
    Error,
    RequiresHumanInput,
}

// ============================================================================
// AGENT GOVERNANCE & CONSTRAINTS
// ============================================================================

/// Rules that constrain agent behavior
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentConstitution {
    pub constitution_id: Uuid,
    pub agent_id: Uuid,
    pub rules: Vec<AgentRule>,
    pub enforcement_level: EnforcementLevel,
    pub version: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentRule {
    pub rule_id: String,
    pub rule_name: String,
    pub rule_expression: String,           // CEL expression
    pub violation_penalty: u32,            // Reputation penalty
    pub enforcement: RuleEnforcement,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RuleEnforcement {
    Mandatory,                              // Must follow
    Strongly,                              // Should follow
    Advisory,                              // Nice to follow
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum EnforcementLevel {
    Strict,                                // Hard enforced
    Soft,                                  // Monitored and reported
    Advisory,                              // Just suggestions
}

/// Audit of agent decisions
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentAudit {
    pub audit_id: Uuid,
    pub agent_id: Uuid,
    pub decision_id: Uuid,
    pub auditor_id: Option<Uuid>,          // Human or other agent
    pub findings: AuditFinding,
    pub approved: bool,
    pub comments: Option<String>,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AuditFinding {
    Approved,
    ApprovedWithCaution,
    Rejected,
    RequiresRevision,
    Escalated,
}

// ============================================================================
// AGENT PERFORMANCE & IMPROVEMENT
// ============================================================================

/// Agent's performance metrics
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentPerformance {
    pub performance_id: Uuid,
    pub agent_id: Uuid,
    pub measurement_period: String,        // "daily", "weekly", "monthly"
    pub decisions_made: u32,
    pub successful_decisions: u32,
    pub rejected_decisions: u32,
    pub success_rate: f64,                 // Percentage
    pub avg_reasoning_time_ms: f64,
    pub goals_achieved: u32,
    pub goal_achievement_rate: f64,
    pub collaboration_score: f64,
    pub measured_at: DateTime<Utc>,
}

/// Agent improvement suggestion
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentImprovement {
    pub improvement_id: Uuid,
    pub agent_id: Uuid,
    pub improvement_type: ImprovementType,
    pub suggestion: String,
    pub expected_impact: f64,              // Expected performance increase
    pub implementation_cost: u32,          // Resources needed
    pub dependencies: Vec<String>,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ImprovementType {
    TrainingData,                          // Agent needs more training
    CapabilityUpgrade,                     // Add new capability
    MemoryOptimization,                    // Clean up memory
    ParameterTuning,                       // Adjust LLM parameters
    CollaborationStrategy,                 // Improve teamwork
}

// ============================================================================
// LLM INTEGRATION FOR AGENTS
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentLLMConfig {
    pub config_id: Uuid,
    pub agent_id: Uuid,
    pub model: String,                     // "gpt-4", "claude-3-opus", etc
    pub temperature: f32,                  // 0.0-1.0 (creativity)
    pub max_tokens: u32,
    pub system_prompt: String,
    pub reasoning_budget: u32,            // Max steps in reasoning
    pub context_window_size: u32,
    pub tools: Vec<LLMTool>,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LLMTool {
    pub tool_name: String,
    pub tool_description: String,
    pub parameters: serde_json::Value,    // JSON Schema
}

// ============================================================================
// AGENT SERVICE INTERFACE
// ============================================================================

pub trait AutonomousAgentService: Send + Sync {
    /// Create reasoning for decision
    fn reason(
        &self,
        agent_id: Uuid,
        query: &str,
        context: serde_json::Value,
    ) -> Result<AgentReasoning, String>;

    /// Make autonomous decision
    fn decide(
        &self,
        agent_id: Uuid,
        reasoning_id: Uuid,
    ) -> Result<AgentDecision, String>;

    /// Store memory of event
    fn remember(
        &self,
        agent_id: Uuid,
        memory: AgentMemory,
    ) -> Result<(), String>;

    /// Learn fact from interaction
    fn learn(
        &self,
        agent_id: Uuid,
        fact: LearnedFact,
    ) -> Result<(), String>;

    /// Retrieve relevant memories
    fn recall(
        &self,
        agent_id: Uuid,
        query: &str,
    ) -> Result<Vec<AgentMemory>, String>;

    /// Send request to another agent
    fn send_request(
        &self,
        request: AgentRequest,
    ) -> Result<Uuid, String>;

    /// Respond to request
    fn send_response(
        &self,
        response: AgentResponse,
    ) -> Result<(), String>;

    /// Get agent performance
    fn get_performance(
        &self,
        agent_id: Uuid,
        period: &str,
    ) -> Result<AgentPerformance, String>;
}
