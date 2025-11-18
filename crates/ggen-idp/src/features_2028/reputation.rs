/// Decentralized Reputation System for Agent Swarms
/// Trust scoring, behavior tracking, and peer-based reputation

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

/// Agent's reputation score
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReputationScore {
    pub score_id: Uuid,
    pub agent_id: Uuid,
    pub swarm_id: Uuid,
    pub overall_score: f64,               // 0.0-100.0
    pub trust_score: f64,                 // Reliability
    pub capability_score: f64,            // Skill level
    pub integrity_score: f64,             // Honesty
    pub collaboration_score: f64,         // Teamwork
    pub history_length_days: u32,
    pub recent_activity_weight: f64,      // Recent actions count more
    pub last_updated: DateTime<Utc>,
}

/// Reputation update event
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReputationEvent {
    pub event_id: Uuid,
    pub agent_id: Uuid,
    pub event_type: ReputationEventType,
    pub impact: i32,                      // -100 to +100
    pub reason: String,
    pub reporter_id: Option<Uuid>,        // Who reported this
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ReputationEventType {
    SuccessfulTransaction,
    FailedTransaction,
    MalformedMessage,
    TimeoutViolation,
    SecurityBreach,
    Collaboration,
    Excellence,
    Complaint,
    Appeal,
}

/// Reputation-based incentives
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ReputationIncentive {
    pub incentive_id: Uuid,
    pub agent_id: Uuid,
    pub incentive_type: IncentiveType,
    pub reward_amount: u64,
    pub triggered_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum IncentiveType {
    HighReputationBonus,
    LoyaltyReward,
    CollaborationBonus,
    SecurityBonus,
    ComplianceReward,
}

pub trait ReputationService: Send + Sync {
    /// Get agent reputation
    fn get_reputation(&self, agent_id: Uuid, swarm_id: Uuid) -> Result<ReputationScore, String>;

    /// Record reputation event
    fn record_event(&self, event: ReputationEvent) -> Result<(), String>;

    /// Get reputation history
    fn get_history(&self, agent_id: Uuid, days: u32) -> Result<Vec<ReputationEvent>, String>;
}
