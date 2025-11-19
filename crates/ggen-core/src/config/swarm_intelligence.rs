//! Swarm Intelligence Module
//!
//! Provides collective memory, consensus mechanisms, and worker coordination for the Hive Queen.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

/// Collective memory for swarm knowledge sharing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CollectiveMemory {
    /// Short-term memory (recent decisions)
    pub short_term: VecDeque<MemoryEntry>,

    /// Long-term memory (persistent knowledge)
    pub long_term: HashMap<String, Vec<MemoryEntry>>,

    /// Pattern library (learned behaviors)
    pub patterns: Vec<LearnedPattern>,

    /// Memory capacity limits
    pub max_short_term: usize,
    pub max_long_term_per_topic: usize,
}

/// A single memory entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryEntry {
    /// Unique identifier
    pub id: String,

    /// Topic/category
    pub topic: String,

    /// Memory content
    pub content: String,

    /// Timestamp
    pub timestamp: DateTime<Utc>,

    /// Confidence score (0.0 - 1.0)
    pub confidence: f32,

    /// Source agent ID
    pub source: String,

    /// Number of reinforcements
    pub reinforcements: u32,
}

/// Learned pattern from swarm experience
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LearnedPattern {
    /// Pattern identifier
    pub id: String,

    /// Pattern description
    pub description: String,

    /// Conditions that trigger this pattern
    pub conditions: Vec<String>,

    /// Recommended actions
    pub actions: Vec<String>,

    /// Success rate (0.0 - 1.0)
    pub success_rate: f32,

    /// Number of times applied
    pub application_count: u32,

    /// Last used timestamp
    pub last_used: DateTime<Utc>,
}

/// Worker coordination state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkerState {
    /// Worker ID
    pub worker_id: String,

    /// Current task
    pub current_task: Option<String>,

    /// Status
    pub status: AgentStatus,

    /// Health
    pub health: f32,

    /// Last heartbeat
    pub last_heartbeat: DateTime<Utc>,

    /// Assigned workload
    pub workload: Vec<String>,

    /// Completed tasks
    pub completed_tasks: u32,

    /// Failed tasks
    pub failed_tasks: u32,
}

/// Agent status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentStatus {
    /// Ready to accept work
    Idle,

    /// Currently processing
    Working,

    /// Waiting for dependencies
    Waiting,

    /// Error state
    Failed,

    /// Recovering from failure
    Recovering,

    /// Shutting down
    Shutdown,
}

/// Agent performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentPerformance {
    /// Average task completion time (seconds)
    pub avg_completion_time: f32,

    /// Success rate (0.0 - 1.0)
    pub success_rate: f32,

    /// Total tasks processed
    pub total_tasks: u32,

    /// Current load (0.0 - 1.0)
    pub current_load: f32,

    /// Quality score (0.0 - 1.0)
    pub quality_score: f32,
}

/// Inter-agent message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMessage {
    /// Message ID
    pub id: String,

    /// Sender agent ID
    pub from: String,

    /// Recipient agent ID (None for broadcast)
    pub to: Option<String>,

    /// Message type
    pub msg_type: MessageType,

    /// Message content
    pub content: String,

    /// Timestamp
    pub timestamp: DateTime<Utc>,

    /// Priority (1-10)
    pub priority: u8,

    /// Requires response
    pub requires_response: bool,
}

/// Message types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageType {
    /// Request for information
    Query,

    /// Response to query
    Response,

    /// Task assignment
    TaskAssignment,

    /// Task completion notification
    TaskComplete,

    /// Error notification
    Error,

    /// Heartbeat
    Heartbeat,

    /// Coordination signal
    Coordination,

    /// Emergency alert
    Alert,
}

/// Consensus voting system
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConsensusVoting {
    /// Voting topic
    pub topic: String,

    /// Proposals
    pub proposals: Vec<Proposal>,

    /// Votes by agent ID
    pub votes: HashMap<String, String>, // agent_id -> proposal_id

    /// Voting deadline
    pub deadline: DateTime<Utc>,

    /// Required quorum (0.0 - 1.0)
    pub quorum: f32,

    /// Consensus threshold (0.0 - 1.0)
    pub threshold: f32,

    /// Current status
    pub status: VotingStatus,
}

/// Voting proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Proposal {
    /// Proposal ID
    pub id: String,

    /// Proposal description
    pub description: String,

    /// Proposed by agent
    pub proposer: String,

    /// Vote count
    pub votes: u32,

    /// Confidence score
    pub confidence: f32,
}

/// Voting status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum VotingStatus {
    /// Collecting votes
    Open,

    /// Consensus reached
    Consensus,

    /// No consensus, needs tie-breaking
    Deadlock,

    /// Voting closed
    Closed,

    /// Cancelled
    Cancelled,
}

impl CollectiveMemory {
    /// Create new collective memory
    pub fn new() -> Self {
        Self {
            short_term: VecDeque::new(),
            long_term: HashMap::new(),
            patterns: Vec::new(),
            max_short_term: 100,
            max_long_term_per_topic: 50,
        }
    }

    /// Store a memory entry
    pub fn store(&mut self, entry: MemoryEntry) {
        // Add to short-term memory
        self.short_term.push_back(entry.clone());

        // Evict oldest if over capacity
        if self.short_term.len() > self.max_short_term {
            if let Some(old_entry) = self.short_term.pop_front() {
                // Promote to long-term if significant
                if old_entry.confidence >= 0.7 {
                    self.promote_to_long_term(old_entry);
                }
            }
        }

        // Also add to long-term if high confidence
        if entry.confidence >= 0.8 {
            self.promote_to_long_term(entry);
        }
    }

    /// Promote entry to long-term memory
    fn promote_to_long_term(&mut self, entry: MemoryEntry) {
        let topic_entries = self.long_term.entry(entry.topic.clone()).or_default();

        topic_entries.push(entry);

        // Evict lowest confidence entries if over capacity
        if topic_entries.len() > self.max_long_term_per_topic {
            topic_entries.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());
            topic_entries.truncate(self.max_long_term_per_topic);
        }
    }

    /// Retrieve memories by topic
    pub fn recall(&self, topic: &str) -> Vec<MemoryEntry> {
        let mut results = Vec::new();

        // Search short-term
        for entry in &self.short_term {
            if entry.topic == topic {
                results.push(entry.clone());
            }
        }

        // Search long-term
        if let Some(long_term_entries) = self.long_term.get(topic) {
            results.extend(long_term_entries.clone());
        }

        // Sort by confidence
        results.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());

        results
    }

    /// Learn a new pattern
    pub fn learn_pattern(&mut self, pattern: LearnedPattern) {
        self.patterns.push(pattern);
    }

    /// Find matching patterns
    pub fn find_patterns(&self, conditions: &[String]) -> Vec<&LearnedPattern> {
        self.patterns
            .iter()
            .filter(|p| {
                // Check if any condition matches
                p.conditions.iter().any(|c| conditions.contains(c))
            })
            .collect()
    }

    /// Reinforce a memory (increase confidence)
    pub fn reinforce(&mut self, memory_id: &str) {
        // Reinforce in short-term
        for entry in &mut self.short_term {
            if entry.id == memory_id {
                entry.reinforcements += 1;
                entry.confidence = (entry.confidence + 0.1).min(1.0);
            }
        }

        // Reinforce in long-term
        for entries in self.long_term.values_mut() {
            for entry in entries {
                if entry.id == memory_id {
                    entry.reinforcements += 1;
                    entry.confidence = (entry.confidence + 0.1).min(1.0);
                }
            }
        }
    }
}

impl ConsensusVoting {
    /// Create new consensus voting
    pub fn new(topic: String, quorum: f32, threshold: f32) -> Self {
        Self {
            topic,
            proposals: Vec::new(),
            votes: HashMap::new(),
            deadline: Utc::now() + Duration::minutes(5),
            quorum,
            threshold,
            status: VotingStatus::Open,
        }
    }

    /// Add a proposal
    pub fn add_proposal(&mut self, proposal: Proposal) {
        self.proposals.push(proposal);
    }

    /// Cast a vote
    pub fn vote(&mut self, agent_id: String, proposal_id: String) -> Result<(), String> {
        if self.status != VotingStatus::Open {
            return Err("Voting is closed".to_string());
        }

        if Utc::now() > self.deadline {
            self.status = VotingStatus::Closed;
            return Err("Voting deadline passed".to_string());
        }

        // Record vote
        self.votes.insert(agent_id, proposal_id.clone());

        // Update proposal vote count
        for proposal in &mut self.proposals {
            if proposal.id == proposal_id {
                proposal.votes += 1;
            }
        }

        Ok(())
    }

    /// Check for consensus
    pub fn check_consensus(&mut self, total_agents: usize) -> Option<String> {
        if self.status != VotingStatus::Open {
            return None;
        }

        // Check quorum
        let participation_rate = self.votes.len() as f32 / total_agents as f32;
        if participation_rate < self.quorum {
            return None; // Not enough votes yet
        }

        // Find winning proposal
        if let Some(winning_proposal) = self.proposals.iter().max_by_key(|p| p.votes) {
            let consensus_rate = winning_proposal.votes as f32 / self.votes.len() as f32;

            if consensus_rate >= self.threshold {
                self.status = VotingStatus::Consensus;
                return Some(winning_proposal.id.clone());
            }
        }

        // Check for deadlock
        if Utc::now() > self.deadline {
            self.status = VotingStatus::Deadlock;
        }

        None
    }

    /// Get winning proposal
    pub fn winner(&self) -> Option<&Proposal> {
        if self.status != VotingStatus::Consensus {
            return None;
        }

        self.proposals.iter().max_by_key(|p| p.votes)
    }
}

impl Default for CollectiveMemory {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for AgentPerformance {
    fn default() -> Self {
        Self {
            avg_completion_time: 0.0,
            success_rate: 1.0,
            total_tasks: 0,
            current_load: 0.0,
            quality_score: 1.0,
        }
    }
}

impl WorkerState {
    /// Check if worker is healthy
    pub fn is_healthy(&self) -> bool {
        self.health >= 0.7 && self.status != AgentStatus::Failed
    }

    /// Check if worker is available for work
    pub fn is_available(&self) -> bool {
        self.status == AgentStatus::Idle && self.is_healthy()
    }

    /// Update heartbeat
    pub fn heartbeat(&mut self) {
        self.last_heartbeat = Utc::now();
        self.health = (self.health + 0.1).min(1.0);
    }

    /// Mark task as complete
    pub fn complete_task(&mut self, task_id: &str) {
        self.completed_tasks += 1;
        self.workload.retain(|t| t != task_id);

        if self.current_task.as_ref() == Some(&task_id.to_string()) {
            self.current_task = None;
            self.status = AgentStatus::Idle;
        }
    }

    /// Mark task as failed
    pub fn fail_task(&mut self) {
        self.failed_tasks += 1;
        self.health = (self.health - 0.2).max(0.0);

        if self.health < 0.5 {
            self.status = AgentStatus::Failed;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_collective_memory_storage() {
        let mut memory = CollectiveMemory::new();

        let entry = MemoryEntry {
            id: "mem-1".to_string(),
            topic: "versioning".to_string(),
            content: "Prefer stable versions".to_string(),
            timestamp: Utc::now(),
            confidence: 0.9,
            source: "agent-1".to_string(),
            reinforcements: 0,
        };

        memory.store(entry);

        let recalled = memory.recall("versioning");
        assert_eq!(recalled.len(), 1);
        assert_eq!(recalled[0].content, "Prefer stable versions");
    }

    #[test]
    fn test_consensus_voting() {
        let mut voting = ConsensusVoting::new("composition".to_string(), 0.5, 0.66);

        voting.add_proposal(Proposal {
            id: "union".to_string(),
            description: "Use union composition".to_string(),
            proposer: "agent-1".to_string(),
            votes: 0,
            confidence: 0.8,
        });

        voting.add_proposal(Proposal {
            id: "intersection".to_string(),
            description: "Use intersection composition".to_string(),
            proposer: "agent-2".to_string(),
            votes: 0,
            confidence: 0.7,
        });

        // Cast votes
        voting
            .vote("agent-1".to_string(), "union".to_string())
            .unwrap();
        voting
            .vote("agent-2".to_string(), "union".to_string())
            .unwrap();
        voting
            .vote("agent-3".to_string(), "union".to_string())
            .unwrap();

        let winner = voting.check_consensus(3);
        assert!(winner.is_some());
        assert_eq!(winner.unwrap(), "union");
    }

    #[test]
    fn test_worker_health() {
        let mut worker = WorkerState {
            worker_id: "worker-1".to_string(),
            current_task: None,
            status: AgentStatus::Idle,
            health: 1.0,
            last_heartbeat: Utc::now(),
            workload: Vec::new(),
            completed_tasks: 0,
            failed_tasks: 0,
        };

        assert!(worker.is_healthy());
        assert!(worker.is_available());

        // Simulate failures
        worker.fail_task();
        worker.fail_task();
        worker.fail_task();

        assert!(!worker.is_healthy());
        assert_eq!(worker.status, AgentStatus::Failed);
    }
}
