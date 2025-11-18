/// Swarm Coordination & Consensus for AI Agent Networks
/// Multi-agent coordination, Byzantine fault tolerance, and distributed consensus

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;
use std::collections::HashMap;

// ============================================================================
// SWARM CONSENSUS ALGORITHMS
// ============================================================================

/// Swarm consensus result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmConsensusResult {
    pub consensus_id: Uuid,
    pub swarm_id: Uuid,
    pub proposal_id: Uuid,
    pub consensus_algorithm: ConsensusAlgorithm,
    pub total_agents: u32,
    pub participating_agents: u32,
    pub consensus_reached: bool,
    pub decision: Option<String>,
    pub vote_distribution: HashMap<String, u32>,
    pub consensus_threshold_met: bool,
    pub started_at: DateTime<Utc>,
    pub completed_at: DateTime<Utc>,
    pub execution_time_ms: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ConsensusAlgorithm {
    PBFT,                                  // Practical Byzantine Fault Tolerance
    Raft,                                  // Raft consensus
    Paxos,                                 // Paxos protocol
    CRDT,                                  // Conflict-free Replicated Data Types
    Gossip,                                // Gossip protocol
    Proof,                                 // Proof-of-stake style
    Weighted,                              // Weighted by reputation/stake
    Lottery,                               // Random selection (Proof-of-Work style)
}

// ============================================================================
// BYZANTINE FAULT TOLERANCE (BFT)
// ============================================================================

/// Agent state in PBFT consensus
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BFTAgentState {
    pub agent_id: Uuid,
    pub node_id: u32,                      // Unique node number
    pub is_primary: bool,                  // Is this agent the leader?
    pub view_number: u64,                  // Current consensus round
    pub log: Vec<BFTLogEntry>,
    pub commited_entries: u32,             // Entries permanently agreed upon
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BFTLogEntry {
    pub sequence_number: u64,
    pub timestamp: DateTime<Utc>,
    pub request: String,
    pub digest: String,                    // Hash of request
    pub status: BFTLogStatus,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BFTLogStatus {
    Preprepared,
    Prepared,
    Committed,
}

/// BFT View Change (new leader election)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BFTViewChange {
    pub view_change_id: Uuid,
    pub old_view: u64,
    pub new_view: u64,
    pub reason: String,                    // Why view change needed
    pub initiator: Uuid,
    pub participants: Vec<Uuid>,
    pub new_primary: Uuid,
    pub completed_at: DateTime<Utc>,
}

// ============================================================================
// GOSSIP PROTOCOL
// ============================================================================

/// Gossip message between agents
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GossipMessage {
    pub message_id: Uuid,
    pub round: u32,
    pub source_agent: Uuid,
    pub data: serde_json::Value,
    pub sequence_number: u64,             // For ordering
    pub hops: u32,
    pub max_hops: u32,
    pub participants_reached: u32,
    pub timestamp: DateTime<Utc>,
}

/// Gossip state (eventual consistency)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GossipState {
    pub state_id: Uuid,
    pub swarm_id: Uuid,
    pub key: String,
    pub value: serde_json::Value,
    pub version: u64,
    pub timestamp: DateTime<Utc>,
    pub vector_clock: HashMap<Uuid, u64>,  // For causal ordering
}

// ============================================================================
// RAFT CONSENSUS
// ============================================================================

/// Agent's Raft state
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RaftAgentState {
    pub agent_id: Uuid,
    pub role: RaftRole,
    pub current_term: u64,
    pub voted_for: Option<Uuid>,
    pub log: Vec<RaftLogEntry>,
    pub commit_index: u32,
    pub last_applied: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RaftRole {
    Follower,
    Candidate,
    Leader,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RaftLogEntry {
    pub term: u64,
    pub index: u32,
    pub command: String,
    pub timestamp: DateTime<Utc>,
}

/// Raft election
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RaftElection {
    pub election_id: Uuid,
    pub term: u64,
    pub candidate_id: Uuid,
    pub votes_received: u32,
    pub total_voters: u32,
    pub election_won: bool,
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
}

// ============================================================================
// CRDT (CONFLICT-FREE REPLICATED DATA TYPES)
// ============================================================================

/// CRDT data structure (automatically resolves conflicts)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CRDTData {
    pub crdt_id: Uuid,
    pub swarm_id: Uuid,
    pub data_type: CRDTType,
    pub value: serde_json::Value,
    pub metadata: CRDTMetadata,
    pub version: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CRDTType {
    GCounter,                              // Grow-only counter
    PNCounter,                             // Positive-Negative counter
    GSet,                                  // Grow-only set
    TwoPhaseSet,                           // Two-phase set (add/remove)
    LWWRegister,                           // Last-write-wins register
    Map,                                   // CRDT map
    Text,                                  // Collaborative text editing
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CRDTMetadata {
    pub actor_id: Uuid,                    // Agent that created/modified
    pub timestamp: DateTime<Utc>,
    pub clock: HashMap<Uuid, u64>,        // Happened-before clock
}

// ============================================================================
// DISTRIBUTED CONSENSUS PROPOSALS
// ============================================================================

/// Proposal for swarm to reach consensus on
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmConsensusProposal {
    pub proposal_id: Uuid,
    pub swarm_id: Uuid,
    pub proposer_id: Uuid,
    pub proposal_type: ProposalType,
    pub details: serde_json::Value,
    pub required_consensus: f64,           // % agreement needed (0.5-1.0)
    pub timeout_secs: u32,
    pub votes: HashMap<Uuid, ConsensusVote>,
    pub status: ProposalStatus,
    pub created_at: DateTime<Utc>,
    pub decided_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProposalType {
    CredentialIssue,
    CredentialRevoke,
    AddAgent,
    RemoveAgent,
    UpdatePolicy,
    EmergencyStop,
    FundsTransfer,
    UpgradeContract,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConsensusVote {
    pub voter_id: Uuid,
    pub vote: VoteChoice,
    pub weight: f64,                       // Reputation-weighted
    pub reasoning: Option<String>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum VoteChoice {
    Approve,
    Reject,
    Abstain,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProposalStatus {
    Proposed,
    Voting,
    Decided,
    Executed,
    Failed,
    Cancelled,
}

// ============================================================================
// SWARM TOPOLOGY & COMMUNICATION
// ============================================================================

/// Network topology of swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmTopology {
    pub topology_id: Uuid,
    pub swarm_id: Uuid,
    pub topology_type: TopologyType,
    pub nodes: Vec<Uuid>,                  // Agent IDs
    pub edges: Vec<SwarmEdge>,            // Communication links
    pub latency_matrix: HashMap<String, u32>, // Agent pair latencies
    pub bandwidth_capacity: HashMap<String, u64>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TopologyType {
    FullMesh,                              // Every agent talks to every other
    Star,                                  // Central hub, spokes to agents
    Ring,                                  // Agents form ring
    Tree,                                  // Hierarchical tree
    RandomGraph,                           // Random connected graph
    DynamicTopology,                       // Topology changes over time
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmEdge {
    pub from_agent: Uuid,
    pub to_agent: Uuid,
    pub bandwidth_mbps: u32,
    pub latency_ms: u32,
    pub is_active: bool,
}

// ============================================================================
// FAILURE DETECTION & RECOVERY
// ============================================================================

/// Monitor agent health in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmHealthMonitor {
    pub monitor_id: Uuid,
    pub swarm_id: Uuid,
    pub heartbeat_interval_secs: u32,
    pub failure_detection_threshold: u32, // Miss N heartbeats = failed
    pub monitored_agents: HashMap<Uuid, AgentHealthStatus>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentHealthStatus {
    pub agent_id: Uuid,
    pub is_alive: bool,
    pub last_heartbeat: DateTime<Utc>,
    pub missed_heartbeats: u32,
    pub failure_suspected_at: Option<DateTime<Utc>>,
    pub failure_confirmed_at: Option<DateTime<Utc>>,
}

/// Recovery of failed agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentRecovery {
    pub recovery_id: Uuid,
    pub failed_agent_id: Uuid,
    pub failure_time: DateTime<Utc>,
    pub recovery_time: DateTime<Utc>,
    pub recovery_duration_secs: u32,
    pub state_synchronized: bool,
    pub data_loss: Option<String>,
}

// ============================================================================
// SWARM SYNCHRONIZATION
// ============================================================================

/// Synchronize agent state in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmStateSync {
    pub sync_id: Uuid,
    pub swarm_id: Uuid,
    pub source_agent: Uuid,
    pub target_agent: Uuid,
    pub state_version: u64,
    pub data: serde_json::Value,
    pub sync_time_ms: u32,
    pub successful: bool,
}

// ============================================================================
// SWARM COORDINATION SERVICE
// ============================================================================

pub trait SwarmCoordinationService: Send + Sync {
    /// Start consensus process
    fn start_consensus(
        &self,
        swarm_id: Uuid,
        proposal: SwarmConsensusProposal,
    ) -> Result<Uuid, String>;

    /// Vote on proposal
    fn vote(
        &self,
        proposal_id: Uuid,
        voter_id: Uuid,
        choice: VoteChoice,
        reasoning: Option<String>,
    ) -> Result<(), String>;

    /// Get consensus result
    fn get_consensus_result(
        &self,
        consensus_id: Uuid,
    ) -> Result<SwarmConsensusResult, String>;

    /// Broadcast message to swarm
    fn broadcast_message(
        &self,
        swarm_id: Uuid,
        from_agent: Uuid,
        message: serde_json::Value,
    ) -> Result<Uuid, String>;

    /// Elect new leader (for Raft/PBFT)
    fn initiate_election(
        &self,
        swarm_id: Uuid,
        candidate_id: Uuid,
    ) -> Result<Uuid, String>;

    /// Synchronize state
    fn sync_state(
        &self,
        source_agent: Uuid,
        target_agent: Uuid,
        state: serde_json::Value,
    ) -> Result<(), String>;

    /// Get swarm health status
    fn get_swarm_health(
        &self,
        swarm_id: Uuid,
    ) -> Result<SwarmHealthStatus, String>;

    /// Handle agent failure
    fn handle_agent_failure(
        &self,
        agent_id: Uuid,
        swarm_id: Uuid,
    ) -> Result<AgentRecovery, String>;
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmHealthStatus {
    pub swarm_id: Uuid,
    pub total_agents: u32,
    pub alive_agents: u32,
    pub failed_agents: u32,
    pub consensus_possible: bool,
    pub leader_active: bool,
}
