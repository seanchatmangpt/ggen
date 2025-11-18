/// 2028 AI Agent Swarm - Complete Identity & Coordination System
/// For autonomous multi-agent interactions with advanced cryptography
///
/// Features:
/// - Decentralized Identity (DIDs) with W3C standards
/// - Zero-Knowledge Proofs for privacy-preserving verification
/// - Autonomous AI Agents with reasoning and learning
/// - Quantum-Safe Cryptography (post-quantum algorithms)
/// - Multi-Chain Blockchain Integration
/// - Swarm Coordination & Consensus (PBFT, Raft, Gossip)
/// - Advanced Credentials (selective disclosure, chaining, ABC)
/// - Biometric Credentials (fingerprint, facial, behavioral)
/// - Decentralized Reputation System
///
/// All designed for AI agent swarms interacting autonomously

pub mod did;
pub mod zk;
pub mod agents;
pub mod quantum;
pub mod swarm;
pub mod credentials;
pub mod blockchain;
pub mod biometric;
pub mod reputation;

use uuid::Uuid;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

// ============================================================================
// AGENT SWARM IDENTITIES
// ============================================================================

/// Unique agent identity in the swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentIdentity {
    pub id: Uuid,
    pub did: String,                      // Decentralized Identifier
    pub swarm_id: Uuid,                   // Which swarm agent belongs to
    pub agent_type: AgentType,
    pub public_key: String,               // Ed25519 public key
    pub capabilities: Vec<String>,        // What this agent can do
    pub tier: AgentTier,                  // Trust/capability tier
    pub reputation_score: f64,            // 0.0 to 100.0
    pub is_active: bool,
    pub created_at: DateTime<Utc>,
    pub last_heartbeat: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum AgentType {
    CredentialIssuer,
    CredentialVerifier,
    TrustAnchor,
    DelegatedSigner,
    RiskAssessor,
    CapabilityBroker,
    OracleProvider,
    ResourceManager,
    PolicyEnforcer,
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd)]
pub enum AgentTier {
    Bronze,      // Basic capabilities, limited trust
    Silver,      // Intermediate capabilities, moderate trust
    Gold,        // Advanced capabilities, high trust
    Platinum,    // All capabilities, maximum trust
    Root,        // System trust anchor
}

// ============================================================================
// SWARM COORDINATION
// ============================================================================

/// Agent Swarm - collective of autonomous agents
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentSwarm {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub leader_id: Uuid,                 // Lead coordinator agent
    pub members: Vec<Uuid>,              // Agent IDs in swarm
    pub consensus_threshold: f64,        // % agreement needed (0.5-1.0)
    pub governance_model: GovernanceModel,
    pub policies: Vec<SwarmPolicy>,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum GovernanceModel {
    Hierarchical,                        // Leader-based decisions
    Democratic,                          // Voting-based (consensus_threshold)
    Meritocratic,                        // Reputation-weighted voting
    ByzantineFaultTolerant,             // BFT consensus (2f+1)
    LiquidDemocracy,                    // Delegated voting
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SwarmPolicy {
    pub id: Uuid,
    pub name: String,
    pub rule: String,                   // CEL expression
    pub enforcement: PolicyEnforcement,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum PolicyEnforcement {
    Advisory,                           // Suggest but don't enforce
    Soft,                               // Discourage but allow
    Strict,                             // Enforce strictly
    Fatal,                              // Expel agent if violated
}

// ============================================================================
// AGENT MESSAGES & CONSENSUS
// ============================================================================

/// Message between agents in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentMessage {
    pub id: Uuid,
    pub from: Uuid,                     // Sender agent ID
    pub to: Vec<Uuid>,                  // Recipients (empty = broadcast)
    pub swarm_id: Uuid,
    pub message_type: MessageType,
    pub payload: serde_json::Value,
    pub signatures: Vec<String>,        // Multi-sig capability
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub priority: u8,                   // 0-255, higher = more important
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum MessageType {
    CredentialRequest,
    CredentialIssue,
    CredentialPresentation,
    ProofChallenge,
    ProofResponse,
    VoteProposal,
    VoteResponse,
    ConsensusPoll,
    ConsensusResult,
    CapabilityQuery,
    CapabilityResponse,
    OracleRequest,
    OracleResponse,
    AlertNotification,
}

/// Consensus proposal in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConsensusProposal {
    pub id: Uuid,
    pub swarm_id: Uuid,
    pub proposer: Uuid,
    pub proposal_type: ProposalType,
    pub details: serde_json::Value,
    pub votes: Vec<AgentVote>,
    pub created_at: DateTime<Utc>,
    pub deadline: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProposalType {
    AddAgent,
    RemoveAgent,
    UpdatePolicy,
    ChangeGovernance,
    MajorTransaction,
    SecurityIncident,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentVote {
    pub voter_id: Uuid,
    pub vote: VoteChoice,
    pub weight: f64,                    // Based on reputation/tier
    pub reasoning: Option<String>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum VoteChoice {
    Yes,
    No,
    Abstain,
}

// ============================================================================
// AGENT CAPABILITIES & DISCOVERY
// ============================================================================

/// Capability declaration
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Capability {
    pub id: String,
    pub name: String,
    pub description: String,
    pub required_tier: AgentTier,
    pub input_schema: serde_json::Value,  // JSON Schema
    pub output_schema: serde_json::Value,
    pub compute_cost: u32,               // In credits
    pub gas_limit: u64,
    pub timeout_secs: u32,
    pub version: String,
}

/// Service discovery entry
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ServiceRegistry {
    pub id: Uuid,
    pub agent_id: Uuid,
    pub service_name: String,
    pub service_version: String,
    pub endpoint: String,                // HTTP/gRPC endpoint
    pub capabilities: Vec<String>,
    pub sla: ServiceLevelAgreement,
    pub is_available: bool,
    pub last_health_check: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ServiceLevelAgreement {
    pub availability_percent: f64,      // 99.9 = 99.9%
    pub max_latency_ms: u32,
    pub max_error_rate: f64,
    pub support_level: SupportLevel,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SupportLevel {
    Community,
    Standard,
    Premium,
    Enterprise,
}

// ============================================================================
// REPUTATION & TRUST
// ============================================================================

/// Agent reputation in swarm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentReputation {
    pub agent_id: Uuid,
    pub swarm_id: Uuid,
    pub overall_score: f64,             // 0-100
    pub trust_score: f64,               // 0-100 (reliability)
    pub capability_score: f64,          // 0-100 (skill level)
    pub integrity_score: f64,           // 0-100 (honesty)
    pub collaboration_score: f64,       // 0-100 (teamwork)
    pub incidents: Vec<Incident>,
    pub total_interactions: u64,
    pub successful_interactions: u64,
    pub failed_interactions: u64,
    pub last_updated: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Incident {
    pub id: Uuid,
    pub incident_type: IncidentType,
    pub severity: SeverityLevel,
    pub description: String,
    pub impact_score: f64,              // Negative impact on reputation
    pub timestamp: DateTime<Utc>,
    pub resolved: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum IncidentType {
    FailedTransaction,
    MalformedMessage,
    TimeoutViolation,
    SecurityBreach,
    DataCorruption,
    UnauthorizedAccess,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SeverityLevel {
    Low,
    Medium,
    High,
    Critical,
    Fatal,
}

// ============================================================================
// VERIFIABLE CREDENTIALS FOR AGENTS
// ============================================================================

/// Credential issued to/about an agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentCredential {
    pub id: Uuid,
    pub issuer_id: Uuid,               // Agent that issued
    pub subject_id: Uuid,              // Agent that received
    pub credential_type: CredentialType,
    pub claims: Vec<Claim>,
    pub issued_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub proof: CredentialProof,
    pub revocation_id: Option<String>, // For revocation
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialType {
    CapabilityCertificate,             // Proves capability
    TierCertificate,                   // Proves tier level
    SecurityClearance,                 // Proves access level
    ServiceEndorsement,                // Service quality promise
    ComplianceCertificate,             // Compliance with standards
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Claim {
    pub key: String,
    pub value: serde_json::Value,
    pub proof_required: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialProof {
    pub proof_type: String,            // "BbsBlsSignature2020", "Ed25519Signature2020", "ZKProof"
    pub proof_value: String,           // Signature or proof bytes
    pub verification_method: String,   // Which key to use for verification
}

// ============================================================================
// DELEGATED SIGNING FOR AGENTS
// ============================================================================

/// Delegation of signing authority
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SigningDelegation {
    pub id: Uuid,
    pub delegator: Uuid,               // Agent granting authority
    pub delegate: Uuid,                // Agent receiving authority
    pub swarm_id: Uuid,
    pub signing_keys: Vec<String>,     // Which keys can be used
    pub max_transaction_value: u64,    // If None, unlimited
    pub operations: Vec<String>,       // Which operations allowed
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub active: bool,
}

/// Signed transaction by agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentSignedTransaction {
    pub id: Uuid,
    pub signer_id: Uuid,
    pub transaction_type: String,
    pub data: serde_json::Value,
    pub signature: String,             // Ed25519 or BLS signature
    pub signers: Vec<Uuid>,           // Multi-sig: who signed
    pub delegation_id: Option<Uuid>,   // If using delegation
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// TRANSACTION & SETTLEMENT
// ============================================================================

/// Transaction between agents
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentTransaction {
    pub id: Uuid,
    pub swarm_id: Uuid,
    pub from: Uuid,
    pub to: Vec<Uuid>,                // Multiple recipients
    pub amount: u64,
    pub currency: String,             // "CREDITS", "ETH", etc
    pub tx_type: TransactionType,
    pub data: serde_json::Value,
    pub signatures: Vec<AgentSignature>,
    pub confirmations: u32,
    pub finalized: bool,
    pub created_at: DateTime<Utc>,
    pub settled_at: Option<DateTime<Utc>>,
    pub blockchain_tx_id: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TransactionType {
    Payment,
    ContractExecution,
    CredentialIssue,
    TokenTransfer,
    DataExchange,
    ServicePayment,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentSignature {
    pub signer_id: Uuid,
    pub signature: String,
    pub timestamp: DateTime<Utc>,
}

// ============================================================================
// 2028 FEATURE FLAGS & EXPERIMENTAL
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FeatureFlags2028 {
    pub decentralized_identity_enabled: bool,
    pub zero_knowledge_proofs_enabled: bool,
    pub autonomous_agents_enabled: bool,
    pub quantum_safe_crypto_enabled: bool,
    pub blockchain_integration_enabled: bool,
    pub biometric_credentials_enabled: bool,
    pub reputation_system_enabled: bool,
    pub privacy_preserving_enabled: bool,
    pub cross_chain_enabled: bool,
    pub ai_powered_issuance_enabled: bool,
    pub swarm_consensus_enabled: bool,
    pub byzantine_fault_tolerance_enabled: bool,
}

impl Default for FeatureFlags2028 {
    fn default() -> Self {
        Self {
            decentralized_identity_enabled: true,
            zero_knowledge_proofs_enabled: true,
            autonomous_agents_enabled: true,
            quantum_safe_crypto_enabled: true,
            blockchain_integration_enabled: true,
            biometric_credentials_enabled: true,
            reputation_system_enabled: true,
            privacy_preserving_enabled: true,
            cross_chain_enabled: true,
            ai_powered_issuance_enabled: true,
            swarm_consensus_enabled: true,
            byzantine_fault_tolerance_enabled: true,
        }
    }
}
