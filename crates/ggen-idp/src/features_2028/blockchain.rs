/// Blockchain Integration for Agent Swarm Credentials
/// Multi-chain credential registration, verification, and on-chain governance

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// BLOCKCHAIN NETWORKS FOR AGENTS
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum BlockchainNetwork {
    Ethereum {
        chain_id: u32,              // 1=mainnet, 5=goerli, 11155111=sepolia
        network_name: String,
    },
    Polygon {
        chain_id: u32,              // 137=mainnet, 80001=mumbai
        network_name: String,
    },
    Solana {
        cluster: String,            // "mainnet-beta", "devnet", "testnet"
    },
    Arbitrum {
        chain_id: u32,              // 42161=mainnet, 421613=goerli
    },
    Optimism {
        chain_id: u32,              // 10=mainnet, 420=goerli
    },
    Cosmos {
        chain_id: String,
    },
    Cardano {
        network: String,            // "mainnet", "testnet"
    },
}

impl BlockchainNetwork {
    /// Get RPC endpoint
    pub fn rpc_endpoint(&self) -> String {
        match self {
            BlockchainNetwork::Ethereum { chain_id, .. } => {
                if *chain_id == 1 {
                    "https://eth.blockscout.com".to_string()
                } else {
                    "https://goerli.infura.io/v3/YOUR_KEY".to_string()
                }
            }
            BlockchainNetwork::Polygon { chain_id, .. } => {
                if *chain_id == 137 {
                    "https://polygon-rpc.com/".to_string()
                } else {
                    "https://rpc-mumbai.maticvigil.com".to_string()
                }
            }
            BlockchainNetwork::Solana { cluster } => {
                format!("https://api.{}.solana.com", cluster)
            }
            _ => String::new(),
        }
    }
}

// ============================================================================
// ON-CHAIN CREDENTIALS
// ============================================================================

/// Credential registered on blockchain
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OnChainCredential {
    pub credential_id: String,
    pub blockchain: BlockchainNetwork,
    pub contract_address: String,          // Smart contract storing credential
    pub token_id: Option<String>,          // For NFT-based credentials
    pub issuer_address: String,            // Blockchain address of issuer
    pub subject_address: String,           // Blockchain address of subject
    pub credential_hash: String,           // Hash of credential content
    pub issuer_signature: String,
    pub blockchain_tx_id: String,          // Transaction hash
    pub block_number: u64,
    pub timestamp: DateTime<Utc>,
    pub status: OnChainCredentialStatus,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OnChainCredentialStatus {
    Pending,                               // Waiting for blockchain confirmation
    Confirmed,                             // On-chain
    Revoked,                               // Revocation transaction mined
    Suspended,                             // Temporarily suspended
    Expired,                               // Expired according to rules
}

/// Credential Smart Contract
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialSmartContract {
    pub contract_id: Uuid,
    pub contract_address: String,
    pub blockchain: BlockchainNetwork,
    pub contract_type: CredentialContractType,
    pub issuer_address: String,
    pub is_upgradeable: bool,
    pub deployment_tx: String,
    pub deployment_block: u64,
    pub abi: serde_json::Value,            // Contract ABI
    pub deployed_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialContractType {
    ERC721,                                // NFT-based credentials
    ERC1155,                               // Multi-token credentials
    Custom,                                // Custom credential contract
}

// ============================================================================
// CROSS-CHAIN CREDENTIALS
// ============================================================================

/// Credential valid across multiple blockchains
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CrossChainCredential {
    pub credential_id: Uuid,
    pub primary_blockchain: BlockchainNetwork,
    pub replicated_on: Vec<BlockchainNetwork>,
    pub verification_hash: String,         // Hash for cross-chain verification
    pub bridge_addresses: Vec<String>,     // Addresses on each chain
    pub issued_at: DateTime<Utc>,
    pub last_synced: DateTime<Utc>,
}

/// Cross-chain credential bridge
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialBridge {
    pub bridge_id: Uuid,
    pub source_blockchain: BlockchainNetwork,
    pub target_blockchain: BlockchainNetwork,
    pub bridge_contract: String,
    pub is_operational: bool,
    pub supported_credentials: Vec<String>,
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// BLOCKCHAIN WALLETS FOR AGENTS
// ============================================================================

/// Agent's blockchain wallet
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgentBlockchainWallet {
    pub wallet_id: Uuid,
    pub agent_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub address: String,                   // Public address
    pub private_key_encrypted: String,     // Encrypted private key
    pub key_derivation_path: String,       // For HD wallets
    pub balance: String,                   // Current balance
    pub nonce: u64,                        // For transaction ordering
    pub created_at: DateTime<Utc>,
    pub last_transaction: Option<DateTime<Utc>>,
}

// ============================================================================
// BLOCKCHAIN TRANSACTIONS FOR AGENTS
// ============================================================================

/// Transaction for credential operations
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BlockchainCredentialTransaction {
    pub tx_id: Uuid,
    pub agent_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub tx_hash: String,                   // Blockchain transaction hash
    pub from_address: String,
    pub to_address: String,
    pub transaction_type: CredentialTxType,
    pub data: String,                      // Encoded transaction data
    pub gas_limit: u64,
    pub gas_price: String,                 // In wei/atomic units
    pub gas_used: Option<u64>,
    pub status: TransactionStatus,
    pub confirmations: u32,
    pub created_at: DateTime<Utc>,
    pub mined_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialTxType {
    IssueCredential,
    RevokeCredential,
    TransferCredential,
    UpdateCredential,
    BatchIssue,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TransactionStatus {
    Pending,
    Confirmed,
    Failed,
    Replaced,
}

// ============================================================================
// ON-CHAIN GOVERNANCE
// ============================================================================

/// Governance proposal on-chain
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OnChainGovernanceProposal {
    pub proposal_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub governance_contract: String,
    pub proposer_address: String,
    pub proposal_type: ProposalType,
    pub description: String,
    pub voting_period_blocks: u64,
    pub required_quorum: f64,              // % of votes
    pub start_block: u64,
    pub end_block: u64,
    pub votes_for: u64,
    pub votes_against: u64,
    pub votes_abstain: u64,
    pub status: ProposalStatus,
    pub execution_tx: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProposalType {
    AddAgent,
    RemoveAgent,
    UpdatePolicy,
    ChangeFeeStructure,
    EmergencyShutdown,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProposalStatus {
    Pending,
    Active,
    Succeeded,
    Defeated,
    Cancelled,
    Executed,
}

// ============================================================================
// CREDENTIAL REGISTRY
// ============================================================================

/// Registry of all credentials issued by agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialRegistry {
    pub registry_id: Uuid,
    pub issuer_agent_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub registry_contract: String,
    pub total_issued: u64,
    pub total_revoked: u64,
    pub total_active: u64,
    pub created_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
}

/// Entry in credential registry
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RegistryEntry {
    pub entry_id: Uuid,
    pub registry_id: Uuid,
    pub credential_id: String,
    pub subject_address: String,
    pub credential_hash: String,
    pub issuer_signature: String,
    pub added_at: DateTime<Utc>,
    pub revoked_at: Option<DateTime<Utc>>,
}

// ============================================================================
// VERIFIABLE ON-CHAIN TIMESTAMPS
// ============================================================================

/// Credential with blockchain timestamp proof
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TimestampedCredential {
    pub credential_id: String,
    pub blockchain: BlockchainNetwork,
    pub tx_hash: String,
    pub block_hash: String,
    pub block_number: u64,
    pub timestamp: DateTime<Utc>,         // Block timestamp
    pub merkle_root: String,               // Block merkle root
    pub timestamp_proof: String,           // Proof of inclusion
}

// ============================================================================
// DECENTRALIZED ORACLE INTEGRATION
// ============================================================================

/// Oracle providing external data for on-chain credentials
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialOracle {
    pub oracle_id: Uuid,
    pub agent_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub contract_address: String,
    pub oracle_type: OracleType,
    pub data_sources: Vec<String>,
    pub update_frequency: String,          // "hourly", "daily", etc
    pub is_operational: bool,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OracleType {
    CredentialVerifier,                    // Verifies credentials off-chain
    AgeVerifier,                           // Age verification
    GeolocationVerifier,                   // Location data
    ComplianceChecker,                     // Compliance status
}

/// Oracle request for credential data
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OracleRequest {
    pub request_id: Uuid,
    pub oracle_id: Uuid,
    pub blockchain: BlockchainNetwork,
    pub credential_id: String,
    pub query: String,                     // What data is needed
    pub callback_contract: String,
    pub callback_function: String,
    pub status: OracleRequestStatus,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum OracleRequestStatus {
    Pending,
    Processing,
    Fulfilled,
    Failed,
}

// ============================================================================
// BLOCKCHAIN INTEGRATION SERVICE
// ============================================================================

pub trait BlockchainIntegrationService: Send + Sync {
    /// Issue credential on blockchain
    fn issue_on_chain(
        &self,
        credential_id: &str,
        issuer_agent: &str,
        subject_address: &str,
        blockchain: &BlockchainNetwork,
    ) -> Result<OnChainCredential, String>;

    /// Revoke on-chain credential
    fn revoke_on_chain(
        &self,
        credential_id: &str,
        blockchain: &BlockchainNetwork,
    ) -> Result<String, String>;

    /// Verify on-chain credential
    fn verify_on_chain(
        &self,
        credential: &OnChainCredential,
    ) -> Result<bool, String>;

    /// Bridge credential to another chain
    fn bridge_credential(
        &self,
        credential_id: &str,
        target_blockchain: &BlockchainNetwork,
    ) -> Result<CrossChainCredential, String>;

    /// Get credential from blockchain
    fn get_credential(
        &self,
        blockchain: &BlockchainNetwork,
        credential_id: &str,
    ) -> Result<OnChainCredential, String>;

    /// Submit governance proposal
    fn submit_governance_proposal(
        &self,
        blockchain: &BlockchainNetwork,
        proposal: OnChainGovernanceProposal,
    ) -> Result<String, String>;

    /// Vote on proposal
    fn vote_on_proposal(
        &self,
        blockchain: &BlockchainNetwork,
        proposal_id: Uuid,
        voter_address: &str,
        vote: bool,
    ) -> Result<String, String>;
}
