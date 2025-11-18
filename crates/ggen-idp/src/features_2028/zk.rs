/// Zero-Knowledge Proofs for AI Agent Swarms
/// Privacy-preserving credential verification without disclosing underlying data

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

/// Zero-Knowledge Proof
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ZeroKnowledgeProof {
    pub id: Uuid,
    pub proof_type: ProofType,
    pub prover_id: Uuid,                    // Agent creating proof
    pub verifier_id: Option<Uuid>,          // Agent verifying (None = anyone)
    pub commitment: String,                 // Commitment hash
    pub witness: Option<String>,            // Secret witness (only prover has)
    pub proof_data: String,                 // Cryptographic proof
    pub verified: bool,
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub metadata: ProofMetadata,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProofType {
    AgeInRange,                             // Age between min and max without revealing exact age
    Membership,                             // Member of set without revealing identity
    Balance,                                // Account has balance above threshold
    CredentialAttribute,                    // Attribute value satisfies condition
    ZKSNARKs,                              // Generic zkSNARK
    ZKSTARKs,                              // Generic zkSTARK (quantum-resistant)
    BulletproofRangeProof,                 // Prove number in range
    Commitment,                             // Prove commitment knowledge
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofMetadata {
    pub circuit_id: Option<String>,
    pub circuit_version: Option<String>,
    pub proving_time_ms: u32,
    pub verification_time_ms: u32,
    pub num_constraints: Option<u32>,
    pub num_public_inputs: Option<u32>,
    pub proof_size_bytes: u32,
}

// ============================================================================
// SPECIFIC PROOF TYPES FOR AGENTS
// ============================================================================

/// Age Range Proof (prove age ≥ 18 without revealing exact age)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AgeRangeProof {
    pub min_age: u8,
    pub max_age: u8,
    pub proof: String,                      // Cryptographic proof
    pub commitment_of_birth_date: String,  // Hiding the actual date
    pub created_at: DateTime<Utc>,
}

/// Membership Proof (prove membership without revealing member)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MembershipProof {
    pub set_id: String,                    // Which set (e.g., "approved-agents")
    pub set_size: u32,
    pub commitment_to_member: String,
    pub merkle_root: String,               // Root of Merkle tree
    pub merkle_proof_path: Vec<String>,    // Path in Merkle tree
    pub proof: String,
    pub created_at: DateTime<Utc>,
}

/// Balance Proof (prove balance above threshold without revealing amount)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BalanceProof {
    pub account_id: String,                // Account/agent
    pub currency: String,
    pub min_balance: u64,                  // Threshold
    pub commitment: String,                // Commitment to actual balance
    pub range_proof: String,               // Bulletproof range proof
    pub created_at: DateTime<Utc>,
}

/// Attribute Condition Proof (prove attribute satisfies condition)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AttributeConditionProof {
    pub credential_id: String,
    pub attribute_name: String,
    pub condition: String,                 // CEL expression (e.g., "value > 100")
    pub proof: String,
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// CIRCUITS & PROVING SYSTEMS
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CircuitType {
    AgeVerification,
    SetMembership,
    RangeProof,
    CredentialAttribute,
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Circuit {
    pub id: String,
    pub circuit_type: CircuitType,
    pub name: String,
    pub description: String,
    pub version: String,
    pub proving_system: ProvingSystem,
    pub num_constraints: u32,
    pub num_public_inputs: u32,
    pub num_private_inputs: u32,
    pub zksnark_params: Option<String>,   // Path to proving/verification key
    pub compiled_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ProvingSystem {
    Groth16,                               // Fast, small proofs
    Plonk,                                 // Flexible, universal
    Marlin,                                // Universal without ceremony
    STARK,                                 // Quantum-resistant
    Bulletproof,                           // Range proofs
    MimC7,                                 // Hash-based
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ZKSNARKParams {
    pub params_id: Uuid,
    pub proving_system: ProvingSystem,
    pub circuit_id: String,
    pub proving_key: String,               // Base64-encoded proving key
    pub verification_key: String,          // Base64-encoded verification key
    pub prepared_verification_key: Option<String>,
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// PROOF GENERATION & VERIFICATION
// ============================================================================

/// Request to generate a zero-knowledge proof
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofGenerationRequest {
    pub request_id: Uuid,
    pub circuit_id: String,
    pub public_inputs: serde_json::Value,  // Public data
    pub private_inputs: serde_json::Value, // Secret data
    pub proving_system: ProvingSystem,
}

/// Result of proof generation
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofGenerationResult {
    pub request_id: Uuid,
    pub proof: String,                     // Serialized proof
    pub public_inputs_hash: String,       // Hash of public inputs
    pub generating_time_ms: u32,
    pub success: bool,
    pub error: Option<String>,
}

/// Request to verify a zero-knowledge proof
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofVerificationRequest {
    pub proof: String,
    pub circuit_id: String,
    pub public_inputs: serde_json::Value,
    pub proving_system: ProvingSystem,
}

/// Result of proof verification
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProofVerificationResult {
    pub verified: bool,
    pub verification_time_ms: u32,
    pub error: Option<String>,
}

// ============================================================================
// PRIVACY-PRESERVING CREDENTIAL PRESENTATION
// ============================================================================

/// Present credential with ZK proof instead of revealing data
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ZKCredentialPresentation {
    pub presentation_id: Uuid,
    pub credential_id: String,
    pub holder_id: Uuid,
    pub verifier_id: Uuid,
    pub disclosed_attributes: Vec<String>,    // Which attributes shown plaintext
    pub hidden_attributes: Vec<String>,       // Which attributes hidden in ZK proof
    pub zk_proofs: Vec<ZeroKnowledgeProof>,  // Proofs for hidden attributes
    pub challenge: String,                    // Nonce to prevent replay
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// AGGREGATED PROOFS (MULTI-PARTY)
// ============================================================================

/// Multiple agents prove something together (e.g., majority approval)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AggregatedZKProof {
    pub aggregation_id: Uuid,
    pub provers: Vec<Uuid>,                    // Agents participating
    pub proofs: Vec<ZeroKnowledgeProof>,      // Individual proofs
    pub aggregation_method: AggregationMethod,
    pub aggregated_proof: Option<String>,     // Combined proof (if possible)
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AggregationMethod {
    Threshold,                                  // M-of-N proofs needed
    Unanimous,                                  // All proofs required
    Weighted,                                   // Weighted by agent reputation
    RecursiveComposition,                      // Compose proofs recursively
}

// ============================================================================
// PRIVACY POOLS (ANONYMITY SET)
// ============================================================================

/// Privacy pool for agents to hide in larger set
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PrivacyPool {
    pub pool_id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub member_commitment_tree: MerkleTree,
    pub denomination: u64,                     // Amount agents deposit
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MerkleTree {
    pub root: String,
    pub height: u32,
    pub leaf_count: u32,
    pub leaf_hashes: Vec<String>,
}

/// Proof using privacy pool (e.g., transaction from anonymity set)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PrivacyPoolProof {
    pub pool_id: Uuid,
    pub nullifier: String,                     // Prevents double-spending
    pub merkle_path: Vec<String>,              // Proof membership in pool
    pub proof: String,                         // zk-SNARK proving membership
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// ZK SERVICE INTERFACE
// ============================================================================

pub trait ZKService: Send + Sync {
    /// Generate zero-knowledge proof
    fn generate_proof(
        &self,
        request: ProofGenerationRequest,
    ) -> Result<ProofGenerationResult, String>;

    /// Verify zero-knowledge proof
    fn verify_proof(
        &self,
        request: ProofVerificationRequest,
    ) -> Result<ProofVerificationResult, String>;

    /// Create age range proof
    fn prove_age_range(
        &self,
        actual_age: u8,
        min_age: u8,
        max_age: u8,
    ) -> Result<AgeRangeProof, String>;

    /// Create membership proof
    fn prove_membership(
        &self,
        member_id: String,
        set: Vec<String>,
    ) -> Result<MembershipProof, String>;

    /// Create balance proof
    fn prove_balance(
        &self,
        account_id: String,
        actual_balance: u64,
        min_balance: u64,
    ) -> Result<BalanceProof, String>;

    /// Create privacy pool proof
    fn prove_privacy_pool_membership(
        &self,
        pool_id: Uuid,
        secret: String,
    ) -> Result<PrivacyPoolProof, String>;
}

// ============================================================================
// 2028+ ADVANCED ZK FEATURES
// ============================================================================

/// Recursive zk-SNARK (proof of proofs)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RecursiveZKProof {
    pub outer_proof: String,               // Proof verifying inner proofs
    pub inner_proofs: Vec<String>,
    pub created_at: DateTime<Utc>,
}

/// ZK Machine Learning (prove model output without revealing model)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ZKMLProof {
    pub model_id: String,
    pub input_commitment: String,
    pub output: serde_json::Value,         // Public output
    pub proof: String,                     // Proves output is correct
    pub created_at: DateTime<Utc>,
}

/// Quantum-Resistant ZK (zkSTARK instead of zkSNARK)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumResistantZKProof {
    pub proof_type: String,                // "zkSTARK"
    pub proof_data: String,
    pub parameters: String,                // No trusted setup needed
    pub created_at: DateTime<Utc>,
}
