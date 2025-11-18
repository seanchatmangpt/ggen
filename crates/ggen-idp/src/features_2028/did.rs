/// Decentralized Identity (DIDs) for AI Agent Swarms
/// W3C DID Core 1.0 Standard Implementation

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

/// W3C DID Document
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDDocument {
    pub id: String,                              // did:web:ggen.dev/agents/alice
    pub context: Vec<String>,                    // @context
    pub public_keys: Vec<PublicKeyEntry>,
    pub authentication: Vec<String>,             // Key IDs for authentication
    pub assertion_method: Vec<String>,           // Key IDs for assertions
    pub key_agreement: Vec<String>,              // Key IDs for encryption
    pub capability_delegation: Vec<String>,      // For capability delegation
    pub capability_invocation: Vec<String>,      // For invoking capabilities
    pub service_endpoints: Vec<ServiceEndpoint>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub proof: DIDProof,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PublicKeyEntry {
    pub id: String,                              // #key-1
    pub key_type: KeyType,
    pub controller: String,                      // DID that controls this key
    pub public_key_pem: String,                  // PEM format
    pub multibase: Option<String>,              // Multibase format (base58btc, base64)
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum KeyType {
    Ed25519VerificationKey2020,                 // For signatures
    X25519KeyAgreementKey2020,                  // For key agreement
    RsaVerificationKey2020,
    BbsBlsSignature2020,                        // For selective disclosure
    EcdsaSecp256k1VerificationKey2019,         // For blockchain
    Kyber768,                                   // Post-quantum
    Dilithium3,                                 // Post-quantum
    Sphincs,                                    // Post-quantum
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ServiceEndpoint {
    pub id: String,
    pub endpoint_type: String,                  // "LinkedDomains", "W3CCredentialRepository", "OracleService", etc
    pub service_endpoint: String,               // URI or object
    pub accept: Option<Vec<String>>,           // MIME types accepted
    pub metadata: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDProof {
    pub proof_type: String,                     // "Ed25519Signature2020", "BbsBlsSignature2020"
    pub proof_purpose: String,                  // "assertionMethod", "authentication"
    pub verification_method: String,            // Did method used
    pub signature_value: String,                // Proof value
    pub created: DateTime<Utc>,
}

// ============================================================================
// DID METHODS FOR AGENT SWARMS
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DIDMethod {
    /// did:web - Web-based DID (easiest for centralized)
    Web {
        domain: String,
        path: Option<String>,
    },
    /// did:key - Self-managed (most private)
    Key {
        public_key: String,
        key_type: KeyType,
    },
    /// did:ethereum - Blockchain-based
    Ethereum {
        chain_id: u32,           // 1 = mainnet, 5 = goerli, etc
        address: String,          // 0x...
        registry_contract: String,
    },
    /// did:polygon - Polygon blockchain
    Polygon {
        chain_id: u32,
        address: String,
    },
    /// did:solana - Solana blockchain
    Solana {
        public_key: String,
    },
    /// did:swarm - Custom swarm DID (for multi-agent coordination)
    Swarm {
        swarm_id: String,
        agent_id: String,
    },
}

impl DIDMethod {
    /// Encode as DID string
    pub fn to_did_string(&self) -> String {
        match self {
            DIDMethod::Web { domain, path } => {
                match path {
                    Some(p) => format!("did:web:{}:{}", domain, p),
                    None => format!("did:web:{}", domain),
                }
            }
            DIDMethod::Key { public_key, key_type } => {
                format!("did:key:z{}#{}", public_key, Self::key_type_suffix(key_type))
            }
            DIDMethod::Ethereum { chain_id, address, .. } => {
                format!("did:ethereum:{}:{}", chain_id, address)
            }
            DIDMethod::Polygon { chain_id, address } => {
                format!("did:polygon:{}:{}", chain_id, address)
            }
            DIDMethod::Solana { public_key } => {
                format!("did:solana:{}", public_key)
            }
            DIDMethod::Swarm { swarm_id, agent_id } => {
                format!("did:swarm:{}:{}", swarm_id, agent_id)
            }
        }
    }

    fn key_type_suffix(key_type: &KeyType) -> &'static str {
        match key_type {
            KeyType::Ed25519VerificationKey2020 => "ed25519",
            KeyType::X25519KeyAgreementKey2020 => "x25519",
            KeyType::Kyber768 => "kyber768",
            KeyType::Dilithium3 => "dilithium3",
            _ => "key",
        }
    }
}

// ============================================================================
// VERIFIABLE CREDENTIALS (VCs) FOR AGENTS
// ============================================================================

/// Verifiable Credential (W3C VC Data Model 1.1)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VerifiableCredential {
    pub context: Vec<String>,                   // @context
    pub id: Option<String>,
    pub credential_type: Vec<String>,           // Type(s) of credential
    pub issuer: String,                         // DID of issuer
    pub issuance_date: DateTime<Utc>,
    pub expiration_date: Option<DateTime<Utc>>,
    pub credential_subject: CredentialSubject,
    pub proof: CredentialProofVC,
    pub status: Option<CredentialStatus>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialSubject {
    pub id: String,                             // DID of subject
    pub claims: std::collections::HashMap<String, serde_json::Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialProofVC {
    pub proof_type: String,
    pub proof_purpose: String,
    pub verification_method: String,
    pub signature_value: String,
    pub created: DateTime<Utc>,
    pub domain: Option<String>,
    pub challenge: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialStatus {
    pub id: String,
    pub status_type: String,                    // "StatusList2021Entry", "RevocationList2020Status", etc
    pub status_purpose: String,                 // "revocation" or "suspension"
    pub status_list_index: Option<u32>,        // Bit position in status list
}

// ============================================================================
// VERIFIABLE PRESENTATIONS (VPs)
// ============================================================================

/// Verifiable Presentation (Agent presenting credentials)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VerifiablePresentation {
    pub context: Vec<String>,
    pub id: Option<String>,
    pub presentation_type: Vec<String>,
    pub verifiable_credential: Vec<VerifiableCredential>,
    pub holder: String,                         // DID of holder
    pub proof: PresentationProof,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PresentationProof {
    pub proof_type: String,
    pub proof_purpose: String,
    pub verification_method: String,
    pub challenge: String,                      // Nonce to prevent replay
    pub domain: String,                         // Verifier's domain
    pub signature_value: String,
    pub created: DateTime<Utc>,
}

// ============================================================================
// DID RESOLUTION
// ============================================================================

/// DID URL with fragments and parameters
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDUrl {
    pub did: String,
    pub path: Option<String>,
    pub query: Option<String>,
    pub fragment: Option<String>,
}

/// Result of DID resolution
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDResolutionResult {
    pub did_document: DIDDocument,
    pub metadata: DIDResolutionMetadata,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDResolutionMetadata {
    pub content_type: String,                   // "application/did+ld+json"
    pub retrieved_time: DateTime<Utc>,
    pub duration_ms: u32,
}

/// DID Resolution Error
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDResolutionError {
    pub error: String,
    pub error_code: DIDErrorCode,
    pub metadata: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DIDErrorCode {
    InvalidDid,
    NotFound,
    RepresentationNotSupported,
    UnsupportedDidMethod,
    InvalidDidUrl,
    DeactivatedDid,
    InternalError,
}

// ============================================================================
// DID REGISTRY & MANAGEMENT
// ============================================================================

/// Agent's DID Registry Entry
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDRegistryEntry {
    pub id: Uuid,
    pub agent_id: Uuid,
    pub did: String,
    pub did_method: String,
    pub did_document: DIDDocument,
    pub is_primary: bool,
    pub is_active: bool,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub blockchain_tx_id: Option<String>,      // For blockchain-based DIDs
    pub rotation_next: Option<DateTime<Utc>>,  // Key rotation schedule
}

/// DID Key Rotation
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDKeyRotation {
    pub id: Uuid,
    pub did: String,
    pub old_key_id: String,
    pub new_key_id: String,
    pub rotation_proof: String,
    pub created_at: DateTime<Utc>,
    pub effective_at: DateTime<Utc>,
}

// ============================================================================
// ADVANCED DID FEATURES FOR SWARMS
// ============================================================================

/// Linked Data Proofs (selective disclosure)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LinkedDataProof {
    pub proof_type: String,                     // "BbsBlsSignature2020"
    pub verification_method: String,
    pub proof_value: String,
    pub nonce: Option<String>,
    pub created: DateTime<Utc>,
}

/// Selective Disclosure Proof (prove claims without revealing values)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SelectiveDisclosureProof {
    pub proof_id: Uuid,
    pub verifiable_credential_id: String,
    pub disclosed_attributes: Vec<String>,     // Which attributes to reveal
    pub hidden_attributes: Vec<String>,        // Which to hide
    pub proof: String,                         // BBS+ proof
}

/// Cross-DID Delegation (one DID delegates to another)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DIDDelegation {
    pub id: Uuid,
    pub delegator_did: String,
    pub delegate_did: String,
    pub delegated_capabilities: Vec<String>,
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub delegation_proof: String,
}

// ============================================================================
// DID SERVICE INTERFACE
// ============================================================================

/// DID Service trait
pub trait DIDService: Send + Sync {
    /// Create new DID document
    fn create_did(&self, method: DIDMethod) -> Result<DIDDocument, String>;

    /// Resolve DID to document
    fn resolve_did(&self, did: &str) -> Result<DIDResolutionResult, DIDResolutionError>;

    /// Update DID document
    fn update_did(&self, did: &str, document: DIDDocument) -> Result<(), String>;

    /// Deactivate DID (no longer usable)
    fn deactivate_did(&self, did: &str) -> Result<(), String>;

    /// Rotate DID keys
    fn rotate_keys(&self, did: &str, new_key: PublicKeyEntry) -> Result<DIDKeyRotation, String>;

    /// Create verifiable credential
    fn issue_credential(&self, credential: VerifiableCredential) -> Result<String, String>;

    /// Verify credential
    fn verify_credential(&self, credential: &VerifiableCredential) -> Result<bool, String>;

    /// Create presentation
    fn create_presentation(&self, credentials: Vec<VerifiableCredential>, holder_did: &str) -> Result<VerifiablePresentation, String>;

    /// Verify presentation
    fn verify_presentation(&self, presentation: &VerifiablePresentation) -> Result<bool, String>;

    /// Selective disclosure
    fn create_selective_disclosure_proof(
        &self,
        credential: &VerifiableCredential,
        disclosed_attributes: Vec<String>,
    ) -> Result<SelectiveDisclosureProof, String>;
}
