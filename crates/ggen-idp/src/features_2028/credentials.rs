/// Advanced Credential Management for Agent Swarms
/// Multi-format credentials, selective disclosure, and privacy-preserving verification

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// CREDENTIAL TYPES & FORMATS
// ============================================================================

/// Unified credential (supports multiple formats)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct UnifiedCredential {
    pub credential_id: Uuid,
    pub issuer_id: Uuid,
    pub subject_id: Uuid,
    pub credential_type: CredentialType,
    pub formats: Vec<CredentialFormat>,
    pub claims: Vec<CredentialClaim>,
    pub issued_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
    pub status: CredentialStatus,
    pub metadata: CredentialMetadata,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialType {
    CapabilityCertificate,                 // What agent can do
    Attestation,                           // Someone attests to something
    License,                               // Permission to perform action
    Badge,                                 // Achievement/milestone
    Endorsement,                           // Recommendation from peer
    Assertion,                             // Claim about agent/data
    Certificate,                           // Formal certification
    Custom(String),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialFormat {
    VerifiableCredentialJsonLd,            // W3C VC in JSON-LD
    JsonWebToken,                          // JWT format
    MDoc,                                  // Mobile Driver License format
    CBOR,                                  // Compact Binary Object Representation
    ZKProof,                               // Zero-knowledge proof format
    Blockchain,                            // On-chain credential
    XML,                                   // Legacy XML format
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialClaim {
    pub claim_key: String,
    pub claim_value: serde_json::Value,
    pub disclosure: DisclosureLevel,
    pub proof_required: bool,
    pub verified: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DisclosureLevel {
    Public,                                // Can be disclosed freely
    SemiPrivate,                           // Can be disclosed to specific agents
    Private,                               // Never disclosed directly
    ZKOnly,                                // Only disclosed as ZK proof
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialStatus {
    Active,
    Suspended,
    Revoked,
    Expired,
    Archived,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialMetadata {
    pub issuer_name: String,
    pub issuer_logo: Option<String>,
    pub tags: Vec<String>,
    pub description: String,
    pub color: Option<String>,             // UI color hint
    pub icon: Option<String>,              // UI icon
}

// ============================================================================
// SELECTIVE DISCLOSURE
// ============================================================================

/// Selective disclosure request
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SelectiveDisclosureRequest {
    pub request_id: Uuid,
    pub requestor_id: Uuid,
    pub holder_id: Uuid,
    pub credential_id: Uuid,
    pub requested_claims: Vec<String>,     // Which claims to disclose
    pub purpose: String,                   // Why they're needed
    pub deadline: Option<DateTime<Utc>>,
    pub one_time_use: bool,               // Can only be used once
    pub status: SelectiveDisclosureStatus,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SelectiveDisclosureStatus {
    Pending,
    Approved,
    Denied,
    Partially,
    Expired,
}

/// Selective disclosure response
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SelectiveDisclosureResponse {
    pub response_id: Uuid,
    pub request_id: Uuid,
    pub disclosed_claims: Vec<DisclosedClaim>,
    pub hidden_claims_proof: Option<String>, // ZK proof for hidden claims
    pub presentation_nonce: String,        // Prevent replay attacks
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DisclosedClaim {
    pub claim_key: String,
    pub claim_value: serde_json::Value,
    pub encrypted: bool,
}

// ============================================================================
// ATTRIBUTE-BASED CREDENTIALS
// ============================================================================

/// Attribute-based credential (based on attributes, not identity)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AttributeBasedCredential {
    pub abc_id: Uuid,
    pub issuer_id: Uuid,
    pub attributes: Vec<Attribute>,
    pub attribute_public_keys: Vec<String>,
    pub credential_structure: String,     // ABC scheme info
    pub issued_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Attribute {
    pub name: String,
    pub value: serde_json::Value,
    pub private: bool,                    // Issuer keeps secret?
}

// ============================================================================
// DELEGATED CREDENTIALS
// ============================================================================

/// Credential authority delegation
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialAuthority {
    pub authority_id: Uuid,
    pub issuer_id: Uuid,
    pub delegate_id: Uuid,
    pub credential_types: Vec<CredentialType>,
    pub max_issuances: Option<u32>,       // Limit how many can be issued
    pub max_validity_period: Option<u32>, // Max validity in days
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub active: bool,
}

// ============================================================================
// REVOCATION & SUSPENSION
// ============================================================================

/// Credential revocation entry
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialRevocation {
    pub revocation_id: Uuid,
    pub credential_id: Uuid,
    pub revoker_id: Uuid,
    pub reason: RevocationReason,
    pub timestamp: DateTime<Utc>,
    pub blockchain_tx: Option<String>,    // If revoked on-chain
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RevocationReason {
    Superseded,                            // New version exists
    Compromised,                           // Key or data compromised
    Expired,                               // Natural expiration
    RequestedByHolder,                     // Holder asked for revocation
    RequestedByIssuer,                     // Issuer wants to revoke
    AdminRequest,                          // Admin forced revocation
}

/// Revocation status list (efficient revocation checking)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RevocationStatusList {
    pub list_id: Uuid,
    pub issuer_id: Uuid,
    pub entries: Vec<RevocationStatusEntry>,
    pub version: u32,
    pub updated_at: DateTime<Utc>,
    pub blockchain_anchor: Option<String>, // Hash published on-chain
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RevocationStatusEntry {
    pub credential_id: String,
    pub status: RevocationStatus,
    pub last_update: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum RevocationStatus {
    Active,
    Revoked,
    Suspended,
}

// ============================================================================
// CREDENTIAL CHAINING
// ============================================================================

/// Credential that references other credentials
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ChainedCredential {
    pub credential_id: Uuid,
    pub issuer_id: Uuid,
    pub subject_id: Uuid,
    pub required_credentials: Vec<Uuid>,  // Must hold these credentials
    pub credential_logic: CredentialLogic,
    pub claims: Vec<CredentialClaim>,
    pub issued_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum CredentialLogic {
    AND,   // Must have ALL required credentials
    OR,    // Must have AT LEAST ONE required credential
    CUSTOM(String), // Custom logic expression
}

// ============================================================================
// CREDENTIAL PREDICATES
// ============================================================================

/// Predicate proof (prove condition without revealing data)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialPredicate {
    pub predicate_id: Uuid,
    pub credential_id: Uuid,
    pub predicate: String,                // e.g., "age > 18"
    pub proof: String,                    // ZK proof
    pub verified: bool,
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// CREDENTIAL HOLDER WALLET
// ============================================================================

/// Agent's credential wallet
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialWallet {
    pub wallet_id: Uuid,
    pub holder_id: Uuid,
    pub credentials: Vec<Uuid>,
    pub organization_credentials: Vec<Uuid>,
    pub personal_credentials: Vec<Uuid>,
    pub total_credentials: u32,
    pub last_updated: DateTime<Utc>,
}

/// Credential offer from issuer
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialOffer {
    pub offer_id: Uuid,
    pub issuer_id: Uuid,
    pub credential_type: CredentialType,
    pub credential_manifest: CredentialManifest,
    pub expires_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialManifest {
    pub presentation_definition: String,  // What's needed to get this credential
    pub output_descriptors: Vec<OutputDescriptor>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct OutputDescriptor {
    pub id: String,
    pub schema: String,                   // JSON Schema
    pub name: String,
    pub description: String,
}

// ============================================================================
// CREDENTIAL EXCHANGE PROTOCOL
// ============================================================================

/// Credential exchange flow
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CredentialExchangeFlow {
    pub flow_id: Uuid,
    pub issuer_id: Uuid,
    pub holder_id: Uuid,
    pub phase: ExchangePhase,
    pub credential_id: Option<Uuid>,
    pub exchanges: Vec<ExchangeMessage>,
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ExchangePhase {
    OfferSent,
    OfferReceived,
    RequestSent,
    RequestReceived,
    CredentialSent,
    CredentialReceived,
    AckSent,
    Complete,
    Failed,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExchangeMessage {
    pub message_id: Uuid,
    pub from: Uuid,
    pub to: Uuid,
    pub message_type: String,
    pub payload: serde_json::Value,
    pub timestamp: DateTime<Utc>,
}

// ============================================================================
// CREDENTIAL SERVICE INTERFACE
// ============================================================================

pub trait CredentialService: Send + Sync {
    /// Issue credential
    fn issue_credential(
        &self,
        issuer_id: Uuid,
        subject_id: Uuid,
        credential: UnifiedCredential,
    ) -> Result<Uuid, String>;

    /// Verify credential
    fn verify_credential(
        &self,
        credential: &UnifiedCredential,
    ) -> Result<bool, String>;

    /// Request selective disclosure
    fn request_selective_disclosure(
        &self,
        request: SelectiveDisclosureRequest,
    ) -> Result<Uuid, String>;

    /// Respond to disclosure request
    fn respond_to_disclosure_request(
        &self,
        response: SelectiveDisclosureResponse,
    ) -> Result<(), String>;

    /// Revoke credential
    fn revoke_credential(
        &self,
        credential_id: Uuid,
        reason: RevocationReason,
    ) -> Result<(), String>;

    /// Check revocation status
    fn check_revocation_status(
        &self,
        credential_id: Uuid,
    ) -> Result<RevocationStatus, String>;
}
