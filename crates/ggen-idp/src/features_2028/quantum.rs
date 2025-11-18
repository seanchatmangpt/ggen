/// Quantum-Safe Cryptography for Agent Swarms
/// Post-quantum algorithms resistant to quantum computing attacks

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// QUANTUM-SAFE ALGORITHMS (NIST Standardized)
// ============================================================================

/// Quantum-safe key pair
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumSafeKeyPair {
    pub key_pair_id: Uuid,
    pub algorithm: QuantumSafeAlgorithm,
    pub public_key: String,                // Base64-encoded
    pub private_key: String,               // Encrypted, base64-encoded
    pub created_at: DateTime<Utc>,
    pub rotation_scheduled: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq)]
pub enum QuantumSafeAlgorithm {
    /// Lattice-based KEM (Key Encapsulation Mechanism)
    CrystalsKyber512,                     // Lightweight
    CrystalsKyber768,                     // Balanced (RECOMMENDED)
    CrystalsKyber1024,                    // Maximum security

    /// Lattice-based Digital Signature
    CrystalsDilithium2,                   // Lightweight
    CrystalsDilithium3,                   // Balanced (RECOMMENDED)
    CrystalsDilithium5,                   // Maximum security

    /// Lattice-based Fast Signature
    Falcon512,                            // Ultra-fast signing
    Falcon1024,                           // Maximum security

    /// Hash-based Signature (stateless)
    SphincsPlus,                          // SPHINCS+ SHA-256

    /// Multivariate polynomial
    RainbowIa,                            // Lightweight
    RainbowIc,                            // Balanced
    RainbowVc,                            // Maximum security

    /// Isogeny-based
    Sike,                                 // Small keys
    Csidh,                                // Minimal structure

    /// Hybrid (classical + quantum-safe)
    HybridEd25519Dilithium,               // Ed25519 + Dilithium
    HybridEcdsaKyber,                     // ECDSA + Kyber
}

impl QuantumSafeAlgorithm {
    /// Get key size in bytes
    pub fn key_size(&self) -> u32 {
        match self {
            QuantumSafeAlgorithm::CrystalsKyber512 => 800,
            QuantumSafeAlgorithm::CrystalsKyber768 => 1184,
            QuantumSafeAlgorithm::CrystalsKyber1024 => 1568,
            QuantumSafeAlgorithm::CrystalsDilithium2 => 1312,
            QuantumSafeAlgorithm::CrystalsDilithium3 => 1952,
            QuantumSafeAlgorithm::CrystalsDilithium5 => 2592,
            QuantumSafeAlgorithm::Falcon512 => 897,
            QuantumSafeAlgorithm::Falcon1024 => 1793,
            QuantumSafeAlgorithm::SphincsPlus => 8080,
            _ => 0,
        }
    }

    /// Security level (NIST)
    pub fn security_level(&self) -> SecurityLevel {
        match self {
            QuantumSafeAlgorithm::CrystalsKyber512
            | QuantumSafeAlgorithm::CrystalsDilithium2
            | QuantumSafeAlgorithm::Falcon512
            | QuantumSafeAlgorithm::RainbowIa => SecurityLevel::Level1,

            QuantumSafeAlgorithm::CrystalsKyber768
            | QuantumSafeAlgorithm::CrystalsDilithium3
            | QuantumSafeAlgorithm::RainbowIc => SecurityLevel::Level3,

            QuantumSafeAlgorithm::CrystalsKyber1024
            | QuantumSafeAlgorithm::CrystalsDilithium5
            | QuantumSafeAlgorithm::Falcon1024
            | QuantumSafeAlgorithm::RainbowVc => SecurityLevel::Level5,

            QuantumSafeAlgorithm::SphincsPlus => SecurityLevel::Level5,
            QuantumSafeAlgorithm::Sike => SecurityLevel::Level1,
            QuantumSafeAlgorithm::Csidh => SecurityLevel::Level3,
            QuantumSafeAlgorithm::HybridEd25519Dilithium => SecurityLevel::Level3,
            QuantumSafeAlgorithm::HybridEcdsaKyber => SecurityLevel::Level3,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SecurityLevel {
    Level1,    // 128-bit classical equivalent
    Level3,    // 192-bit classical equivalent
    Level5,    // 256-bit classical equivalent
}

// ============================================================================
// QUANTUM-SAFE SIGNATURES
// ============================================================================

/// Signature using quantum-safe algorithm
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumSafeSignature {
    pub signature_id: Uuid,
    pub signer_id: Uuid,
    pub algorithm: QuantumSafeAlgorithm,
    pub message_hash: String,             // SHA-256 hash of message
    pub signature_value: String,          // Base64-encoded signature
    pub signing_time_ms: u32,
    pub created_at: DateTime<Utc>,
}

/// Verification result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignatureVerificationResult {
    pub signature_id: Uuid,
    pub verified: bool,
    pub verification_time_ms: u32,
    pub key_used: String,
    pub error: Option<String>,
}

// ============================================================================
// HYBRID QUANTUM-CLASSICAL KEYS
// ============================================================================

/// Hybrid key combines classical and quantum-safe algorithms
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HybridKeyPair {
    pub hybrid_key_id: Uuid,
    pub agent_id: Uuid,
    pub classical_algorithm: String,      // "ed25519", "ecdsa", "rsa"
    pub classical_public_key: String,
    pub classical_private_key: String,    // Encrypted
    pub quantum_safe_algorithm: QuantumSafeAlgorithm,
    pub quantum_safe_public_key: String,
    pub quantum_safe_private_key: String, // Encrypted
    pub created_at: DateTime<Utc>,
    pub post_quantum_migration_scheduled: Option<DateTime<Utc>>,
}

/// Hybrid signature (both algorithms sign)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HybridSignature {
    pub signature_id: Uuid,
    pub message: String,
    pub classical_signature: QuantumSafeSignature,
    pub quantum_safe_signature: QuantumSafeSignature,
    pub created_at: DateTime<Utc>,
}

// ============================================================================
// POST-QUANTUM KEY MIGRATION
// ============================================================================

/// Plan to migrate keys to quantum-safe algorithms
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct KeyMigrationPlan {
    pub plan_id: Uuid,
    pub agent_id: Uuid,
    pub from_algorithm: String,           // Current algorithm
    pub to_algorithm: QuantumSafeAlgorithm,
    pub migration_status: MigrationStatus,
    pub phase: MigrationPhase,
    pub rollout_percentage: u8,           // % of keys migrated
    pub created_at: DateTime<Utc>,
    pub estimated_completion: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum MigrationStatus {
    Planning,
    InProgress,
    Testing,
    RollingOut,
    Completed,
    RolledBack,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum MigrationPhase {
    GenerateNewKeys,
    DualSignatures,
    Migration,
    PhaseOut,
    Retire,
}

/// Progress of a migration
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MigrationProgress {
    pub progress_id: Uuid,
    pub plan_id: Uuid,
    pub keys_total: u32,
    pub keys_migrated: u32,
    pub keys_dual_signing: u32,
    pub keys_retired: u32,
    pub last_updated: DateTime<Utc>,
}

// ============================================================================
// QUANTUM-SAFE ENCRYPTION
// ============================================================================

/// Encryption using quantum-safe KEM (Key Encapsulation Mechanism)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumSafeEncryption {
    pub encryption_id: Uuid,
    pub kem_algorithm: QuantumSafeAlgorithm,
    pub encapsulated_key: String,        // KEM ciphertext
    pub encrypted_data: String,          // Actual ciphertext
    pub nonce: String,
    pub authentication_tag: String,      // AEAD tag
    pub created_at: DateTime<Utc>,
}

/// Decryption result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DecryptionResult {
    pub plaintext: Vec<u8>,
    pub decryption_time_ms: u32,
    pub success: bool,
    pub error: Option<String>,
}

// ============================================================================
// QUANTUM THREAT TIMELINE
// ============================================================================

/// Analysis of quantum threat for agent's keys
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumThreatAssessment {
    pub assessment_id: Uuid,
    pub agent_id: Uuid,
    pub current_algorithm: String,
    pub threat_level: ThreatLevel,
    pub harvest_now_decrypt_later_risk: bool,  // Could recorded traffic be decrypted later?
    pub estimated_years_to_risk: u32,
    pub recommended_migration_date: DateTime<Utc>,
    pub assessment_date: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ThreatLevel {
    Safe,                                  // Not threatened
    LowRisk,                              // Some threat in 10+ years
    MediumRisk,                           // Threat in 5-10 years
    HighRisk,                             // Threat in 1-5 years
    CriticalRisk,                         // Immediate migration needed
}

// ============================================================================
// FORWARD SECRECY WITH QUANTUM SAFETY
// ============================================================================

/// Session key using quantum-safe algorithms
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumSafeSessionKey {
    pub session_id: Uuid,
    pub kem_public_key: String,           // Ephemeral public key for KEM
    pub encapsulated_secret: String,      // Ephemeral secret
    pub session_key: String,              // Derived session key
    pub derived_from: QuantumSafeAlgorithm,
    pub ephemeral: bool,                  // Key is used once
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
}

// ============================================================================
// QUANTUM-SAFE CREDENTIAL BINDING
// ============================================================================

/// Credential with quantum-safe binding to agent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct QuantumSafeCredentialBinding {
    pub binding_id: Uuid,
    pub credential_id: String,
    pub agent_id: Uuid,
    pub binding_algorithm: QuantumSafeAlgorithm,
    pub binding_signature: String,
    pub created_at: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
}

// ============================================================================
// CRYPTO AGILITY
// ============================================================================

/// Agent's crypto agility configuration
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CryptoAgilityConfig {
    pub config_id: Uuid,
    pub agent_id: Uuid,
    pub primary_algorithm: QuantumSafeAlgorithm,
    pub fallback_algorithms: Vec<QuantumSafeAlgorithm>,
    pub algorithm_selection_policy: AlgorithmSelectionPolicy,
    pub auto_rotate_interval_days: u32,
    pub created_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AlgorithmSelectionPolicy {
    AlwaysPrimary,                        // Use primary only
    FallbackOnFailure,                    // Try primary, fallback if fails
    EnvironmentBased,                     // Choose based on context
    PerformanceBased,                     // Choose fastest
    SecurityBased,                        // Choose strongest
}

// ============================================================================
// QUANTUM-SAFE SERVICE INTERFACE
// ============================================================================

pub trait QuantumSafeCryptoService: Send + Sync {
    /// Generate quantum-safe key pair
    fn generate_key_pair(
        &self,
        algorithm: QuantumSafeAlgorithm,
    ) -> Result<QuantumSafeKeyPair, String>;

    /// Sign with quantum-safe algorithm
    fn sign(
        &self,
        message: &[u8],
        private_key: &str,
        algorithm: QuantumSafeAlgorithm,
    ) -> Result<QuantumSafeSignature, String>;

    /// Verify signature
    fn verify_signature(
        &self,
        signature: &QuantumSafeSignature,
        public_key: &str,
    ) -> Result<bool, String>;

    /// Encapsulate (KEM public key operation)
    fn encapsulate(
        &self,
        public_key: &str,
        algorithm: QuantumSafeAlgorithm,
    ) -> Result<(String, Vec<u8>), String>;

    /// Decapsulate (KEM private key operation)
    fn decapsulate(
        &self,
        encapsulated_key: &str,
        private_key: &str,
        algorithm: QuantumSafeAlgorithm,
    ) -> Result<Vec<u8>, String>;

    /// Encrypt with quantum-safe KEM
    fn encrypt(
        &self,
        plaintext: &[u8],
        public_key: &str,
        algorithm: QuantumSafeAlgorithm,
    ) -> Result<QuantumSafeEncryption, String>;

    /// Decrypt
    fn decrypt(
        &self,
        encryption: &QuantumSafeEncryption,
        private_key: &str,
    ) -> Result<DecryptionResult, String>;

    /// Plan key migration
    fn plan_key_migration(
        &self,
        agent_id: Uuid,
        from_algorithm: String,
        to_algorithm: QuantumSafeAlgorithm,
    ) -> Result<KeyMigrationPlan, String>;

    /// Get quantum threat assessment
    fn assess_quantum_threat(
        &self,
        agent_id: Uuid,
        algorithm: &str,
    ) -> Result<QuantumThreatAssessment, String>;

    /// Rotate keys
    fn rotate_keys(
        &self,
        agent_id: Uuid,
        new_algorithm: QuantumSafeAlgorithm,
    ) -> Result<HybridKeyPair, String>;
}
