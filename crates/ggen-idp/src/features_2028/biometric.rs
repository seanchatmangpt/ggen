/// Biometric Credentials for AI Agent Swarms
/// Fingerprint, facial recognition, voice, behavioral biometrics binding to identity

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ============================================================================
// BIOMETRIC TYPES
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BiometricType {
    Fingerprint,                          // Fingerprint recognition
    FacialRecognition,                    // Face recognition
    VoiceRecognition,                     // Voice/speaker recognition
    IrisRecognition,                      // Iris pattern recognition
    PalmVein,                             // Palm vein pattern
    Behavioral,                           // Typing pattern, gait, etc.
    Multimodal,                           // Combination of multiple
}

// ============================================================================
// BIOMETRIC TEMPLATE
// ============================================================================

/// Biometric template (encrypted, privacy-preserving)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricTemplate {
    pub template_id: Uuid,
    pub agent_id: Uuid,
    pub biometric_type: BiometricType,
    pub template_data: String,            // Encrypted biometric template
    pub template_format: String,          // ISO/IEC format
    pub quality_score: f32,               // 0.0-1.0, quality of capture
    pub creation_timestamp: DateTime<Utc>,
    pub enrollment_date: DateTime<Utc>,
    pub last_verified: Option<DateTime<Utc>>,
    pub verification_count: u32,
    pub false_acceptance_rate: f32,       // FAR
    pub false_rejection_rate: f32,        // FRR
}

/// Biometric enrollment
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricEnrollment {
    pub enrollment_id: Uuid,
    pub agent_id: Uuid,
    pub biometric_type: BiometricType,
    pub samples_collected: u32,
    pub enrollment_status: EnrollmentStatus,
    pub quality_threshold: f32,           // Minimum required quality
    pub liveness_detected: bool,          // Liveness detection passed
    pub spoofing_check: bool,             // Anti-spoofing check passed
    pub completed_at: Option<DateTime<Utc>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum EnrollmentStatus {
    Started,
    InProgress,
    PartiallyComplete,
    Complete,
    Failed,
    Cancelled,
}

// ============================================================================
// BIOMETRIC VERIFICATION
// ============================================================================

/// Biometric verification attempt
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricVerification {
    pub verification_id: Uuid,
    pub agent_id: Uuid,
    pub template_id: Uuid,
    pub biometric_type: BiometricType,
    pub sample_data: String,              // Encrypted sample
    pub match_score: f32,                 // 0.0-1.0, how close match is
    pub threshold: f32,                   // Decision threshold
    pub verified: bool,                   // Did it pass threshold?
    pub verification_time_ms: u32,
    pub timestamp: DateTime<Utc>,
    pub liveness_score: Option<f32>,      // Liveness confidence
    pub spoofing_detected: bool,
}

/// Multi-factor biometric verification (multiple samples)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MultiBiometricVerification {
    pub verification_id: Uuid,
    pub agent_id: Uuid,
    pub biometric_verifications: Vec<BiometricVerification>,
    pub fusion_method: FusionMethod,
    pub overall_match_score: f32,
    pub all_passed: bool,                 // All must pass
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FusionMethod {
    Weighted,                             // Weighted score fusion
    Majority,                             // Majority voting
    Strictest,                            // Only pass if all pass
    Probabilistic,                        // Bayesian fusion
}

// ============================================================================
// LIVENESS DETECTION & ANTI-SPOOFING
// ============================================================================

/// Liveness detection (prevent replay attacks, deepfakes)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LivenessDetection {
    pub liveness_id: Uuid,
    pub verification_id: Uuid,
    pub liveness_type: LivenessType,
    pub challenge: String,                // e.g., "blink 3 times", "say the number 42"
    pub response_expected: String,        // Expected response
    pub response_received: Option<String>,
    pub liveness_score: f32,              // 0.0-1.0, confidence it's live
    pub passed: bool,
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LivenessType {
    Active,                               // Agent performs actions (blink, speak)
    Passive,                              // Behavioral analysis, no action
    Hybrid,                               // Combination
}

/// Spoofing detection (prevent impersonation)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SpoofingDetection {
    pub spoofing_id: Uuid,
    pub verification_id: Uuid,
    pub spoofing_type: SpoofingType,
    pub detection_method: DetectionMethod,
    pub spoofing_likelihood: f32,         // 0.0-1.0, likelihood it's spoofed
    pub spoofing_detected: bool,
    pub timestamp: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum SpoofingType {
    Presentation,                         // Photo, video, mask
    Deepfake,                             // AI-generated synthetic face
    Replay,                               // Recorded session replayed
    Morphing,                             // Face morphing attack
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum DetectionMethod {
    Textural,                             // Analyze texture patterns
    Frequencyanalysis,                    // Fourier/wavelet analysis
    MotionAnalysis,                       // Analyze motion patterns
    LightReflection,                      // Analyze light properties
    MLBased,                              // Machine learning model
}

// ============================================================================
// BIOMETRIC TEMPLATE PROTECTION
// ============================================================================

/// Cancelable biometrics (revocable if compromised)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CancelableBiometrics {
    pub cancelable_id: Uuid,
    pub original_template_id: Uuid,
    pub transformation_key: String,       // Encryption/transformation key
    pub cancelable_template: String,      // Transformed template
    pub version: u32,                     // Can be revoked and regenerated
    pub created_at: DateTime<Utc>,
    pub revoked_at: Option<DateTime<Utc>>,
}

/// Fuzzy vault (biometric template hiding)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FuzzyVault {
    pub vault_id: Uuid,
    pub agent_id: Uuid,
    pub template_id: Uuid,
    pub vault_data: String,              // Encrypted vault
    pub chaff_size: u32,                 // Number of chaff points
    pub helper_data: String,             // Publicly shareable helper
    pub created_at: DateTime<Utc>,
}

/// Biometric cryptosystem (derive crypto key from biometric)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricCryptosystem {
    pub system_id: Uuid,
    pub agent_id: Uuid,
    pub biometric_type: BiometricType,
    pub derived_key_length: u32,         // Bits
    pub public_key: String,              // Can be published
    pub template_id: Uuid,
    pub binding_data: String,            // Binds biometric to crypto key
}

// ============================================================================
// BEHAVIORAL BIOMETRICS
// ============================================================================

/// Agent's behavioral biometrics
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BehavioralBiometrics {
    pub behavioral_id: Uuid,
    pub agent_id: Uuid,
    pub behavior_type: BehaviorType,
    pub samples: Vec<BehaviorSample>,
    pub profile_updated: DateTime<Utc>,
    pub confidence: f32,                 // How confident in profile
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BehaviorType {
    Typing,                              // Typing speed, rhythm, error patterns
    MouseMovement,                       // Mouse movement patterns
    Scrolling,                           // Scrolling behavior
    Gait,                                // Walking pattern
    NetworkUsage,                        // Network access patterns
    APIUsage,                            // Which APIs called, how
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BehaviorSample {
    pub sample_id: Uuid,
    pub behavior_features: Vec<f32>,    // Feature vector
    pub timestamp: DateTime<Utc>,
    pub context: serde_json::Value,     // Where/when/what was happening
}

/// Continuous authentication (verify behavior ongoing)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ContinuousAuthentication {
    pub auth_session_id: Uuid,
    pub agent_id: Uuid,
    pub behavioral_profile: BehavioralBiometrics,
    pub anomaly_threshold: f32,          // Score indicating anomaly
    pub authentication_score: f32,       // Current confidence
    pub authenticated: bool,
    pub anomalies_detected: Vec<AnomalyDetection>,
    pub started_at: DateTime<Utc>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AnomalyDetection {
    pub anomaly_id: Uuid,
    pub anomaly_type: String,
    pub anomaly_score: f32,
    pub timestamp: DateTime<Utc>,
    pub action_taken: String,            // "flag", "challenge", "revoke"
}

// ============================================================================
// BIOMETRIC PRIVACY & COMPLIANCE
// ============================================================================

/// Biometric data storage policy
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricDataPolicy {
    pub policy_id: Uuid,
    pub jurisdiction: String,            // Where data is stored
    pub retention_days: u32,              // How long to keep
    pub encryption_algorithm: String,    // How encrypted
    pub anonymization: bool,              // De-identify templates?
    pub deletion_method: String,          // How to securely delete
    pub compliance_standards: Vec<String>, // GDPR, HIPAA, etc.
}

/// Biometric consent
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricConsent {
    pub consent_id: Uuid,
    pub agent_id: Uuid,
    pub biometric_type: BiometricType,
    pub consent_given: bool,
    pub purpose: String,
    pub timestamp: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
    pub revoked_at: Option<DateTime<Utc>>,
}

// ============================================================================
// BIOMETRIC PERFORMANCE METRICS
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct BiometricMetrics {
    pub metrics_id: Uuid,
    pub biometric_type: BiometricType,
    pub false_acceptance_rate: f32,       // FAR (type I error)
    pub false_rejection_rate: f32,        // FRR (type II error)
    pub equal_error_rate: f32,            // EER (FAR = FRR)
    pub genuine_acceptance_rate: f32,     // GAR = 1 - FRR
    pub presentation_attack_detection_rate: f32, // PAD
    pub verification_speed_ms: u32,
    pub enrollment_speed_ms: u32,
    pub measured_at: DateTime<Utc>,
}

// ============================================================================
// BIOMETRIC SERVICE INTERFACE
// ============================================================================

pub trait BiometricService: Send + Sync {
    /// Enroll biometric
    fn enroll_biometric(
        &self,
        agent_id: Uuid,
        biometric_type: BiometricType,
        samples: Vec<Vec<u8>>,
    ) -> Result<BiometricTemplate, String>;

    /// Verify biometric
    fn verify_biometric(
        &self,
        agent_id: Uuid,
        template_id: Uuid,
        sample: Vec<u8>,
    ) -> Result<BiometricVerification, String>;

    /// Perform multi-biometric verification
    fn verify_multi_biometric(
        &self,
        agent_id: Uuid,
        samples: Vec<(BiometricType, Vec<u8>)>,
    ) -> Result<MultiBiometricVerification, String>;

    /// Check liveness
    fn check_liveness(
        &self,
        verification_id: Uuid,
        challenge_response: Vec<u8>,
    ) -> Result<LivenessDetection, String>;

    /// Detect spoofing
    fn detect_spoofing(
        &self,
        verification_id: Uuid,
    ) -> Result<SpoofingDetection, String>;

    /// Continuous authentication
    fn authenticate_continuous(
        &self,
        agent_id: Uuid,
        behavior_sample: BehaviorSample,
    ) -> Result<ContinuousAuthentication, String>;

    /// Revoke biometric
    fn revoke_biometric(
        &self,
        template_id: Uuid,
    ) -> Result<(), String>;

    /// Get metrics
    fn get_biometric_metrics(
        &self,
        biometric_type: BiometricType,
    ) -> Result<BiometricMetrics, String>;
}
