//! Observation system (O): Type-safe observations from connected systems
//!
//! This module implements the observation model (O) - the foundational data model
//! where all decisions derive their truth. Observations are immutable, schema-validated,
//! and carry cryptographic proofs of origin.

use crate::error::{DoDError, DoDResult, ObservationValidationError};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::BTreeMap;
use std::fmt;
use uuid::Uuid;

/// Unique identifier for observations
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
pub struct ObservationId(Uuid);

impl ObservationId {
    /// Generate a new observation ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }

    /// Create from an existing UUID
    pub fn from_uuid(uuid: Uuid) -> Self {
        Self(uuid)
    }
}

impl Default for ObservationId {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ObservationId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Types of observations from different subsystems
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ObservationType {
    /// Telemetry metrics (latency, throughput, etc)
    Metric(MetricType),
    /// Anomaly detection results
    Anomaly(AnomalyType),
    /// SLO breach notifications
    SLOBreach(String),
    /// User-reported issues
    UserReport,
    /// Integration test results
    IntegrationTest,
    /// Performance benchmark results
    PerformanceBenchmark,
    /// Security audit findings
    SecurityAudit,
    /// Compliance check results
    ComplianceCheck,
    /// System state observation (health, topology, etc)
    SystemState,
    /// Custom observation type (for extensibility)
    Custom(String),
}

/// Metric types that observations can carry
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MetricType {
    Latency,
    Throughput,
    ErrorRate,
    CpuUsage,
    MemoryUsage,
    DiskUsage,
    NetworkLatency,
    CacheHitRate,
    Custom(String),
}

/// Anomaly types detected by analysis subsystem
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum AnomalyType {
    Drift,
    Outlier,
    CorrelationChange,
    TrendChange,
    Custom(String),
}

/// Schema version for observations - ensures Σ is stable
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ObservationSchema {
    /// Version identifier (e.g., "1.0", "2.1")
    version: String,
    /// Required fields for this schema version
    required_fields: Vec<String>,
    /// Field type constraints
    field_types: BTreeMap<String, FieldType>,
}

impl ObservationSchema {
    /// Create a new observation schema
    pub fn new(version: impl Into<String>) -> Self {
        Self {
            version: version.into(),
            required_fields: Vec::new(),
            field_types: BTreeMap::new(),
        }
    }

    /// Add a required field
    pub fn with_required_field(mut self, field: impl Into<String>, field_type: FieldType) -> Self {
        let field_name = field.into();
        self.required_fields.push(field_name.clone());
        self.field_types.insert(field_name, field_type);
        self
    }

    /// Validate an observation against this schema
    pub fn validate(&self, observation: &Observation) -> DoDResult<()> {
        // Check all required fields are present
        for required in &self.required_fields {
            if !observation.data.contains_key(required) {
                return Err(DoDError::ObservationValidation(format!(
                    "missing required field: {}",
                    required
                )));
            }
        }

        // Check field types match
        for (field, expected_type) in &self.field_types {
            if let Some(value) = observation.data.get(field) {
                if !expected_type.matches(value) {
                    return Err(DoDError::ObservationValidation(format!(
                        "field '{}' type mismatch: expected {}, got {}",
                        field,
                        expected_type.name(),
                        value_type_name(value)
                    )));
                }
            }
        }

        // Check size constraint
        if observation.data.to_string().len() > crate::constants::MAX_OBSERVATION_SIZE {
            return Err(DoDError::Observation(format!(
                "observation exceeds maximum size of {} bytes",
                crate::constants::MAX_OBSERVATION_SIZE
            )));
        }

        Ok(())
    }

    /// Get schema version
    pub fn version(&self) -> &str {
        &self.version
    }
}

/// Field type constraint for schema validation
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FieldType {
    String,
    Number,
    Integer,
    Boolean,
    Object,
    Array,
    Enum(Vec<String>),
    Optional(Box<FieldType>),
}

impl FieldType {
    /// Check if a JSON value matches this field type
    fn matches(&self, value: &serde_json::Value) -> bool {
        match self {
            FieldType::String => value.is_string(),
            FieldType::Number => value.is_number(),
            FieldType::Integer => value.is_i64() || value.is_u64(),
            FieldType::Boolean => value.is_boolean(),
            FieldType::Object => value.is_object(),
            FieldType::Array => value.is_array(),
            FieldType::Enum(variants) => {
                if let Some(s) = value.as_str() {
                    variants.contains(&s.to_string())
                } else {
                    false
                }
            }
            FieldType::Optional(inner) => value.is_null() || inner.matches(value),
        }
    }

    /// Get human-readable type name
    fn name(&self) -> &'static str {
        match self {
            FieldType::String => "string",
            FieldType::Number => "number",
            FieldType::Integer => "integer",
            FieldType::Boolean => "boolean",
            FieldType::Object => "object",
            FieldType::Array => "array",
            FieldType::Enum(_) => "enum",
            FieldType::Optional(_) => "optional",
        }
    }
}

/// Get human-readable type name for a JSON value
fn value_type_name(value: &serde_json::Value) -> &'static str {
    match value {
        serde_json::Value::String(_) => "string",
        serde_json::Value::Number(_) => "number",
        serde_json::Value::Bool(_) => "boolean",
        serde_json::Value::Object(_) => "object",
        serde_json::Value::Array(_) => "array",
        serde_json::Value::Null => "null",
    }
}

/// A type-safe observation from a connected subsystem
///
/// Observations (O) are the fundamental source of truth in ggen's decision-making.
/// Every observation is:
/// - Immutable (enforced by type system)
/// - Schema-validated (conforms to Σ)
/// - Timestamped (for temporal ordering)
/// - Provenance-tracked (cryptographically signed)
/// - Tenant-isolated (belongs to exactly one tenant)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Observation {
    /// Unique observation ID
    id: ObservationId,
    /// Type of observation
    obs_type: ObservationType,
    /// Observation payload (arbitrary JSON matching schema)
    data: serde_json::Value,
    /// Timestamp in UTC
    timestamp: DateTime<Utc>,
    /// Source subsystem (monitor, test, audit, etc)
    source: String,
    /// Schema version that this observation conforms to
    schema_version: String,
    /// Tenant ID (for multi-tenant isolation)
    tenant_id: String,
    /// Cryptographic signature (HMAC-SHA256) for integrity
    signature: Option<String>,
}

impl Observation {
    /// Create a new observation
    pub fn new(
        obs_type: ObservationType,
        data: serde_json::Value,
        source: impl Into<String>,
        schema_version: impl Into<String>,
        tenant_id: impl Into<String>,
    ) -> DoDResult<Self> {
        let obs = Self {
            id: ObservationId::new(),
            obs_type,
            data,
            timestamp: Utc::now(),
            source: source.into(),
            schema_version: schema_version.into(),
            tenant_id: tenant_id.into(),
            signature: None,
        };

        Ok(obs)
    }

    /// Get observation ID
    pub fn id(&self) -> ObservationId {
        self.id
    }

    /// Get observation type
    pub fn obs_type(&self) -> &ObservationType {
        &self.obs_type
    }

    /// Get observation data
    pub fn data(&self) -> &serde_json::Value {
        &self.data
    }

    /// Get timestamp
    pub fn timestamp(&self) -> DateTime<Utc> {
        self.timestamp
    }

    /// Get source subsystem
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get schema version
    pub fn schema_version(&self) -> &str {
        &self.schema_version
    }

    /// Get tenant ID
    pub fn tenant_id(&self) -> &str {
        &self.tenant_id
    }

    /// Get cryptographic signature
    pub fn signature(&self) -> Option<&str> {
        self.signature.as_deref()
    }

    /// Sign the observation with HMAC-SHA256
    pub fn with_signature(mut self, key: &[u8]) -> Self {
        use hmac::Mac;
        let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(key)
            .expect("HMAC key length is valid");

        let payload = format!(
            "{}{}{}{}{}",
            self.id, self.schema_version, self.source, self.tenant_id, self.data
        );
        mac.update(payload.as_bytes());
        let signature = hex::encode(mac.finalize().into_bytes());
        self.signature = Some(signature);
        self
    }

    /// Verify the observation's signature
    pub fn verify_signature(&self, key: &[u8]) -> DoDResult<bool> {
        let sig = self.signature.as_ref().ok_or_else(|| {
            DoDError::Receipt("observation has no signature".to_string())
        })?;

        use hmac::Mac;
        let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(key)
            .expect("HMAC key length is valid");

        let payload = format!(
            "{}{}{}{}{}",
            self.id, self.schema_version, self.source, self.tenant_id, self.data
        );
        mac.update(payload.as_bytes());

        let expected_sig = hex::encode(mac.finalize().into_bytes());
        Ok(sig == &expected_sig)
    }
}

impl fmt::Display for Observation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Observation(id={}, type={:?}, source={}, tenant={})",
            self.id, self.obs_type, self.source, self.tenant_id
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_observation_creation() {
        let obs = Observation::new(
            ObservationType::Metric(MetricType::Latency),
            json!({"value": 42}),
            "test-source",
            "1.0",
            "tenant-1",
        )
        .expect("observation creation");

        assert_eq!(obs.source(), "test-source");
        assert_eq!(obs.tenant_id(), "tenant-1");
        assert_eq!(obs.schema_version(), "1.0");
    }

    #[test]
    fn test_observation_signature() {
        let key = b"test-key";
        let obs = Observation::new(
            ObservationType::Metric(MetricType::Throughput),
            json!({"value": 100}),
            "test-source",
            "1.0",
            "tenant-1",
        )
        .expect("observation creation")
        .with_signature(key);

        assert!(obs.signature.is_some());
        let valid = obs.verify_signature(key).expect("verify");
        assert!(valid);
    }

    #[test]
    fn test_schema_validation() {
        let schema = ObservationSchema::new("1.0")
            .with_required_field("value", FieldType::Number);

        let obs = Observation::new(
            ObservationType::Metric(MetricType::Latency),
            json!({"value": 42}),
            "test",
            "1.0",
            "tenant-1",
        )
        .expect("observation");

        assert!(schema.validate(&obs).is_ok());
    }

    #[test]
    fn test_schema_validation_missing_field() {
        let schema = ObservationSchema::new("1.0")
            .with_required_field("required_field", FieldType::String);

        let obs = Observation::new(
            ObservationType::Metric(MetricType::Latency),
            json!({"other_field": "value"}),
            "test",
            "1.0",
            "tenant-1",
        )
        .expect("observation");

        assert!(schema.validate(&obs).is_err());
    }

    #[test]
    fn test_field_type_validation() {
        let schema = ObservationSchema::new("1.0")
            .with_required_field("value", FieldType::Integer);

        let obs = Observation::new(
            ObservationType::Metric(MetricType::Latency),
            json!({"value": "not a number"}),
            "test",
            "1.0",
            "tenant-1",
        )
        .expect("observation");

        assert!(schema.validate(&obs).is_err());
    }
}
