//! Rice Quarantine: boundary enforcement before payload admission to LawObject.
//!
//! The Rice Quarantine pattern ensures that untrusted observations (raw strings, JSON, etc.)
//! are validated against a schema before they can be wrapped in `LawObject`. No observation
//! is automatically trusted — all must be explicitly validated and transformed into a
//! bounded, typed payload.
//!
//! # Pattern Flow
//!
//! ```text
//! Untyped Observation (String)
//!         ↓
//!    BoundarySchema::validate()
//!         ↓
//!    [Schema Check: Pass/Fail]
//!         ↓ (Pass)
//!    Quarantined Payload (T)
//!         ↓
//!    LawObject::new(payload, obligations)
//! ```

#[allow(unused_imports)]
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use thiserror::Error;

/// Error type for quarantine violations and schema validation failures.
#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum QuarantineError {
    /// Schema validation failed: observation does not match boundary.
    #[error("schema validation failed: {reason}")]
    ValidationFailed {
        /// The reason validation failed.
        reason: String,
    },

    /// Observation could not be deserialized into the target type.
    #[error("deserialization failed: {reason}")]
    DeserializationFailed {
        /// The reason deserialization failed.
        reason: String,
    },

    /// Observation failed a custom predicate check.
    #[error("predicate denied admission: {reason}")]
    PredicateDenied {
        /// The reason the predicate denied admission.
        reason: String,
    },

    /// Observation was rejected as empty or malformed.
    #[error("malformed observation: {reason}")]
    Malformed {
        /// The reason the observation was malformed.
        reason: String,
    },
}

/// Trait defining a schema boundary that can validate and transform observations into a payload type.
///
/// Implementations of `BoundarySchema` act as validators: they take an untyped string and
/// either admit it as a validated payload of type `T` or reject it with a reason.
///
/// This is the gating mechanism for the Rice Quarantine: only observations that pass
/// `validate()` can be used in the system.
pub trait BoundarySchema<T: Serialize + DeserializeOwned> {
    /// Validate an observation string against this schema.
    ///
    /// # Returns
    ///
    /// - `Ok(T)` if the observation is valid and admits to the payload type.
    /// - `Err(QuarantineError)` if validation fails.
    fn validate(&self, observation: &str) -> Result<T, QuarantineError>;
}

/// Rice Quarantine: the boundary enforcement boundary that admits or rejects observations.
///
/// A `RiceQuarantine<S, P>` holds a schema `S` and admits untyped observations (strings)
/// by validating them against that schema. Only observations that pass validation
/// are returned as quarantined payloads.
///
/// Type parameters:
/// - `S` — the schema type (implements `BoundarySchema<P>`).
/// - `P` — the payload type that results from successful validation.
pub struct RiceQuarantine<S, P: Serialize + DeserializeOwned> {
    schema: S,
    _payload: std::marker::PhantomData<P>,
}

impl<S, P> RiceQuarantine<S, P>
where
    S: BoundarySchema<P>,
    P: Serialize + DeserializeOwned,
{
    /// Create a new quarantine with the given schema.
    pub fn new(schema: S) -> Self {
        RiceQuarantine {
            schema,
            _payload: std::marker::PhantomData,
        }
    }

    /// Admit an observation: validate it against the schema and return the quarantined payload.
    ///
    /// # Arguments
    ///
    /// - `observation` — an untyped string (e.g., raw JSON, free text, etc.)
    ///
    /// # Returns
    ///
    /// - `Ok(P)` if the observation passes schema validation.
    /// - `Err(QuarantineError)` if validation fails or the observation is malformed.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let quarantine = RiceQuarantine::new(MySchema);
    /// let payload = quarantine.admit(r#"{"id":"123","action":"run"}"#)?;
    /// let law_obj = LawObject::new(payload, vec![...]);
    /// ```
    pub fn admit(&self, observation: &str) -> Result<P, QuarantineError> {
        if observation.trim().is_empty() {
            return Err(QuarantineError::Malformed {
                reason: "observation is empty".to_string(),
            });
        }
        self.schema.validate(observation)
    }

    /// Get a reference to the schema (for introspection or configuration).
    pub fn schema(&self) -> &S {
        &self.schema
    }
}

/// A simple JSON schema validator that deserializes and applies optional predicates.
///
/// This is a reference implementation of `BoundarySchema`. It deserializes the observation
/// as JSON into type `T`, then applies an optional predicate for domain-specific validation.
///
/// # Example
///
/// ```ignore
/// let schema = JsonBoundarySchema::new(|obj: &MyPayload| {
///     obj.id.len() > 0 && obj.action != "forbidden"
/// });
/// let quarantine = RiceQuarantine::new(schema);
/// let payload = quarantine.admit(raw_json)?;
/// ```
pub struct JsonBoundarySchema<T, F = fn(&T) -> bool>
where
    T: Serialize + DeserializeOwned,
    F: Fn(&T) -> bool,
{
    predicate: Option<F>,
    _payload: std::marker::PhantomData<T>,
}

impl<T> JsonBoundarySchema<T, fn(&T) -> bool>
where
    T: Serialize + DeserializeOwned,
{
    /// Create a new JSON schema validator with no predicate (accept all valid JSON).
    pub fn new() -> Self {
        JsonBoundarySchema {
            predicate: None,
            _payload: std::marker::PhantomData,
        }
    }
}

impl<T> Default for JsonBoundarySchema<T, fn(&T) -> bool>
where
    T: Serialize + DeserializeOwned,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T, F> JsonBoundarySchema<T, F>
where
    T: Serialize + DeserializeOwned,
    F: Fn(&T) -> bool,
{
    /// Create a JSON schema validator with a predicate function.
    pub fn with_predicate(predicate: F) -> Self {
        JsonBoundarySchema {
            predicate: Some(predicate),
            _payload: std::marker::PhantomData,
        }
    }
}

impl<T, F> BoundarySchema<T> for JsonBoundarySchema<T, F>
where
    T: Serialize + DeserializeOwned,
    F: Fn(&T) -> bool,
{
    fn validate(&self, observation: &str) -> Result<T, QuarantineError> {
        // Step 1: Parse to Value first, then deserialize
        let value: serde_json::Value = match serde_json::from_str(observation) {
            Ok(v) => v,
            Err(e) => {
                return Err(QuarantineError::DeserializationFailed {
                    reason: e.to_string(),
                })
            }
        };

        // Step 2: Convert Value to target type T
        let payload: T = match serde_json::from_value(value) {
            Ok(p) => p,
            Err(e) => {
                return Err(QuarantineError::DeserializationFailed {
                    reason: format!("type conversion failed: {}", e),
                })
            }
        };

        // Step 3: Apply predicate if present
        if let Some(ref pred) = self.predicate {
            if !pred(&payload) {
                return Err(QuarantineError::PredicateDenied {
                    reason: "predicate check failed".to_string(),
                });
            }
        }

        Ok(payload)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
    struct TestPayload {
        id: String,
        action: String,
    }

    #[test]
    fn quarantine_admits_valid_json() {
        let schema = JsonBoundarySchema::<TestPayload>::new();
        let quarantine = RiceQuarantine::new(schema);

        let observation = r#"{"id":"123","action":"run"}"#;
        let result = quarantine.admit(observation);

        assert!(result.is_ok());
        let payload = result.unwrap();
        assert_eq!(payload.id, "123");
        assert_eq!(payload.action, "run");
    }

    #[test]
    fn quarantine_rejects_malformed_json() {
        let schema = JsonBoundarySchema::<TestPayload>::new();
        let quarantine = RiceQuarantine::new(schema);

        let observation = r#"{"id":"123","action":"run"#; // Missing closing brace
        let result = quarantine.admit(observation);

        assert!(result.is_err());
        match result.unwrap_err() {
            QuarantineError::DeserializationFailed { .. } => {}
            _ => panic!("expected DeserializationFailed"),
        }
    }

    #[test]
    fn quarantine_rejects_empty_observation() {
        let schema = JsonBoundarySchema::<TestPayload>::new();
        let quarantine = RiceQuarantine::new(schema);

        let result = quarantine.admit("");

        assert!(result.is_err());
        match result.unwrap_err() {
            QuarantineError::Malformed { reason } => {
                assert!(reason.contains("empty"));
            }
            _ => panic!("expected Malformed"),
        }
    }

    #[test]
    fn quarantine_rejects_whitespace_only() {
        let schema = JsonBoundarySchema::<TestPayload>::new();
        let quarantine = RiceQuarantine::new(schema);

        let result = quarantine.admit("   \n\t  ");

        assert!(result.is_err());
        match result.unwrap_err() {
            QuarantineError::Malformed { .. } => {}
            _ => panic!("expected Malformed"),
        }
    }

    #[test]
    fn quarantine_with_predicate_admits_valid_payload() {
        let schema = JsonBoundarySchema::with_predicate(|p: &TestPayload| {
            !p.id.is_empty() && p.action == "run"
        });
        let quarantine = RiceQuarantine::new(schema);

        let observation = r#"{"id":"456","action":"run"}"#;
        let result = quarantine.admit(observation);

        assert!(result.is_ok());
        let payload = result.unwrap();
        assert_eq!(payload.id, "456");
    }

    #[test]
    fn quarantine_with_predicate_rejects_payload() {
        let schema = JsonBoundarySchema::with_predicate(|p: &TestPayload| {
            !p.id.is_empty() && p.action == "run"
        });
        let quarantine = RiceQuarantine::new(schema);

        let observation = r#"{"id":"789","action":"stop"}"#;
        let result = quarantine.admit(observation);

        assert!(result.is_err());
        match result.unwrap_err() {
            QuarantineError::PredicateDenied { .. } => {}
            _ => panic!("expected PredicateDenied"),
        }
    }

    #[test]
    fn quarantine_payload_integrates_with_law_object() {
        use crate::{
            law::{LawObject, Obligation},
            lifecycle::Raw,
        };

        let schema = JsonBoundarySchema::<TestPayload>::new();
        let quarantine = RiceQuarantine::new(schema);

        let observation = r#"{"id":"999","action":"execute"}"#;
        let payload = quarantine.admit(observation).expect("should admit");

        // Wire into LawObject with explicit type parameters
        // LawObject<Payload, Stage, Law>
        #[derive(Debug)]
        struct TestLaw;

        let law_obj = LawObject::<TestPayload, Raw, TestLaw>::new(
            payload,
            vec![Obligation::Precondition {
                predicate_id: "check_id".to_string(),
                params_hash: [0u8; 32],
            }],
        );

        assert_eq!(law_obj.payload().id, "999");
        assert_eq!(law_obj.obligations().len(), 1);
    }
}
