//! JSON-first result envelope.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/speckit-ralph`
//! (portfolio-obl-0002, promotion id `json-result-envelope`).
//!
//! Outer protocol contract for canonical MCPP. All command results,
//! Andon events, receipts, and invocation classifications fit inside
//! this envelope.
//!
//! Doctrine:
//! - JSON is the default output. Human output is opt-in only.
//! - Failure paths emit valid JSON envelopes with non-empty `errors`.
//! - Tool/command success encoded as `status` is not completion.
//!   Receipt verification is completion.

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

/// Stable schema identifier for the canonical MCPP result envelope.
pub const ENVELOPE_SCHEMA: &str = "chatmangpt.mcpp.result.v1";

/// Result envelope used by every canonical MCPP command.
///
/// Required fields: `schema`, `command`, `status`, `target`, `data`,
/// `errors`, `warnings`, `next`. The shape mirrors
/// `chatmangpt.sr.result.v1` from the absorbed v2 cell so downstream
/// consumers can keep the same parsing path.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Envelope {
    pub schema: String,
    pub command: String,
    pub status: String,
    pub target: String,
    pub data: Value,
    pub errors: Vec<Value>,
    pub warnings: Vec<Value>,
    pub next: Value,
}

impl Envelope {
    /// Construct a passing envelope for `command` against `target`.
    pub fn pass(command: &str, target: &str) -> Self {
        Envelope {
            schema: ENVELOPE_SCHEMA.into(),
            command: command.into(),
            status: "pass".into(),
            target: target.into(),
            data: json!({}),
            errors: Vec::new(),
            warnings: Vec::new(),
            next: Value::Null,
        }
    }

    /// Construct a failing envelope tagged with a class and message.
    pub fn fail(command: &str, target: &str, class: &str, message: &str) -> Self {
        let mut env = Envelope::pass(command, target);
        env.status = "fail".into();
        env.errors.push(json!({
            "class": class,
            "message": message,
        }));
        env
    }

    /// Attach structured data.
    pub fn with_data(mut self, data: Value) -> Self {
        self.data = data;
        self
    }

    /// Attach a `next` hint pointing at the next admissible command.
    pub fn with_next(mut self, command: &str, reason: &str) -> Self {
        self.next = json!({
            "command": command,
            "reason": reason,
        });
        self
    }

    /// Push a warning record.
    pub fn warn(mut self, value: Value) -> Self {
        self.warnings.push(value);
        self
    }

    /// Serialize to compact JSON.
    pub fn to_json(&self) -> String {
        serde_json::to_string(self).expect("envelope serialization is infallible")
    }

    /// Serialize to pretty JSON (opt-in).
    pub fn to_json_pretty(&self) -> String {
        serde_json::to_string_pretty(self).expect("envelope serialization is infallible")
    }
}

/// Emit a passing envelope to stdout as JSON. JSON-first by default.
pub fn emit_pass(command: &str, target: &str, data: Value) {
    let env = Envelope::pass(command, target).with_data(data);
    println!("{}", env.to_json());
}

/// Emit a failing envelope to stdout as JSON. Failure paths must still
/// produce valid JSON.
pub fn emit_fail(command: &str, target: &str, class: &str, message: &str) {
    let env = Envelope::fail(command, target, class, message);
    println!("{}", env.to_json());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pass_envelope_carries_required_fields() {
        let env = Envelope::pass("autonomics.run", "mcpp");
        assert_eq!(env.schema, ENVELOPE_SCHEMA);
        assert_eq!(env.command, "autonomics.run");
        assert_eq!(env.status, "pass");
        assert_eq!(env.target, "mcpp");
        assert!(env.errors.is_empty());
        assert!(env.warnings.is_empty());
        assert!(env.next.is_null());
        assert!(env.data.is_object());
    }

    #[test]
    fn fail_envelope_is_valid_json_with_error() {
        let env = Envelope::fail("autonomics.run", "mcpp", "POLICY_DEFECT", "shacl violation");
        assert_eq!(env.status, "fail");
        let parsed: Value = serde_json::from_str(&env.to_json()).expect("must be valid JSON");
        assert_eq!(parsed["status"], "fail");
        assert_eq!(parsed["errors"][0]["class"], "POLICY_DEFECT");
        assert_eq!(parsed["errors"][0]["message"], "shacl violation");
    }

    #[test]
    fn schema_is_stable_constant() {
        assert_eq!(ENVELOPE_SCHEMA, "chatmangpt.mcpp.result.v1");
    }

    #[test]
    fn with_next_records_admissible_followup() {
        let env = Envelope::pass("autonomics.run", "mcpp")
            .with_next("mcpp autonomics receipt verify", "verify result");
        let parsed: Value = serde_json::from_str(&env.to_json()).unwrap();
        assert_eq!(parsed["next"]["command"], "mcpp autonomics receipt verify");
    }

    #[test]
    fn round_trip_serialization_preserves_fields() {
        let env = Envelope::pass("x.y", "mcpp")
            .with_data(json!({"k": 1, "ok": true}))
            .warn(json!({"class": "noisy", "message": "advisory"}));
        let bytes = env.to_json();
        let back: Envelope = serde_json::from_str(&bytes).expect("deserializes");
        assert_eq!(back.command, "x.y");
        assert_eq!(back.data["k"], 1);
        assert_eq!(back.warnings.len(), 1);
    }
}
