//! `chatmangpt.sr.result.v1` JSON envelope — promoted from
//! `chatmangpt-mcpp-v2-cell::speckit-ralph::Envelope` per
//! `portfolio-obl-0002-promote-v2-constraints-into-canonical-mcpp-mcpp`.
//!
//! Every MCPP CLI command emits one of these to stdout. JSON is the
//! authoritative output; pretty-printed forms are non-authoritative.

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

pub const SR_RESULT_SCHEMA: &str = "chatmangpt.sr.result.v1";

/// Status discriminant carried by every envelope. Stable strings; consumers
/// match on them.
pub mod status {
    pub const PASS: &str = "pass";
    pub const FAIL: &str = "fail";
    pub const VERIFIED: &str = "verified";
    pub const EMITTED: &str = "emitted";
    pub const ACCEPTED: &str = "accepted";
    pub const STOPPED: &str = "stopped";
    pub const INVALID: &str = "invalid";
    pub const DISPATCHED: &str = "dispatched";
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Envelope {
    pub schema: String,
    pub command: String,
    pub status: String,
    pub target: String,
    pub line_status: String,
    pub work_unit: String,
    #[serde(default)]
    pub data: Value,
    #[serde(default)]
    pub errors: Vec<Value>,
    #[serde(default)]
    pub warnings: Vec<Value>,
    #[serde(default)]
    pub next: Option<Value>,
}

impl Envelope {
    pub fn new(command: impl Into<String>, target: impl Into<String>) -> Self {
        Self {
            schema: SR_RESULT_SCHEMA.into(),
            command: command.into(),
            status: status::PASS.into(),
            target: target.into(),
            line_status: "running".into(),
            work_unit: String::new(),
            data: json!({}),
            errors: Vec::new(),
            warnings: Vec::new(),
            next: None,
        }
    }

    /// Serialize to a single canonical JSON line (no trailing newline) — the
    /// primary stdout format for MCPP CLI commands.
    pub fn to_json_line(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| "{}".into())
    }

    pub fn fail(mut self, message: impl Into<String>, failure_class: impl Into<String>) -> Self {
        self.status = status::FAIL.into();
        self.errors.push(json!({
            "message": message.into(),
            "failure_class": failure_class.into(),
        }));
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn envelope_default_status_is_pass() {
        let e = Envelope::new("mcpp.smoke", "mcpp-mcpp");
        assert_eq!(e.schema, SR_RESULT_SCHEMA);
        assert_eq!(e.status, status::PASS);
    }

    #[test]
    fn envelope_serializes_to_json_with_required_fields() {
        let e = Envelope::new("mcpp.smoke", "mcpp-mcpp");
        let line = e.to_json_line();
        let parsed: Value = serde_json::from_str(&line).expect("parse json");
        for k in [
            "schema",
            "command",
            "status",
            "target",
            "line_status",
            "work_unit",
        ] {
            assert!(parsed.get(k).is_some(), "missing key {k}");
        }
        assert_eq!(parsed["schema"], json!(SR_RESULT_SCHEMA));
    }

    #[test]
    fn fail_records_error_and_failure_class() {
        let e = Envelope::new("mcpp.smoke", "mcpp-mcpp").fail("boom", "RECEIPT_DEFECT");
        assert_eq!(e.status, status::FAIL);
        assert_eq!(e.errors.len(), 1);
        assert_eq!(e.errors[0]["failure_class"], json!("RECEIPT_DEFECT"));
    }
}
