//! MCP tool adapter: `ggen.construct`
//!
//! Exposes ggen's code generation pipeline as an MCP tool that can be invoked
//! from mcpp agents across process boundaries. Implements the ggen.construct
//! tool specification from PLAN_INTEGRATION.md Section 2.2.
//!
//! ## Purpose
//!
//! Bridge between mcpp (Agent-to-Agent routing) and ggen (RDF → Code generation).
//! When an mcpp agent needs to manufacture code, it invokes ggen.construct via
//! this MCP tool.
//!
//! ## Interface
//!
//! **Input:**
//! ```json
//! {
//!   "task_id": "uuid-v4",
//!   "jtbd": "Job-to-be-done description",
//!   "avatar": "invoking-agent-name",
//!   "ontology_uri": "path/to/ontology.ttl or https://...",
//!   "target_language": "rust|python|ts|erlang",
//!   "output_format": "source|wasm|docker"
//! }
//! ```
//!
//! **Output:**
//! ```json
//! {
//!   "status": "success|error",
//!   "task_id": "echo-uuid",
//!   "jtbd": "echo-jtbd",
//!   "avatar": "echo-avatar",
//!   "message": "human-readable result",
//!   "result": {
//!     "artifact_path": "relative/path/to/artifact",
//!     "artifact_hash": "blake3:<64 hex>",
//!     "receipt_path": ".ggen/receipts/rcpt-<id>-<timestamp>.json",
//!     "proof_gates": {
//!       "compiler": "pass|fail",
//!       "lint": "pass|fail",
//!       "tests": "pass|fail",
//!       "slo": "pass|fail"
//!     },
//!     "generation_time_ms": 3421,
//!     "error_details": null
//!   }
//! }
//! ```
//!
//! ## OCEL Event Emitted
//!
//! At the boundary, an OCEL event is emitted:
//! ```json
//! {
//!   "event_id": "uuid-v4",
//!   "activity": "a2a.mcp.tool.invoked",
//!   "timestamp": "RFC3339",
//!   "objects": {
//!     "task": "task_id",
//!     "tool": "ggen.construct",
//!     "invoker": "avatar"
//!   },
//!   "attributes": {
//!     "tool.duration_ms": <int>,
//!     "artifact.hash": "blake3:<hex>",
//!     "receipt.path": ".ggen/receipts/..."
//!   }
//! }
//! ```

use serde::{Deserialize, Serialize};
use std::time::Instant;

/// Input parameters for `ggen.construct` MCP tool.
#[derive(Debug, Clone, Deserialize)]
pub struct GgenConstructInput {
    /// UUID v4 for cross-boundary tracing (RFC 4122).
    pub task_id: String,

    /// Job-to-be-done: human-readable intent for generation.
    pub jtbd: String,

    /// Agent identity invoking this tool.
    pub avatar: String,

    /// RDF ontology URI or filesystem path (.specify/*.ttl source).
    pub ontology_uri: String,

    /// Code generation target language.
    #[serde(default)]
    pub target_language: String,

    /// Artifact packaging format.
    #[serde(default)]
    pub output_format: String,
}

/// Proof gate result (pass/fail).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofGateResult {
    pub compiler: String,
    pub lint: String,
    pub tests: String,
    pub slo: String,
}

/// Success result payload for ggen.construct.
#[derive(Debug, Clone, Serialize)]
pub struct GgenConstructResult {
    /// Relative path to generated artifact.
    pub artifact_path: String,

    /// BLAKE3 hash of artifact.
    pub artifact_hash: String,

    /// Path to signed receipt file (`.ggen/receipts/rcpt-<id>-<ts>.json`).
    pub receipt_path: String,

    /// Proof gate results.
    pub proof_gates: ProofGateResult,

    /// Generation time in milliseconds.
    pub generation_time_ms: u64,

    /// Error details (null on success).
    pub error_details: Option<String>,
}

/// Output of `ggen.construct` MCP tool.
#[derive(Debug, Serialize)]
pub struct GgenConstructOutput {
    /// "success" or "error".
    pub status: String,

    /// Echo task_id from request.
    pub task_id: String,

    /// Echo jtbd from request.
    pub jtbd: String,

    /// Echo avatar from request.
    pub avatar: String,

    /// Human-readable message.
    pub message: String,

    /// Payload (only present on success).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<GgenConstructResult>,
}

/// Tool definition for MCP server registration.
pub fn tool_definition() -> serde_json::Value {
    serde_json::json!({
        "name": "ggen.construct",
        "description": "Construct a target artifact using ggen A2A pipeline (RDF ontology → Code via μ₁–μ₅)",
        "input_schema": {
            "type": "object",
            "properties": {
                "task_id": {
                    "type": "string",
                    "description": "UUID v4 (RFC 4122) for tracing across boundaries"
                },
                "jtbd": {
                    "type": "string",
                    "description": "Job-to-be-done: human-readable intent for generation"
                },
                "avatar": {
                    "type": "string",
                    "description": "Agent identity invoking this tool (e.g., mcpp-router-01)"
                },
                "ontology_uri": {
                    "type": "string",
                    "description": "RDF ontology URI (.specify/*.ttl source) or filesystem path"
                },
                "target_language": {
                    "type": "string",
                    "enum": ["rust", "python", "ts", "erlang"],
                    "description": "Code generation target language (default: rust)"
                },
                "output_format": {
                    "type": "string",
                    "enum": ["source", "wasm", "docker"],
                    "description": "Artifact packaging format (default: source)"
                }
            },
            "required": ["task_id", "jtbd", "avatar", "ontology_uri"]
        }
    })
}

/// Execute the ggen.construct tool.
///
/// This function serves as the MCP tool handler. It:
/// 1. Validates input parameters
/// 2. Invokes ggen pipeline (via subprocess or library import)
/// 3. Collects proof gate results
/// 4. Emits OCEL event at boundary
/// 5. Returns response with receipt reference
pub fn execute(input: GgenConstructInput) -> GgenConstructOutput {
    let start = Instant::now();

    // Validate required fields
    if input.task_id.is_empty() {
        return GgenConstructOutput {
            status: "error".to_string(),
            task_id: input.task_id,
            jtbd: input.jtbd,
            avatar: input.avatar,
            message: "task_id must be non-empty UUID".to_string(),
            result: None,
        };
    }

    if input.ontology_uri.is_empty() {
        return GgenConstructOutput {
            status: "error".to_string(),
            task_id: input.task_id,
            jtbd: input.jtbd,
            avatar: input.avatar,
            message: "ontology_uri must be non-empty".to_string(),
            result: None,
        };
    }

    let duration_ms = start.elapsed().as_millis() as u64;

    // OCEL boundary-crossing evidence: the tool WAS invoked. We preserve this
    // log even though we refuse to fake a result — the invocation is real
    // (genuine boundary crossing), only the construction is not yet wired.
    tracing::warn!(
        event = "a2a.mcp.tool.invoked",
        task_id = %input.task_id,
        tool = "ggen.construct",
        invoker = %input.avatar,
        duration_ms = duration_ms,
        status = "unimplemented",
        "ggen.construct invoked but the μ₁–μ₅ pipeline is not wired through this A2A adapter"
    );

    // Oracle Gap closure: this adapter does NOT run the real μ₁–μ₅ pipeline.
    // Reporting "success" for work that never happened is contract drift — any
    // caller, receipt, or provenance record would capture a fabricated outcome
    // (a synthetic artifact hash, a dummy receipt path, all-pass proof gates).
    // The only honest response is an explicit failure that:
    //   - does NOT claim status "success",
    //   - does NOT emit or reference a fabricated receipt path,
    //   - does NOT report synthetic artifact hashes or all-pass proof gates,
    //   - directs the caller to the real construction path.
    //
    // This mirrors the MCP-side closure in `mcp_server.rs` (commit 3a7f1f7e),
    // which returns an explicit error rather than a hardcoded success.
    //
    // TODO(μ₁–μ₅): wire this adapter to the real ggen pipeline (subprocess to
    // `ggen sync` or FFI into ggen-core), then return a real receipt + hashes.
    GgenConstructOutput {
        status: "unimplemented".to_string(),
        task_id: input.task_id,
        jtbd: input.jtbd,
        avatar: input.avatar,
        message: "ggen.construct is not yet wired to the μ₁–μ₅ pipeline through \
                  this A2A adapter. No artifact was generated and no receipt was \
                  emitted. Use the real construction path: run `ggen sync` (the \
                  μ₁–μ₅ actuator) to construct artifacts from your ontology."
            .to_string(),
        result: None,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
/// Test module: unwrap()/expect() acceptable after validating JSON structure in tests.
mod tests {
    use super::*;

    #[test]
    fn test_tool_definition_valid_schema() {
        let def = tool_definition();
        assert_eq!(def["name"], "ggen.construct");
        assert!(def.get("description").is_some());
        assert!(def.get("input_schema").is_some());

        let schema = &def["input_schema"];
        assert_eq!(schema["type"], "object");
        assert!(schema["properties"]["task_id"].get("type").is_some());
        assert!(schema["properties"]["ontology_uri"].get("type").is_some());

        let required = &schema["required"];
        assert!(required.as_array().unwrap().contains(&"task_id".into()));
        assert!(required
            .as_array()
            .unwrap()
            .contains(&"ontology_uri".into()));
    }

    /// Oracle Gap closure: with valid input, the adapter must FAIL LOUD rather
    /// than fake success. The μ₁–μ₅ pipeline is not wired through this adapter,
    /// so it must not claim "success", must not emit a result payload (no
    /// fabricated receipt path, no synthetic artifact hash, no all-pass proof
    /// gates), and must direct the caller to the real construction path.
    #[test]
    fn test_execute_valid_input_fails_loud_not_fake_success() {
        let input = GgenConstructInput {
            task_id: "123e4567-e89b-12d3-a456-426614174000".to_string(),
            jtbd: "Generate Rust service".to_string(),
            avatar: "mcpp-router-01".to_string(),
            ontology_uri: ".specify/myapp.ttl".to_string(),
            target_language: "rust".to_string(),
            output_format: "source".to_string(),
        };

        let output = execute(input);

        // Must NOT claim success.
        assert_ne!(output.status, "success");
        assert_eq!(output.status, "unimplemented");

        // Must NOT emit a result payload (no fabricated receipt/hash/gates).
        assert!(
            output.result.is_none(),
            "honest refusal must not carry a result payload"
        );

        // Must direct the caller to the real construction path.
        assert!(
            output.message.contains("ggen sync"),
            "message must point to the real μ₁–μ₅ actuator: {}",
            output.message
        );

        // The serialized output must not contain any fabricated receipt path
        // or synthetic artifact hash.
        let json = serde_json::to_string(&output).expect("serialization should succeed");
        assert!(
            !json.contains("rcpt-simulated"),
            "must not reference a fabricated receipt path"
        );
        assert!(
            !json.contains("blake3:"),
            "must not report a synthetic artifact hash"
        );
    }

    #[test]
    fn test_execute_missing_task_id() {
        let input = GgenConstructInput {
            task_id: "".to_string(),
            jtbd: "Generate Rust service".to_string(),
            avatar: "mcpp-router-01".to_string(),
            ontology_uri: ".specify/myapp.ttl".to_string(),
            target_language: "rust".to_string(),
            output_format: "source".to_string(),
        };

        let output = execute(input);

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(output.message.contains("task_id"));
    }

    #[test]
    fn test_execute_missing_ontology_uri() {
        let input = GgenConstructInput {
            task_id: "123e4567-e89b-12d3-a456-426614174000".to_string(),
            jtbd: "Generate Rust service".to_string(),
            avatar: "mcpp-router-01".to_string(),
            ontology_uri: "".to_string(),
            target_language: "rust".to_string(),
            output_format: "source".to_string(),
        };

        let output = execute(input);

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(output.message.contains("ontology_uri"));
    }

    /// Even with empty (defaultable) language/format, the adapter must still
    /// refuse honestly — defaults do not unlock a fake-success path.
    #[test]
    fn test_execute_defaults_still_fails_loud() {
        let input = GgenConstructInput {
            task_id: "123e4567-e89b-12d3-a456-426614174000".to_string(),
            jtbd: "Generate artifact".to_string(),
            avatar: "mcpp-router-01".to_string(),
            ontology_uri: ".specify/myapp.ttl".to_string(),
            target_language: "".to_string(),
            output_format: "".to_string(),
        };

        let output = execute(input);

        assert_ne!(output.status, "success");
        assert_eq!(output.status, "unimplemented");
        assert!(output.result.is_none());
        assert!(output.message.contains("ggen sync"));
    }

    #[test]
    fn test_output_serialization() {
        let output = GgenConstructOutput {
            status: "success".to_string(),
            task_id: "123e4567-e89b-12d3-a456-426614174000".to_string(),
            jtbd: "test".to_string(),
            avatar: "test-avatar".to_string(),
            message: "Success".to_string(),
            result: Some(GgenConstructResult {
                artifact_path: "target/artifact.rs".to_string(),
                artifact_hash: "blake3:abc123".to_string(),
                receipt_path: ".ggen/receipts/rcpt-123.json".to_string(),
                proof_gates: ProofGateResult {
                    compiler: "pass".to_string(),
                    lint: "pass".to_string(),
                    tests: "pass".to_string(),
                    slo: "pass".to_string(),
                },
                generation_time_ms: 1000,
                error_details: None,
            }),
        };

        let json = serde_json::to_string(&output).expect("serialization should succeed");
        assert!(json.contains("\"status\":\"success\""));
        assert!(json.contains("\"artifact_hash\":\"blake3:abc123\""));
    }
}
