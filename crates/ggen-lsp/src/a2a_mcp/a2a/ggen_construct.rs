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

use ggen_engine::sync::{SyncOptions, RECEIPT_REL_PATH};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
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

    // `ontology_uri` is a filesystem path to the project's ontology Turtle
    // file; the project root (the directory holding `ggen.toml`) is its
    // parent. Remote URIs are refused: this adapter runs the real in-process
    // μ₁–μ₅ pipeline (`ggen_engine::sync::sync`, the same call the MCP-side
    // sibling handler in `mcp_server.rs` makes), which reads from disk.
    if input.ontology_uri.starts_with("http://") || input.ontology_uri.starts_with("https://") {
        tracing::warn!(
            event = "a2a.mcp.tool.invoked",
            task_id = %input.task_id,
            tool = "ggen.construct",
            invoker = %input.avatar,
            duration_ms = duration_ms,
            status = "REFUSED:REMOTE_ONTOLOGY_URI",
            "ggen.construct refused: remote ontology URIs are not supported"
        );
        return GgenConstructOutput {
            status: "error".to_string(),
            task_id: input.task_id,
            jtbd: input.jtbd,
            avatar: input.avatar,
            message: "REFUSED:REMOTE_ONTOLOGY_URI — ontology_uri must be a filesystem \
                      path to a Turtle file inside a ggen project (its parent directory \
                      must hold ggen.toml). Fetch the ontology to disk first."
                .to_string(),
            result: None,
        };
    }

    let ontology_path = PathBuf::from(&input.ontology_uri);
    let base_path = match ontology_path.canonicalize() {
        Ok(p) => p
            .parent()
            .map(std::path::Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from(".")),
        Err(e) => {
            let message = format!(
                "REFUSED:ONTOLOGY_UNREADABLE — ontology_uri `{}` cannot be read: {e}",
                input.ontology_uri
            );
            return error_output(input, message);
        }
    };

    if !base_path.join("ggen.toml").exists() {
        return error_output(
            input,
            format!(
                "REFUSED:NO_PROJECT_ROOT — no ggen.toml in `{}`. ggen.construct \
                 requires a ggen project context (the ontology's parent directory \
                 must hold ggen.toml), exactly like the `ggen.construct` MCP tool.",
                base_path.display()
            ),
        );
    }

    // Real μ₁–μ₅ actuation: load ggen.toml + ontology, enrich/extract, render
    // via Tera, write outputs, and chain a praxis-core receipt over the
    // payload at `.ggen-v2/receipt.json`. Same engine entrypoint as the
    // MCP-side handler — one route engine, two transports, no drift.
    let sync_started = Instant::now();
    let report = match ggen_engine::sync::sync(&base_path, SyncOptions::default()) {
        Ok(r) => r,
        Err(e) => {
            tracing::warn!(
                event = "a2a.mcp.tool.invoked",
                task_id = %input.task_id,
                tool = "ggen.construct",
                invoker = %input.avatar,
                duration_ms = start.elapsed().as_millis() as u64,
                status = "error",
                "ggen.construct pipeline failed"
            );
            return error_output(input, format!("Generation pipeline failed: {e}"));
        }
    };
    let generation_time_ms = sync_started.elapsed().as_millis() as u64;

    // Artifact identity: first written output, hashed from the real bytes on
    // disk (never synthesized).
    let artifact_rel = report
        .written
        .first()
        .map(|p| p.display().to_string())
        .unwrap_or_default();
    let artifact_hash = report
        .written
        .first()
        .and_then(|p| std::fs::read(base_path.join(p)).ok())
        .map(|bytes| format!("blake3:{}", blake3::hash(&bytes).to_hex()))
        .unwrap_or_default();

    tracing::info!(
        event = "a2a.mcp.tool.invoked",
        task_id = %input.task_id,
        tool = "ggen.construct",
        invoker = %input.avatar,
        duration_ms = start.elapsed().as_millis() as u64,
        files_written = report.written.len(),
        graph_hash = %report.graph_hash_hex,
        status = "success",
        "ggen.construct completed via the real μ₁–μ₅ sync pipeline"
    );

    let _ = duration_ms;
    GgenConstructOutput {
        status: "success".to_string(),
        task_id: input.task_id,
        jtbd: input.jtbd,
        avatar: input.avatar,
        message: format!(
            "Constructed {} file(s) from `{}` via the μ₁–μ₅ sync pipeline.",
            report.written.len(),
            input.ontology_uri
        ),
        result: Some(GgenConstructResult {
            artifact_path: artifact_rel,
            artifact_hash,
            receipt_path: RECEIPT_REL_PATH.to_string(),
            // Honest gate reporting: the sync pipeline validates the graph and
            // gates (law/SHACL) but does NOT compile, lint, or run consumer
            // tests — fabricating "pass" would be an Oracle Gap. Callers
            // that need compiler/lint/test verdicts must run them against the
            // written outputs themselves.
            proof_gates: ProofGateResult {
                compiler: "not_run".to_string(),
                lint: "not_run".to_string(),
                tests: "not_run".to_string(),
                slo: "not_run".to_string(),
            },
            generation_time_ms,
            error_details: None,
        }),
    }
}

/// Typed refusal output preserving the request echo fields.
fn error_output(input: GgenConstructInput, message: String) -> GgenConstructOutput {
    GgenConstructOutput {
        status: "error".to_string(),
        task_id: input.task_id,
        jtbd: input.jtbd,
        avatar: input.avatar,
        message,
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

    /// Scaffold a minimal real ggen project in a tempdir: ggen.toml +
    /// ontology.ttl + one static Tera template. No mocks — `execute` runs the
    /// real `ggen_engine::sync::sync` against these real files.
    fn scaffold_project() -> (tempfile::TempDir, std::path::PathBuf) {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path().to_path_buf();
        std::fs::write(
            root.join("ggen.toml"),
            "[project]\nname = \"a2a-construct-test\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
        )
        .expect("write ggen.toml");
        std::fs::write(
            root.join("ontology.ttl"),
            "@prefix ex: <http://example.com/ns#> .\nex:thing a ex:Thing .\n",
        )
        .expect("write ontology.ttl");
        std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
        std::fs::write(
            root.join("templates").join("hello.tmpl"),
            "---\nto: generated/hello.txt\n---\nHello from ggen.construct\n",
        )
        .expect("write template");
        (dir, root)
    }

    fn input_for(ontology_uri: String) -> GgenConstructInput {
        GgenConstructInput {
            task_id: "123e4567-e89b-12d3-a456-426614174000".to_string(),
            jtbd: "Generate a greeting artifact".to_string(),
            avatar: "mcpp-router-01".to_string(),
            ontology_uri,
            target_language: "rust".to_string(),
            output_format: "source".to_string(),
        }
    }

    /// Chicago: real project on disk, real μ₁–μ₅ pipeline, real output file
    /// and receipt asserted on disk. Status "success" only because real work
    /// happened.
    #[test]
    fn test_execute_real_pipeline_generates_and_hashes_artifact() {
        let (_dir, root) = scaffold_project();
        let ontology_uri = root.join("ontology.ttl").display().to_string();

        let output = execute(input_for(ontology_uri));

        assert_eq!(output.status, "success", "message: {}", output.message);
        let result = output.result.expect("success must carry a result payload");

        // Real file written at the declared path.
        let artifact_path = root.join(&result.artifact_path);
        assert!(
            artifact_path.exists(),
            "artifact must exist on disk: {}",
            artifact_path.display()
        );
        let body = std::fs::read_to_string(&artifact_path).expect("read artifact");
        assert_eq!(body, "Hello from ggen.construct\n");

        // Hash is real BLAKE3 of the real bytes, not synthesized.
        let expected = format!("blake3:{}", blake3::hash(body.as_bytes()).to_hex());
        assert_eq!(result.artifact_hash, expected);

        // Receipt: the real chained receipt written by sync.
        assert_eq!(result.receipt_path, ".ggen-v2/receipt.json");
        assert!(
            root.join(".ggen-v2/receipt.json").exists(),
            "sync must have written the real receipt"
        );

        // Honest gate reporting: the pipeline did not compile/lint/test.
        assert_eq!(result.proof_gates.compiler, "not_run");
        assert_eq!(result.proof_gates.lint, "not_run");
        assert_eq!(result.proof_gates.tests, "not_run");
        assert!(result.generation_time_ms < 60_000);
    }

    /// A remote ontology URI is a typed refusal — never a fabricated
    /// generation and never a crash.
    #[test]
    fn test_execute_remote_ontology_uri_is_typed_refusal() {
        let output = execute(input_for("https://example.com/ontology.ttl".to_string()));

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(
            output.message.contains("REFUSED:REMOTE_ONTOLOGY_URI"),
            "typed refusal codes required: {}",
            output.message
        );
    }

    /// An ontology path whose parent lacks ggen.toml is a typed refusal with
    /// the fix (create a ggen project around the ontology).
    #[test]
    fn test_execute_no_project_root_is_typed_refusal() {
        let dir = tempfile::tempdir().expect("tempdir");
        let ttl = dir.path().join("ontology.ttl");
        std::fs::write(&ttl, "@prefix ex: <http://example.com/ns#> .\n").expect("write ttl");

        let output = execute(input_for(ttl.display().to_string()));

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(
            output.message.contains("REFUSED:NO_PROJECT_ROOT"),
            "typed refusal code required: {}",
            output.message
        );
    }

    /// An unreadable ontology path is a typed refusal, not a panic.
    #[test]
    fn test_execute_unreadable_ontology_is_typed_refusal() {
        let output = execute(input_for("/nonexistent/a2a/ontology.ttl".to_string()));

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(
            output.message.contains("REFUSED:ONTOLOGY_UNREADABLE"),
            "typed refusal code required: {}",
            output.message
        );
    }

    /// A structurally invalid project (ontology missing while ggen.toml
    /// references it) surfaces the pipeline's own typed error, fail-closed —
    /// never a fake success.
    #[test]
    fn test_execute_pipeline_failure_fails_closed_with_typed_error() {
        let dir = tempfile::tempdir().expect("tempdir");
        let root = dir.path().to_path_buf();
        std::fs::write(
            root.join("ggen.toml"),
            "[project]\nname = \"broken\"\n\n[ontology]\nsource = \"missing.ttl\"\n\n[templates]\ndir = \"templates\"\n",
        )
        .expect("write ggen.toml");
        // ontology.ttl deliberately NOT written.
        let ttl = root.join("ontology.ttl");
        std::fs::write(&ttl, "").expect("write empty ttl so root-resolution passes");

        let output = execute(input_for(ttl.display().to_string()));

        assert_eq!(output.status, "error");
        assert!(output.result.is_none());
        assert!(
            output.message.contains("Generation pipeline failed"),
            "pipeline error must surface: {}",
            output.message
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

    /// Even with empty (defaultable) language/format, the pipeline still runs
    /// for a real project — defaults do not unlock or block real construction.
    #[test]
    fn test_execute_defaults_run_real_pipeline() {
        let (_dir, root) = scaffold_project();
        let ontology_uri = root.join("ontology.ttl").display().to_string();
        let mut input = input_for(ontology_uri);
        input.target_language = String::new();
        input.output_format = String::new();

        let output = execute(input);

        assert_eq!(output.status, "success", "message: {}", output.message);
        let result = output.result.expect("success carries result");
        assert!(root.join(&result.artifact_path).exists());
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
