//! `ggen_config_classify` — answer "which of ggen.toml's two incompatible
//! schemas will my project be parsed as" without running any pipeline.
//!
//! Closes a verified friction point: `ggen_config::classify_ggen_toml` is
//! pure, fast, and side-effect-free, but the only ways to reach its answer
//! today are `ggen doctor` or `ggen sync run --dry-run` — both of which do
//! far more (pack resolution, potentially a git clone) than the agent
//! wanted, just to learn which parser applies.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use ggen_config::ConfigSchemaClassification;

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct ConfigClassifyParams {
    /// Project root directory (containing `ggen.toml`).
    pub root: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ConfigClassifyResult {
    pub ok: bool,
    /// `declarative_rules` | `frontmatter` | `ambiguous` | `unsupported` |
    /// `malformed` — which typed schema classification fired.
    pub schema: String,
    /// The typed outcome code the classifier itself reports
    /// (`CONFIG_SCHEMA_SUPPORTED` / `_AMBIGUOUS` / `_UNSUPPORTED` /
    /// `CONFIG_PARSE_FAILED`) — carried through verbatim rather than
    /// re-derived here.
    pub code: String,
    /// For `ambiguous`: every structural marker that fired, each prefixed
    /// with the schema it belongs to. For `unsupported`: the document's
    /// observed top-level tables. Empty otherwise.
    pub markers: Vec<String>,
    /// For `malformed`: the underlying TOML parser diagnostic.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostic: Option<String>,
    /// Absolute path of the `ggen.toml` that was classified.
    pub manifest_path: String,
    /// BLAKE3 of the exact bytes classified — so a caller can prove which
    /// file content produced this answer.
    pub manifest_blake3: String,
}

/// Classify `root`'s `ggen.toml`. Reads exactly one file; runs no pipeline
/// stage, resolves no packs, clones no git repositories.
///
/// # Errors
/// `ErrorCategory::PathTraversal` if `root` does not resolve to a real
/// directory; `ErrorCategory::NotFound` if it has no readable `ggen.toml`.
pub fn config_classify(params: &ConfigClassifyParams) -> Result<ConfigClassifyResult, McpError> {
    let root = resolve_root(&params.root)?;
    let manifest_path = root.join("ggen.toml");
    let raw = std::fs::read_to_string(&manifest_path).map_err(|e| {
        McpError::new(
            ErrorCategory::NotFound,
            format!("{} unreadable: {e}", manifest_path.display()),
        )
    })?;

    let classification = ggen_config::classify_ggen_toml(&raw);
    let code = classification.code().to_string();
    let (schema, markers, diagnostic) = match classification {
        ConfigSchemaClassification::DeclarativeRules => {
            ("declarative_rules".to_string(), Vec::new(), None)
        }
        ConfigSchemaClassification::Frontmatter => ("frontmatter".to_string(), Vec::new(), None),
        ConfigSchemaClassification::Ambiguous { matched } => {
            ("ambiguous".to_string(), matched, None)
        }
        ConfigSchemaClassification::Unsupported { observed_markers } => {
            ("unsupported".to_string(), observed_markers, None)
        }
        ConfigSchemaClassification::Malformed { diagnostic } => {
            ("malformed".to_string(), Vec::new(), Some(diagnostic))
        }
    };

    Ok(ConfigClassifyResult {
        // `ok` reports that classification RAN, not that the document is
        // well-formed -- a `malformed` verdict is a successful, informative
        // classification, and collapsing it into an error would hide the
        // parser diagnostic the caller needs.
        ok: true,
        schema,
        code,
        markers,
        diagnostic,
        manifest_path: manifest_path.display().to_string(),
        manifest_blake3: blake3::hash(raw.as_bytes()).to_hex().to_string(),
    })
}
