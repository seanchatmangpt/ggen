//! `ggen_frontmatter_schema` — enumerate every legal template frontmatter
//! key, from the `schemars::JsonSchema` derive on
//! `ggen_engine::template::Frontmatter` (the same struct that is
//! `#[serde(deny_unknown_fields)]` and drift-tested against
//! `crates/ggen-engine/schema/frontmatter-schema.ttl`).
//!
//! Closes a verified friction point: an agent authoring a ggen project used
//! 3 of the 25 legal keys and never discovered `for_each:` — the fan-out
//! mechanism its whole redesign turned on — because nothing surfaced the
//! key set.
//!
//! The key list is DERIVED, never hardcoded, so it cannot drift from the
//! struct. The projection-mode rule below is the one thing that must be
//! stated separately: it is control flow in `sync()`, not schema, so no
//! amount of schema derivation would reveal it.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::McpError;

#[derive(Debug, Default, Deserialize, JsonSchema)]
pub struct FrontmatterSchemaParams {
    /// Optional: return only this key's entry instead of all of them.
    #[serde(default)]
    pub key: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct FrontmatterKey {
    pub name: String,
    /// JSON-Schema type/shape for this key, as derived.
    pub schema: serde_json::Value,
    pub required: bool,
    /// Doc comment from the `Frontmatter` field, when the derive carried
    /// one through.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct FrontmatterSchemaResult {
    pub ok: bool,
    /// Total number of legal frontmatter keys. Derived from the schema, so
    /// this count is definitionally correct rather than a maintained claim.
    pub key_count: usize,
    pub keys: Vec<FrontmatterKey>,
    /// The projection-mode rule, which determines whether a template writes
    /// ONE file or one file PER ROW. This is **not** expressible in the
    /// JSON Schema -- it is control flow in `ggen_engine::sync`, decided by
    /// the interaction of `to:` and `for_each:`. Stated explicitly here
    /// because it is exactly the mechanism the verified friction case
    /// needed and could not discover from the key list alone.
    pub projection_modes: Vec<ProjectionMode>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct ProjectionMode {
    pub mode: String,
    pub condition: String,
    pub effect: String,
}

fn projection_modes() -> Vec<ProjectionMode> {
    vec![
        ProjectionMode {
            mode: "fan_out".to_string(),
            condition: "`to:` contains a Tera interpolation (`{{ ... }}`)".to_string(),
            effect: "One output file PER ROW of the driving query. The `to:` path \
                     is rendered per row, so each row must produce a distinct path."
                .to_string(),
        },
        ProjectionMode {
            mode: "aggregate".to_string(),
            condition: "`for_each:` names a query AND `to:` is a static path".to_string(),
            effect: "ONE output file. The template body is rendered once per row and \
                     the results are concatenated into that single file."
                .to_string(),
        },
        ProjectionMode {
            mode: "single".to_string(),
            condition: "neither of the above".to_string(),
            effect: "ONE output file, body rendered once. Named `sparql:` results are \
                     available in the Tera context; the first array-valued one is the \
                     implicit driving row set."
                .to_string(),
        },
    ]
}

/// Enumerate the frontmatter key set from the live `schemars` derive.
///
/// # Errors
/// `ErrorCategory::NotFound` if `params.key` names a key that does not exist
/// (the caller gets the real key set rather than a silent empty result).
pub fn frontmatter_schema(
    params: &FrontmatterSchemaParams,
) -> Result<FrontmatterSchemaResult, McpError> {
    let schema = schemars::schema_for!(ggen_engine::template::Frontmatter);
    let value = serde_json::to_value(&schema).unwrap_or_default();

    let required: std::collections::BTreeSet<String> = value
        .get("required")
        .and_then(|r| r.as_array())
        .map(|a| {
            a.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        })
        .unwrap_or_default();

    let mut keys: Vec<FrontmatterKey> = value
        .get("properties")
        .and_then(|p| p.as_object())
        .map(|props| {
            props
                .iter()
                .map(|(name, prop)| FrontmatterKey {
                    name: name.clone(),
                    description: prop
                        .get("description")
                        .and_then(|d| d.as_str())
                        .map(String::from),
                    required: required.contains(name),
                    schema: prop.clone(),
                })
                .collect()
        })
        .unwrap_or_default();
    keys.sort_by(|a, b| a.name.cmp(&b.name));

    if let Some(wanted) = params.key.as_deref() {
        let available: Vec<String> = keys.iter().map(|k| k.name.clone()).collect();
        keys.retain(|k| k.name == wanted);
        if keys.is_empty() {
            return Err(McpError::new(
                crate::error::ErrorCategory::NotFound,
                format!(
                    "no frontmatter key named {wanted:?}. Legal keys: {}",
                    available.join(", ")
                ),
            ));
        }
    }

    Ok(FrontmatterSchemaResult {
        ok: true,
        key_count: keys.len(),
        keys,
        projection_modes: projection_modes(),
    })
}
