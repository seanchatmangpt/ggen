//! ggen-scip: SCIP-compatible index (JSON encoding, `scip.json`) of ggen's
//! generated surface.
//!
//! Symbols are `ggen <package> <dotted.identifier>`; ranges are
//! `[start_line, start_col, end_col]` with byte columns (tree-sitter byte
//! offsets into the line — documented ggen JSON variant; canonical SCIP is
//! UTF-16).
//!
//! Generated-surface detection: a `.rs` file whose first 5 lines contain
//! (case-insensitive) "generated" and "ggen", and do not contain
//! "placeholder" (excludes the `ggen-sjira/src/generated.rs` placeholder).

pub mod emit;
pub mod query;

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Occurrence role bits (`Occurrence.roles`, bitmask).
pub const ROLE_REFERENCE: u32 = 1 << 0;
/// Definition role bit.
pub const ROLE_DEFINITION: u32 = 1 << 1;

/// Dotted identifier path, e.g. `generated_commands.COMMANDS_REFERENCE`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Dotted(pub String);

impl std::fmt::Display for Dotted {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// Typed SCIP symbol: scheme `"ggen"`, package = crate name,
/// identifier = dotted path.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Symbol {
    /// Always `"ggen"` for this emitter.
    pub scheme: String,
    /// Rust crate (package) name, e.g. `ggen-cli-lib`.
    pub package: String,
    /// Dotted identifier path.
    pub identifier: Dotted,
}

impl Symbol {
    /// Wire form: `ggen <package> <identifier>`.
    pub fn to_wire(&self) -> String {
        format!("ggen {} {}", self.package, self.identifier)
    }
}

/// Parse `ggen <package> <identifier>` into a typed [`Symbol`].
pub fn parse_symbol(s: &str) -> Option<Symbol> {
    let mut parts = s.splitn(3, ' ');
    let scheme = parts.next()?;
    let package = parts.next()?;
    let identifier = parts.next()?;
    if scheme != "ggen" || package.is_empty() || identifier.is_empty() {
        return None;
    }
    Some(Symbol {
        scheme: scheme.to_string(),
        package: package.to_string(),
        identifier: Dotted(identifier.to_string()),
    })
}

/// SCIP JSON index model.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Index {
    /// Index metadata.
    pub metadata: Metadata,
    /// Documents sorted by `path`.
    pub documents: Vec<Document>,
}

/// Index metadata.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metadata {
    /// Project root (absolute path at emit time).
    pub project_root: String,
    /// Emitter identity.
    pub emitter: String,
    /// Shapes IRI the index conforms to (scip:conformsTo in
    /// ontologies/shapes/scip-index.shacl.ttl; R80-04).
    #[serde(rename = "conformsTo", default = "default_conforms_to")]
    pub conforms_to: String,
}

/// Default shapes IRI for [`Metadata::conforms_to`].
pub fn default_conforms_to() -> String {
    "https://ggen.dev/ontology/shapes/scip-index".to_string()
}

/// One source document's occurrences.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Document {
    /// Path relative to the project root.
    pub path: String,
    /// Occurrences sorted by (range, symbol).
    pub occurrences: Vec<Occurrence>,
}

/// A definition or reference at a range.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Occurrence {
    /// Full wire symbol, `ggen <package> <identifier>`.
    pub symbol: String,
    /// `[start_line, start_col, end_col]`, byte columns, 0-based.
    pub range: [u32; 3],
    /// Role bitmask (`ROLE_REFERENCE` | `ROLE_DEFINITION`).
    pub roles: u32,
    /// Definition info; present only when `roles & ROLE_DEFINITION != 0`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub def_info: Option<DefInfo>,
}

/// Extra info carried on definition occurrences.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefInfo {
    /// Simple name of the defined item.
    pub name: String,
    /// Item kind: `fn | struct | enum | const`.
    pub kind: String,
}

/// Canonical serialization: key-sorted compact JSON. Two emits of the same
/// surface are byte-identical.
pub fn canonical_json(index: &Index) -> serde_json::Result<String> {
    let v = serde_json::to_value(index)?;
    Ok(serde_json::to_string(&sort_keys(v))?)
}

/// Recursively sort object keys.
fn sort_keys(v: serde_json::Value) -> serde_json::Value {
    use serde_json::Value;
    match v {
        Value::Object(map) => {
            let btree: BTreeMap<String, Value> = map.into_iter().collect();
            Value::Object(btree.into_iter().map(|(k, x)| (k, sort_keys(x))).collect())
        }
        Value::Array(a) => Value::Array(a.into_iter().map(sort_keys).collect()),
        other => other,
    }
}

/// Write `canonical_json` to `to`, creating parent dirs.
pub fn write_canonical(index: &Index, to: &std::path::Path) -> std::io::Result<()> {
    let json = canonical_json(index).map_err(std::io::Error::other)?;
    if let Some(parent) = to.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(to, json)
}
