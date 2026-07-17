//! `ggen.toml` manifest parsing, validation, and types.
//!
//! Ported from `ggen-core/src/manifest/` (specs/014-ggen-core-replacement,
//! docs/jira/v26.7.16/05-MANIFEST-CONFIG-PORT.md) — the single authoritative
//! typed schema for every recognized `ggen.toml` top-level table:
//! `[project]`/`[ontology]`/`[inference]`/`[generation]`/`[validation]`/
//! `[[packs]]`/`[law]` (this crate's own domain/codegen fields) *and*
//! `[ai]`/`[rdf]`/`[sparql]`/`[lifecycle]`/`[security]`/`[performance]`/
//! `[logging]`/`[telemetry]`/`[features]`/`[env]`/`[templates]`/`[build]`/
//! `[test]`/`[package]`/`[mcp]`/`[a2a]` (reused directly from
//! `crate::config_lib`, not re-defined — see `GgenManifest`'s struct-level
//! doc comment for the full reconciliation, specs/014-ggen-core-replacement
//! T023's follow-up). `crate::config_lib::GgenConfig` remains a second,
//! narrower operational-only schema (no `[[generation.rules]]` concept);
//! `GgenManifest` here is the superset intended as the eventual single
//! parse target for both the engine and the LSP/diagnostic layer.

pub mod parser;
pub mod types;
pub mod validation;

pub use parser::ManifestParser;
pub use types::{
    GenerationConfig, GenerationMode, GenerationRule, GgenManifest, InferenceConfig,
    InferenceRule, Law, OntologyConfig, PackRef, PackSection, PackageToml, ProjectConfig,
    QuerySource, TemplateSource, ValidationConfig, ValidationRule, ValidationSeverity,
};
pub use validation::{query_contains_values, query_has_order_by, ManifestValidator};
