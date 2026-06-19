//! RDF vocabularies for the reverse pipeline (namespaces, prefixes, term helpers).
//!
//! These mirror the static term definitions dropped at
//! `.specify/ontology/discovered.ttl` and `.specify/ontology/defect.ttl`, so a
//! forward `ggen sync` over reverse output can resolve the same types.

/// `code:` — structural facts recovered from source (reuses the existing
/// namespace produced by [`crate::reverse_sync::ast_extractor::convert_to_rdf`]).
pub const CODE_NS: &str = "https://ggen.io/code#";
/// `disco:` — discovered-authority provenance vocabulary.
pub const DISCO_NS: &str = "https://ggen.io/discovered#";
/// `defect:` — defect/standard-work vocabulary (ARM 2).
pub const DEFECT_NS: &str = "https://ggen.io/defect#";
/// `rdfs:` — RDF Schema.
pub const RDFS_NS: &str = "http://www.w3.org/2000/01/rdf-schema#";
/// `xsd:` — XML Schema datatypes.
pub const XSD_NS: &str = "http://www.w3.org/2001/XMLSchema#";
/// `sh:` — SHACL.
pub const SH_NS: &str = "http://www.w3.org/ns/shacl#";

// ── Public industry-ontology namespaces (vendored under `.specify/ontology/public/`) ──
/// PROV-O (W3C provenance).
pub const PROV_NS: &str = "http://www.w3.org/ns/prov#";
/// SOSA (W3C sensors/observations/actuators).
pub const SOSA_NS: &str = "http://www.w3.org/ns/sosa/";
/// QUDT schema (quantities/units).
pub const QUDT_NS: &str = "http://qudt.org/schema/qudt/";

/// Prefix for [`CODE_NS`].
pub const CODE: &str = "code";
/// Prefix for [`DISCO_NS`].
pub const DISCO: &str = "disco";
/// Prefix for [`DEFECT_NS`].
pub const DEFECT: &str = "defect";
/// Prefix for [`RDFS_NS`].
pub const RDFS: &str = "rdfs";
/// Prefix for [`XSD_NS`].
pub const XSD: &str = "xsd";
/// Prefix for [`SH_NS`].
pub const SH: &str = "sh";

/// Build a `code:<local>` prefixed name.
pub fn code(local: &str) -> String {
    format!("{CODE}:{local}")
}

/// Build a `disco:<local>` prefixed name.
pub fn disco(local: &str) -> String {
    format!("{DISCO}:{local}")
}

/// Build a `defect:<local>` prefixed name.
pub fn defect(local: &str) -> String {
    format!("{DEFECT}:{local}")
}

/// Build an `xsd:<local>` prefixed name (for typed-literal datatypes).
pub fn xsd(local: &str) -> String {
    format!("{XSD}:{local}")
}

/// Build an `sh:<local>` prefixed name.
pub fn sh(local: &str) -> String {
    format!("{SH}:{local}")
}
