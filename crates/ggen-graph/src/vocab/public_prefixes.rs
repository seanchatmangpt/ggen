//! Admitted public Turtle `@prefix` names for law / hook surfaces.
//!
//! Single source of truth for ECOSYSTEM-SPEC-27.1.1 Gate 1 alignment with
//! `gall_actuate_code_evaluation`'s private-namespace scan. Project-private
//! prefixes (`gall`, `truex`, `gg`, …) are intentionally absent.

use super::shui;

/// Public vocabulary prefixes admitted in Turtle `@prefix` declarations.
pub const ADMITTED_TURTLE_PREFIXES: &[&str] = &[
    "rdf", "rdfs", "owl", "xsd", "prov", "dcat", "dcterms", "dc", "skos", "sh", "shui", "time",
    "spdx", "ocel", "schema", "mcp", "ex", "mp", "vann", "foaf",
];

/// Chicago-testable admission predicate for Turtle prefix names.
pub fn is_admitted_turtle_prefix(prefix: &str) -> bool {
    ADMITTED_TURTLE_PREFIXES.contains(&prefix)
}

/// Project-private / banned prefix tokens (must remain refused).
pub const REFUSED_PRIVATE_PREFIXES: &[&str] = &[
    "gall", "ggen", "gg", "kh", "pcst", "truex", "doc", "doctest",
];

/// Returns true when `prefix` is a known private/banned token.
pub fn is_refused_private_prefix(prefix: &str) -> bool {
    REFUSED_PRIVATE_PREFIXES.contains(&prefix)
}

/// Returns true when an IRI is under an admitted public namespace that does not
/// require a local-name whitelist (currently: W3C `shui:`).
pub fn is_open_admitted_namespace_iri(iri: &str) -> bool {
    shui::is_shui_iri(iri)
}
