//! Admitted public Turtle `@prefix` names for law / hook surfaces.
//!
//! Single source of truth for ECOSYSTEM-SPEC-27.1.1 Gate 1 alignment with
//! `gall_actuate_code_evaluation`'s private-namespace scan. Project-private
//! prefixes (`gall`, `truex`, `gg`, …) are intentionally absent.
//!
//! Standing vocabulary (AGENTS.md §2):
//! - `ADMITTED` — public Turtle prefix allowlisted
//! - `REFUSED_PRIVATE` — project-private / banned token
//! - `REFUSED_UNKNOWN` — neither admitted nor on the private ban list

use super::shui;

/// Public vocabulary prefixes admitted in Turtle `@prefix` declarations.
pub const ADMITTED_TURTLE_PREFIXES: &[&str] = &[
    "rdf", "rdfs", "owl", "xsd", "prov", "dcat", "dcterms", "dc", "skos", "sh", "shui", "time",
    "spdx", "ocel", "schema", "mcp", "ex", "mp", "vann", "foaf",
];

/// Project-private / banned prefix tokens (must remain refused).
pub const REFUSED_PRIVATE_PREFIXES: &[&str] = &[
    "gall", "ggen", "gg", "kh", "pcst", "truex", "doc", "doctest",
];

/// Typed fence standing for a Turtle `@prefix` name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TurtlePrefixStanding {
    /// Prefix is on the public allowlist (includes W3C `shui:`).
    Admitted,
    /// Prefix is a project-private / banned token (`truex:`, `gall:`, …).
    RefusedPrivate,
    /// Prefix is neither admitted nor listed as private — still refused.
    RefusedUnknown,
}

impl TurtlePrefixStanding {
    /// Stable standing tag for receipts / courts.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Admitted => "ADMITTED",
            Self::RefusedPrivate => "REFUSED_PRIVATE",
            Self::RefusedUnknown => "REFUSED_UNKNOWN",
        }
    }

    /// True when the prefix may appear in public Turtle `@prefix` declarations.
    pub fn is_admitted(self) -> bool {
        matches!(self, Self::Admitted)
    }
}

/// Chicago-testable admission predicate for Turtle prefix names.
pub fn is_admitted_turtle_prefix(prefix: &str) -> bool {
    ADMITTED_TURTLE_PREFIXES.contains(&prefix)
}

/// Returns true when `prefix` is a known private/banned token.
pub fn is_refused_private_prefix(prefix: &str) -> bool {
    REFUSED_PRIVATE_PREFIXES.contains(&prefix)
}

/// Classify a Turtle prefix under the public / private / unknown fence.
///
/// Unknown prefixes are **REFUSED** (same actuation outcome as private bans);
/// only allowlisted public prefixes — including W3C `shui:` — are admitted.
pub fn turtle_prefix_standing(prefix: &str) -> TurtlePrefixStanding {
    if is_admitted_turtle_prefix(prefix) {
        TurtlePrefixStanding::Admitted
    } else if is_refused_private_prefix(prefix) {
        TurtlePrefixStanding::RefusedPrivate
    } else {
        TurtlePrefixStanding::RefusedUnknown
    }
}

/// Returns true when an IRI is under an admitted public namespace that does not
/// require a local-name whitelist (currently: W3C `shui:`).
pub fn is_open_admitted_namespace_iri(iri: &str) -> bool {
    shui::is_shui_iri(iri)
}
