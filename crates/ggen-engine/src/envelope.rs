//! Envelope `CapTable` extraction and envelope-declaration checks
//! (v26.10.20 wave-2).
//!
//! A `res:Envelope` is a named capability grant: a project may declare an
//! envelope IRI on a template's frontmatter (`envelope: <iri>`), and the
//! envelope individual in the project's envelope reticle
//! (`<root>/.specify/envelope-shapes.ttl`, lane A01's `res:` graph, the RDF
//! sole source of truth) carries the Fortune-5 cap overrides that govern
//! that template's sync run. A template with no envelope IRI (the
//! overwhelmingly common case) runs under the built-in defaults —
//! byte-identical to the pre-envelope pipeline.
//!
//! INTEGRATION RETIREMENT (lane I1, v26.10.20): A05's interim JSON sidecar
//! (`<root>/.ggen-v2/envelope.json`) is deleted, not left as a legacy
//! bypass. `extract_caps` now resolves caps by SPARQL over the `res:`
//! graph, per the Integration decision in `RESOLUTIONS.md`.
//!
//! The abb-sbb kernel consumes the *projected* numbers
//! (`ggen_abb_sbb::EnvelopeCaps`) and never parses envelopes itself; this
//! module is the ONE translator (RESOLUTIONS.md vocabulary decisions).

use std::path::Path;

use crate::graph::{EngineQueryResults, EngineValue, GraphEngine, GraphLawStore};

/// Built-in default for [`Cap::QueryResultRows`] (mirrors
/// `crate::sync::MAX_QUERY_RESULT_ROWS`, which aliases this constant).
pub const DEFAULT_MAX_QUERY_RESULT_ROWS: usize = 4_096;

/// Built-in default for [`Cap::PendingOutputs`] (mirrors
/// `crate::sync::MAX_PENDING_OUTPUTS`).
pub const DEFAULT_MAX_PENDING_OUTPUTS: usize = 8_192;

/// Built-in default for [`Cap::TotalRenderedBytes`] (mirrors
/// `crate::sync::MAX_TOTAL_RENDERED_BYTES`).
pub const DEFAULT_MAX_TOTAL_RENDERED_BYTES: usize = 64 * 1024 * 1024;

/// Predicate naming the envelope an envelope-reference triple points at.
/// A fact graph that references an envelope via this predicate must also
/// declare the envelope IRI as a subject — an undeclared reference is a
/// typed `E-ENVELOPE-UNDECLARED` refusal (see
/// [`crate::law_engine`]'s SHACL path), never a silent pass.
pub const ENVELOPE_PREDICATE: &str = "http://ggen.dev/vocab/envelope#envelope";

/// The per-run cap surface an envelope may override.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CapTable {
    /// Bound on rows admitted from any one named SPARQL result.
    pub max_query_result_rows: usize,
    /// Bound on materialized outputs admitted in one sync.
    pub max_pending_outputs: usize,
    /// Bound on combined rendered body bytes admitted across one sync.
    pub max_total_rendered_bytes: usize,
}

impl Default for CapTable {
    fn default() -> Self {
        Self {
            max_query_result_rows: DEFAULT_MAX_QUERY_RESULT_ROWS,
            max_pending_outputs: DEFAULT_MAX_PENDING_OUTPUTS,
            max_total_rendered_bytes: DEFAULT_MAX_TOTAL_RENDERED_BYTES,
        }
    }
}

/// One cap of the [`CapTable`], addressed for [`cap_for`] lookups.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cap {
    /// [`CapTable::max_query_result_rows`]
    QueryResultRows,
    /// [`CapTable::max_pending_outputs`]
    PendingOutputs,
    /// [`CapTable::max_total_rendered_bytes`]
    TotalRenderedBytes,
}

/// Lookup one cap. The one read surface for sync-time limits — direct
/// constant reads in the render loop are replaced by this so an envelope
/// can tighten (never loosen: see [`extract_caps`]'s clamping below) a
/// declared ceiling.
#[must_use]
pub fn cap_for(caps: &CapTable, cap: Cap) -> usize {
    match cap {
        Cap::QueryResultRows => caps.max_query_result_rows,
        Cap::PendingOutputs => caps.max_pending_outputs,
        Cap::TotalRenderedBytes => caps.max_total_rendered_bytes,
    }
}

/// The `res:` envelope vocabulary namespace, matching
/// `.specify/envelope-shapes.ttl` (lane A01's reticle graph).
pub const ENVELOPE_NS: &str = "http://ggen.org/envelope#";

/// The per-project envelope reticle: the RDF sole source of truth for cap
/// overrides. A project that carries no reticle runs under the built-in
/// defaults.
pub const ENVELOPE_RETICLE_REL: &str = ".specify/envelope-shapes.ttl";

/// Why [`extract_caps`] fell back to the built-in defaults despite a
/// declared envelope IRI (WO-06 R9 observability). `None`-style fallback
/// kinds that were previously silent are now surfaced as a structured
/// `ENVELOPE_CAP_FALLBACK` warn; the fail-soft contract (defaults,
/// min-clamped, never refusing sync) is unchanged.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CapFallback {
    /// A declared envelope IRI exists but the reticle file
    /// ([`ENVELOPE_RETICLE_REL`]) is missing or unreadable.
    MissingReticle,
    /// The reticle parsed but declares no `res:Envelope` individual matching
    /// the declared IRI.
    MissingIndividual,
    /// The reticle exists but is unparseable, the SPARQL surface failed, or
    /// a cap literal is a malformed (non-integer) value.
    MalformedLiterals,
}

impl CapFallback {
    /// Stable `kind` field for the `ENVELOPE_CAP_FALLBACK` structured event.
    #[must_use]
    pub fn kind(self) -> &'static str {
        match self {
            CapFallback::MissingReticle => "missing_reticle",
            CapFallback::MissingIndividual => "missing_individual",
            CapFallback::MalformedLiterals => "malformed_literals",
        }
    }
}

/// Extract the effective [`CapTable`] for `envelope` (a template's declared
/// envelope IRI, `None` when no template declares one).
///
/// Caps resolve by SPARQL over the `res:Envelope` graph in
/// `<root>/​.specify/envelope-shapes.ttl`: the declared IRI's
/// `res:maxAnswers` bounds query rows, `res:maxArtifacts` bounds pending
/// outputs, `res:maxTotalBytes` bounds rendered bytes.
///
/// Fail-soft by contract: a missing reticle, a missing envelope individual,
/// or an unparseable reticle all yield the built-in defaults — an envelope
/// grant can only ever TIGHTEN a ceiling (any override value above the
/// default is clamped to the default), never loosen it. Malformed overrides
/// are ignored rather than refused because the caps gate is a ceiling, not
/// an authority grant: refusing sync on a bad ceiling file would turn a
/// defensive limit into a denial-of-service on the project itself.
///
/// Unlike the bare [`extract_caps`], this surface also returns *why* a
/// fallback fired ([`CapFallback`]), so callers (and tests) can assert on
/// the fallback artifact without log capture. Every fallback also emits a
/// structured `ENVELOPE_CAP_FALLBACK` `tracing::warn!` with `code`, `kind`,
/// and the envelope IRI as `key`. The legitimate common path (no envelope
/// IRI declared at all, or no reticle with no envelope declared) emits
/// nothing; a declared-but-corrupt envelope emits exactly one event.
#[must_use]
pub fn extract_caps_checked(
    root: &Path, envelope: Option<&str>,
) -> (CapTable, Option<CapFallback>) {
    let Some(iri) = envelope else {
        return (CapTable::default(), None);
    };
    let Ok(ttl) = std::fs::read_to_string(root.join(ENVELOPE_RETICLE_REL)) else {
        return fallback(CapFallback::MissingReticle, iri);
    };
    let caps_from_graph = (|| -> Result<CapTable, CapFallback> {
        let store = GraphLawStore::new().map_err(|_| CapFallback::MalformedLiterals)?;
        store
            .insert_turtle(&ttl)
            .map_err(|_| CapFallback::MalformedLiterals)?;
        let query = format!(
            "PREFIX res: <{ENVELOPE_NS}>\n\
             SELECT ?env ?answers ?artifacts ?total WHERE {{\n\
             \x20 ?env a res:Envelope .\n\
             \x20 OPTIONAL {{ ?env res:maxAnswers ?answers }}\n\
             \x20 OPTIONAL {{ ?env res:maxArtifacts ?artifacts }}\n\
             \x20 OPTIONAL {{ ?env res:maxTotalBytes ?total }}\n\
             }}"
        );
        let EngineQueryResults::Solutions(rows) = store
            .query(&query)
            .map_err(|_| CapFallback::MalformedLiterals)?
        else {
            return Err(CapFallback::MalformedLiterals);
        };
        let iri_bare = iri.trim_start_matches('<').trim_end_matches('>');
        for row in &rows {
            let mut matched = false;
            let mut found = CapTable::default();
            for (var, value) in row {
                let var: &str = var;
                match (var, value) {
                    ("env", EngineValue::String(subject)) => {
                        let subject = subject.trim_start_matches('<').trim_end_matches('>');
                        // Match the declared IRI as written (relative
                        // frontmatter idiom `res:tight`) or as an absolute
                        // IRI whose bare form equals the declaration.
                        if subject == iri_bare {
                            matched = true;
                        }
                    }
                    ("answers", v) => {
                        found.max_query_result_rows = cap_usize(v)
                            .ok_or(CapFallback::MalformedLiterals)?
                            .min(DEFAULT_MAX_QUERY_RESULT_ROWS);
                    }
                    ("artifacts", v) => {
                        found.max_pending_outputs = cap_usize(v)
                            .ok_or(CapFallback::MalformedLiterals)?
                            .min(DEFAULT_MAX_PENDING_OUTPUTS);
                    }
                    ("total", v) => {
                        found.max_total_rendered_bytes = cap_usize(v)
                            .ok_or(CapFallback::MalformedLiterals)?
                            .min(DEFAULT_MAX_TOTAL_RENDERED_BYTES);
                    }
                    _ => {}
                }
            }
            if matched {
                return Ok(found);
            }
        }
        Err(CapFallback::MissingIndividual)
    })();
    match caps_from_graph {
        Ok(table) => (table, None),
        Err(kind) => fallback(kind, iri),
    }
}

/// The one fallback exit: builtin defaults (fail-soft contract unchanged) +
/// one structured `ENVELOPE_CAP_FALLBACK` warn (WO-06 R9 — a corrupt
/// reticle is never silently absorbed).
fn fallback(kind: CapFallback, iri: &str) -> (CapTable, Option<CapFallback>) {
    tracing::warn!(
        code = "ENVELOPE_CAP_FALLBACK",
        kind = kind.kind(),
        key = %iri,
        "envelope cap extraction fell back to the built-in defaults"
    );
    (CapTable::default(), Some(kind))
}

/// Extract the effective [`CapTable`] for `envelope`, emitting a structured
/// `ENVELOPE_CAP_FALLBACK` warn on any corrupt-reticle fallback (WO-06 R9).
///
/// Thin wrapper over [`extract_caps_checked`] preserving the historical
/// signature; prefer the checked surface when a caller must distinguish
/// "legitimate defaults" from "fallback under a declared envelope".
#[must_use]
pub fn extract_caps(root: &Path, envelope: Option<&str>) -> CapTable {
    extract_caps_checked(root, envelope).0
}

/// Coerce an [`EngineValue`] cap literal into a `usize` (`None` for any
/// non-integer-family literal — a malformed cap is ignored, not clamped).
fn cap_usize(value: &EngineValue) -> Option<usize> {
    match value {
        EngineValue::Int(n) if *n >= 0 => usize::try_from(*n).ok(),
        EngineValue::String(s) => s.trim().parse::<usize>().ok(),
        _ => None,
    }
}

/// Return the first envelope IRI referenced via [`ENVELOPE_PREDICATE`] in
/// `facts_nt` that is never declared as a subject in the same document —
/// `None` when every referenced envelope is declared (or none are
/// referenced).
///
/// Plain N-Triples line parsing, same contract as the rest of this crate's
/// law surface: one subject per line, `<iri>` terms, `.`-terminated.
#[must_use]
pub fn undeclared_envelope(facts_nt: &str) -> Option<String> {
    let pred = format!("<{ENVELOPE_PREDICATE}>");
    let mut referenced: Vec<String> = Vec::new();
    let mut declared: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for line in facts_nt.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix('<') {
            if let Some(subject_end) = rest.find('>') {
                declared.insert(rest[..subject_end].to_string());
            }
        }
        if let Some(obj_start) = line.find(&pred) {
            let after = &line[obj_start + pred.len()..];
            if let Some(obj) = after.trim().trim_end_matches('.').trim().strip_prefix('<') {
                if let Some(end) = obj.find('>') {
                    referenced.push(obj[..end].to_string());
                }
            }
        }
    }
    referenced.into_iter().find(|iri| !declared.contains(iri))
}

/// Translate the declared envelope's `res:maxArtifacts` literal into the
/// abb-sbb kernel's per-call [`ggen_abb_sbb::EnvelopeCaps`] override
/// (WO-01, Stage 2c wiring, 2026-11-03).
///
/// `None` when there is no declared envelope IRI, no reticle file, the
/// envelope is undeclared, or no `res:maxArtifacts` literal is present —
/// in every `None` case the kernel applies its own
/// [`ggen_abb_sbb::DEFAULT_MAX_ARTIFACTS`] (64) default. A `Some` value is
/// clamped to that same default: an envelope can only ever TIGHTEN the
/// kernel ceiling, never loosen it (same tighten-only contract as
/// [`extract_caps`]).
///
/// Unlike [`extract_caps`] (which folds a missing override into
/// `CapTable::default()`), this returns `Option` so the caller can
/// distinguish "no envelope grant" from "grant of exactly the default".
#[must_use]
pub fn abb_envelope_caps(
    root: &Path, envelope: Option<&str>,
) -> Option<ggen_abb_sbb::EnvelopeCaps> {
    let iri = envelope?;
    let ttl = std::fs::read_to_string(root.join(ENVELOPE_RETICLE_REL)).ok()?;
    let store = GraphLawStore::new().ok()?;
    store.insert_turtle(&ttl).ok()?;
    let query = format!(
        "PREFIX res: <{ENVELOPE_NS}>\n\
         SELECT ?env ?artifacts WHERE {{\n\
         \x20 ?env a res:Envelope .\n\
         \x20 ?env res:maxArtifacts ?artifacts .\n\
         }}"
    );
    let EngineQueryResults::Solutions(rows) = store.query(&query).ok()? else {
        return None;
    };
    let iri_bare = iri.trim_start_matches('<').trim_end_matches('>');
    for row in &rows {
        let mut subject: Option<&str> = None;
        let mut artifacts: Option<usize> = None;
        for (var, value) in row {
            // SLA unblock (lane G2, disclosed): match on `as_str()` — `var`
            // is a `&String`, so literal patterns need the deref (E0308).
            match (var.as_str(), value) {
                ("env", EngineValue::String(s)) => {
                    subject = Some(s.trim_start_matches('<').trim_end_matches('>'));
                }
                ("artifacts", v) => artifacts = cap_usize(v),
                _ => {}
            }
        }
        if subject == Some(iri_bare) {
            if let Some(n) = artifacts {
                return Some(ggen_abb_sbb::EnvelopeCaps {
                    max_artifacts: n.min(ggen_abb_sbb::DEFAULT_MAX_ARTIFACTS),
                });
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    #![allow(clippy::expect_used)] // test-only, H05 class (a)

    use super::*;

    #[test]
    fn defaults_match_the_documented_builtin_limits() {
        let caps = CapTable::default();
        assert_eq!(cap_for(&caps, Cap::QueryResultRows), 4_096);
        assert_eq!(cap_for(&caps, Cap::PendingOutputs), 8_192);
        assert_eq!(cap_for(&caps, Cap::TotalRenderedBytes), 64 * 1024 * 1024);
    }

    #[test]
    fn missing_envelope_yields_defaults() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        // Declared envelope but no reticle file: fail-soft defaults PLUS the
        // structured fallback flag (WO-06 R9 — never silently absorbed).
        let (caps, fallback) = extract_caps_checked(dir.path(), Some("res:Missing"));
        assert_eq!(caps, CapTable::default());
        assert_eq!(fallback, Some(CapFallback::MissingReticle));
        // No envelope declared at all: legitimate common path, no fallback.
        let (caps, fallback) = extract_caps_checked(dir.path(), None);
        assert_eq!(caps, CapTable::default());
        assert_eq!(fallback, None);
    }

    #[test]
    fn malformed_cap_literal_falls_back_loud() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = dir.path().join(ENVELOPE_RETICLE_REL);
        std::fs::create_dir_all(path.parent().expect("parent")).expect("mkdir");
        // Present-but-corrupt reticle: a non-integer cap literal.
        std::fs::write(
            &path,
            format!(
                "@prefix res: <{ENVELOPE_NS}> .\n\
                 <http://example.org/env/corrupt> a res:Envelope ;\n\
                 \x20   res:maxArtifacts \"not-a-number\" .\n"
            ),
        )
        .expect("write reticle");
        let (caps, fallback) =
            extract_caps_checked(dir.path(), Some("http://example.org/env/corrupt"));
        // Fail-soft contract untouched: defaults, never a refusal.
        assert_eq!(caps, CapTable::default());
        assert_eq!(fallback, Some(CapFallback::MalformedLiterals));
    }

    #[test]
    fn missing_envelope_individual_falls_back_loud() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = dir.path().join(ENVELOPE_RETICLE_REL);
        std::fs::create_dir_all(path.parent().expect("parent")).expect("mkdir");
        std::fs::write(
            &path,
            format!(
                "@prefix res: <{ENVELOPE_NS}> .\n\
                 <http://example.org/env/other> a res:Envelope ;\n\
                 \x20   res:maxArtifacts 4 .\n"
            ),
        )
        .expect("write reticle");
        let (caps, fallback) =
            extract_caps_checked(dir.path(), Some("http://example.org/env/absent"));
        assert_eq!(caps, CapTable::default());
        assert_eq!(fallback, Some(CapFallback::MissingIndividual));
    }

    #[test]
    fn healthy_envelope_emits_no_fallback() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = dir.path().join(ENVELOPE_RETICLE_REL);
        std::fs::create_dir_all(path.parent().expect("parent")).expect("mkdir");
        std::fs::write(
            &path,
            format!(
                "@prefix res: <{ENVELOPE_NS}> .\n\
                 <http://example.org/env/tight> a res:Envelope ;\n\
                 \x20   res:maxArtifacts 4 .\n"
            ),
        )
        .expect("write reticle");
        let (caps, fallback) =
            extract_caps_checked(dir.path(), Some("http://example.org/env/tight"));
        assert_eq!(caps.max_pending_outputs, 4);
        assert_eq!(fallback, None);
    }

    #[test]
    fn declared_envelope_tightens_and_is_clamped_to_defaults() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = dir.path().join(ENVELOPE_RETICLE_REL);
        std::fs::create_dir_all(path.parent().expect("parent")).expect("mkdir");
        std::fs::write(
            &path,
            format!(
                "@prefix res: <{ENVELOPE_NS}> .\n\
                 <http://example.org/env/tight> a res:Envelope ;\n\
                 \x20   res:maxArtifacts 4 .\n\
                 <http://example.org/env/loose> a res:Envelope ;\n\
                 \x20   res:maxArtifacts 999999 .\n"
            ),
        )
        .expect("write reticle");
        assert_eq!(
            extract_caps(dir.path(), Some("http://example.org/env/tight")).max_pending_outputs,
            4
        );
        // An envelope can never loosen a ceiling above the builtin default.
        assert_eq!(
            extract_caps(dir.path(), Some("http://example.org/env/loose")).max_pending_outputs,
            DEFAULT_MAX_PENDING_OUTPUTS
        );
        // An envelope that declares only one cap leaves the others at the
        // defaults (OPTIONAL-bound SPARQL extraction, not all-or-nothing).
        assert_eq!(
            extract_caps(dir.path(), Some("http://example.org/env/tight")).max_query_result_rows,
            DEFAULT_MAX_QUERY_RESULT_ROWS
        );
    }

    #[test]
    fn undeclared_envelope_reference_is_detected_declared_passes() {
        let pred = format!("<{ENVELOPE_PREDICATE}>");
        let facts = format!(
            "<ex:a> {pred} <res:Env1> .\n<res:Env1> <ex:cap> \"4\" .\n<ex:b> {pred} <res:Env2> .\n"
        );
        assert_eq!(undeclared_envelope(&facts), Some("res:Env2".to_string()));
        let ok = format!("<ex:a> {pred} <res:Env1> .\n<res:Env1> <ex:x> <ex:y> .\n");
        assert_eq!(undeclared_envelope(&ok), None);
        assert_eq!(undeclared_envelope(""), None);
    }
}
