//! `ggen reverse` — the reverse-pipeline CLI surface (code/cheat → RDF authority
//! + Tera template candidates), the inverse of `ggen sync`.
//!
//! Verbs:
//! - `ggen reverse scan`      — code → `.specify/discovered/<name>.ttl` (+ receipt).
//! - `ggen reverse templates` — discovered graph → `templates/candidates/*` (+ receipt).
//! - `ggen reverse cheats`    — diagnostic species + ledger → `.specify/defects/`
//!   + candidate SHACL (requires `--features lsp`: it sources the authoritative
//!   `GGEN-*` diagnostic registry from `ggen-lsp`, so there is no duplicated,
//!   drift-prone species table here).
//!
//! Each verb returns its run's neutral OCEL-shaped event count in the output.
//! Under `--features lsp` those events are also persisted to the `.ggen/ocel`
//! intel log via `ggen-lsp`'s `IntelLog` (see [`persist_events`]); without the
//! feature persistence is a no-op and the events remain only in the output.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb::{NounVerbError, Result as VerbResult};
use clap_noun_verb_macros::verb;
use ggen_core::reverse;
use serde::Serialize;
use std::path::{Path, PathBuf};

/// Persist a run's neutral events to the `.ggen/ocel` intel log.
///
/// Under `--features lsp` this maps each [`reverse::ReverseEvent`] to OCEL and
/// appends it via `ggen-lsp`'s `IntelLog`, so the offline miner sees the
/// reverse-pipeline process. Without the feature it is a no-op (the events are
/// still surfaced in the verb's output).
#[cfg(feature = "lsp")]
fn persist_events(events: &[reverse::ReverseEvent], project_root: &Path) -> VerbResult<()> {
    ggen_lsp::intel::append_reverse_events(project_root, events)
        .map_err(|e| NounVerbError::execution_error(format!("OCEL append: {e}")))
}

/// No-op when the `lsp` feature is disabled (OCEL log lives in `ggen-lsp`).
#[cfg(not(feature = "lsp"))]
fn persist_events(_events: &[reverse::ReverseEvent], _project_root: &Path) -> VerbResult<()> {
    Ok(())
}

/// Output of `ggen reverse scan`.
#[derive(Debug, Clone, Serialize)]
pub struct ReverseScanOutput {
    /// "success".
    pub status: String,
    /// Services discovered.
    pub services: usize,
    /// Triples in the authority graph.
    pub triples: usize,
    /// Source files that contributed services.
    pub files_scanned: usize,
    /// Path to the discovered authority graph.
    pub authority_ttl: String,
    /// Path to the provenance receipt.
    pub receipt_path: String,
    /// Number of OCEL-shaped events the run produced.
    pub events: usize,
}

/// Output of `ggen reverse templates`.
#[derive(Debug, Clone, Serialize)]
pub struct ReverseTemplatesOutput {
    /// "success".
    pub status: String,
    /// Template candidates emitted.
    pub candidates: usize,
    /// Directory the candidates were written to.
    pub out_dir: String,
    /// Path to the provenance receipt.
    pub receipt_path: String,
    /// Number of OCEL-shaped events the run produced.
    pub events: usize,
}

/// `ggen reverse scan` — walk `paths` and emit a discovered RDF authority graph.
#[verb]
pub fn scan(paths: Option<String>, name: Option<String>) -> VerbResult<ReverseScanOutput> {
    let project_root =
        std::env::current_dir().map_err(|e| NounVerbError::execution_error(format!("cwd: {e}")))?;
    let paths = paths.ok_or_else(|| {
        NounVerbError::execution_error(
            "ggen reverse scan requires --paths <dir>[,<dir>...]".to_string(),
        )
    })?;
    let roots = resolve_paths(&paths, &project_root);
    let name = name.unwrap_or_else(|| "discovered".to_string());

    let report = reverse::scan_to_authority(&roots, &project_root, &name)
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;
    persist_events(&report.events, &project_root)?;

    Ok(ReverseScanOutput {
        status: "success".to_string(),
        services: report.services,
        triples: report.triples,
        files_scanned: report.files_scanned,
        authority_ttl: rel(&project_root, &report.authority_ttl),
        receipt_path: rel(&project_root, &report.receipt_path),
        events: report.events.len(),
    })
}

/// `ggen reverse templates` — infer Tera template candidates from a discovered graph.
#[verb]
pub fn templates(from: Option<String>, out: Option<String>) -> VerbResult<ReverseTemplatesOutput> {
    let project_root =
        std::env::current_dir().map_err(|e| NounVerbError::execution_error(format!("cwd: {e}")))?;
    let discovered = from
        .map(|p| absolutize(&p, &project_root))
        .unwrap_or_else(|| {
            project_root
                .join(".specify")
                .join("discovered")
                .join("discovered.ttl")
        });
    let out_dir = out
        .map(|p| absolutize(&p, &project_root))
        .unwrap_or_else(|| project_root.join("templates").join("candidates"));

    let report = reverse::infer_candidates(&project_root, &discovered, &out_dir)
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;
    persist_events(&report.events, &project_root)?;

    Ok(ReverseTemplatesOutput {
        status: "success".to_string(),
        candidates: report.candidates.len(),
        out_dir: rel(&project_root, &out_dir),
        receipt_path: rel(&project_root, &report.receipt_path),
        events: report.events.len(),
    })
}

/// Output of `ggen reverse align` (on admission).
#[derive(Debug, Clone, Serialize)]
pub struct ReverseAlignOutput {
    /// "aligned".
    pub status: String,
    /// Always true on success (the gate fails closed otherwise).
    pub is_aligned: bool,
    /// Count of public-namespace terms the domain composes that are defined.
    pub admitted: usize,
    /// Public namespaces the domain was admitted against.
    pub public_dir: String,
}

/// `ggen reverse align` — admission gate: admit a domain graph only if every
/// public-namespace term it uses is defined in the vendored public ontologies.
///
/// Fails closed: if the domain references any undefined ("fabricated") public
/// term, the command exits non-zero and names the offending terms.
#[verb]
pub fn align(domain: Option<String>, public: Option<String>) -> VerbResult<ReverseAlignOutput> {
    let project_root =
        std::env::current_dir().map_err(|e| NounVerbError::execution_error(format!("cwd: {e}")))?;
    let domain = domain.ok_or_else(|| {
        NounVerbError::execution_error("ggen reverse align requires --domain <ttl>".to_string())
    })?;
    let domain_path = absolutize(&domain, &project_root);
    let public_dir = public.map(|p| absolutize(&p, &project_root)).unwrap_or_else(|| {
        project_root
            .join(".specify")
            .join("ontology")
            .join("vendored")
    });

    let report = reverse::check_alignment(&domain_path, &public_dir)
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;

    if !report.is_aligned() {
        return Err(NounVerbError::execution_error(format!(
            "unaligned: domain references {} undefined public term(s): {}",
            report.unaligned.len(),
            report.unaligned.join(", ")
        )));
    }

    Ok(ReverseAlignOutput {
        status: "aligned".to_string(),
        is_aligned: true,
        admitted: report.admitted.len(),
        public_dir: rel(&project_root, &public_dir),
    })
}

/// Output of `ggen reverse cheats`.
#[cfg(feature = "lsp")]
#[derive(Debug, Clone, Serialize)]
pub struct ReverseCheatsOutput {
    /// "success".
    pub status: String,
    /// Defect nodes derived from the diagnostic species registry.
    pub species_defects: usize,
    /// Defect nodes derived from the fake-inventory ledger (existing files only).
    pub ledger_defects: usize,
    /// Triples in the defect graph.
    pub triples: usize,
    /// Path to the defect graph.
    pub defects_ttl: String,
    /// Candidate SHACL shape files emitted.
    pub shapes: usize,
    /// Path to the provenance receipt.
    pub receipt_path: String,
    /// Number of OCEL-shaped events the run produced.
    pub events: usize,
}

/// `ggen reverse cheats` — project the live `GGEN-*` diagnostic species + the
/// fake-inventory ledger into a defect graph + candidate SHACL.
///
/// Requires `--features lsp`: the diagnostic species are read from the
/// authoritative `ggen_lsp::route::species_registry()` (single source of truth,
/// no duplicated table).
#[cfg(feature = "lsp")]
#[verb]
pub fn cheats(ledger: Option<String>) -> VerbResult<ReverseCheatsOutput> {
    let project_root =
        std::env::current_dir().map_err(|e| NounVerbError::execution_error(format!("cwd: {e}")))?;

    let species: Vec<reverse::DefectSpeciesInput> = ggen_lsp::route::species_registry()
        .iter()
        .map(|s| reverse::DefectSpeciesInput {
            code: s.code.to_string(),
            failure_class: s.failure_class.to_string(),
            surfaces: s.surfaces.iter().map(|x| (*x).to_string()).collect(),
            severity_policy: s.severity_policy.to_string(),
            route: s.route.to_string(),
            origin: s.origin.to_string(),
            actuation_boundary: s.actuation_boundary.to_string(),
            receipt_requirement: s.receipt_requirement.to_string(),
            detector_active: s.detector_active,
        })
        .collect();

    let ledger_path = ledger.map(|p| absolutize(&p, &project_root));
    let report = reverse::extract_defects(&project_root, &species, ledger_path.as_deref())
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;
    persist_events(&report.events, &project_root)?;

    Ok(ReverseCheatsOutput {
        status: "success".to_string(),
        species_defects: report.species_defects,
        ledger_defects: report.ledger_defects,
        triples: report.triples,
        defects_ttl: rel(&project_root, &report.defects_ttl),
        shapes: report.shape_files.len(),
        receipt_path: rel(&project_root, &report.receipt_path),
        events: report.events.len(),
    })
}

/// Split a comma-separated `paths` argument into roots, resolving each relative
/// to `project_root`.
fn resolve_paths(paths: &str, project_root: &Path) -> Vec<PathBuf> {
    paths
        .split(',')
        .map(str::trim)
        .filter(|p| !p.is_empty())
        .map(|p| absolutize(p, project_root))
        .collect()
}

/// Resolve `p` relative to `project_root` unless it is already absolute.
fn absolutize(p: &str, project_root: &Path) -> PathBuf {
    let pb = PathBuf::from(p);
    if pb.is_absolute() {
        pb
    } else {
        project_root.join(pb)
    }
}

/// Render `path` relative to `root` (forward slashes), falling back to the full path.
fn rel(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}
