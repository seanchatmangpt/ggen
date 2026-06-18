//! ARM 2: `ggen reverse cheats` — diagnostics + fake-ledger → defect graph + candidate SHACL.
//!
//! Projects two real sources of "detected cheats" into an admitted
//! standard-work graph:
//! - the live `GGEN-*` diagnostic species (passed in as [`DefectSpeciesInput`]
//!   so this core stays free of a `ggen-lsp` dependency — the CLI fills them
//!   from `ggen_lsp::route::diagnostic_species::species_registry()`), and
//! - the curated `docs/receipts/FAKE_INVENTORY_LEDGER.md` triage table.
//!
//! Honesty: a defect node is emitted only if it traces to a real species code
//! or a real, on-disk `file:line` from the ledger. Unparseable ledger rows are
//! skipped, never fabricated. Candidate SHACL shapes are written under
//! `candidates/` and explicitly marked "not yet enforced".

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::utils::error::{Error, Result};

use super::events::{activity, obj_type, ReverseEvent};
use super::receipt::ReverseReceipt;
use super::turtle::{Object, TripleSet};
use super::vocab;

/// Neutral mirror of `ggen_lsp::route::diagnostic_species::DiagnosticSpecies`.
///
/// Lives here so the cheats engine does not depend on `ggen-lsp`. The CLI
/// transcribes the authoritative registry into this shape.
#[derive(Debug, Clone)]
pub struct DefectSpeciesInput {
    /// Stable diagnostic code, e.g. `"GGEN-TPL-001"`.
    pub code: String,
    /// Failure class, e.g. `"unbound_projection"`.
    pub failure_class: String,
    /// Source-law surfaces involved.
    pub surfaces: Vec<String>,
    /// Severity policy slug.
    pub severity_policy: String,
    /// Repair route slug.
    pub route: String,
    /// Provenance origin.
    pub origin: String,
    /// Actuation boundary, e.g. `"inspect_only"`.
    pub actuation_boundary: String,
    /// Receipt requirement slug.
    pub receipt_requirement: String,
    /// Whether a detector is currently active for this species.
    pub detector_active: bool,
}

/// A single ledger triage row that parsed to a real `file:line`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LedgerDefect {
    /// Cited file (project-relative, as written in the ledger).
    pub file: String,
    /// Cited line number.
    pub line: u32,
    /// Triage classification (`LIVE-PATH-MUST-FIX` / `DORMANT` / `TEST-OR-BENIGN`).
    pub classification: String,
    /// Disposition text.
    pub disposition: String,
}

/// Result of a `ggen reverse cheats` run.
#[derive(Debug, Clone)]
pub struct ReverseCheatsReport {
    /// Path to the written defect graph.
    pub defects_ttl: PathBuf,
    /// Candidate SHACL shape files (one per failure class).
    pub shape_files: Vec<PathBuf>,
    /// Path to the written provenance receipt.
    pub receipt_path: PathBuf,
    /// Number of species-origin defect nodes.
    pub species_defects: usize,
    /// Number of ledger-origin defect nodes (existing files only).
    pub ledger_defects: usize,
    /// Triples in the defect graph.
    pub triples: usize,
    /// Neutral OCEL events for the CLI to log.
    pub events: Vec<ReverseEvent>,
}

/// Build the defect graph + candidate SHACL from the species registry and the
/// fake-inventory ledger.
///
/// `ledger_path` semantics:
/// - `Some(p)` and `p` missing → `Err` (fail loud);
/// - `None` → default `docs/receipts/FAKE_INVENTORY_LEDGER.md` under
///   `project_root`; if absent, the run proceeds with zero ledger defects.
pub fn extract_defects(
    project_root: &Path,
    species: &[DefectSpeciesInput],
    ledger_path: Option<&Path>,
) -> Result<ReverseCheatsReport> {
    if species.is_empty() {
        return Err(Error::new(
            "reverse cheats requires at least one diagnostic species",
        ));
    }

    let mut events = vec![ReverseEvent::new(
        activity::CHEATS_STARTED,
        "defects",
        obj_type::DEFECT,
    )
    .with_attr("species", species.len().to_string())];

    let mut graph = TripleSet::new();
    graph.add_prefix(vocab::DEFECT, vocab::DEFECT_NS);
    graph.add_prefix(vocab::RDFS, vocab::RDFS_NS);
    graph.add_prefix(vocab::XSD, vocab::XSD_NS);

    // ── Species-origin defects ───────────────────────────────────────────
    for sp in species {
        let subject = vocab::defect(&local_name(&sp.code));
        graph.insert(&subject, "a", Object::Iri(vocab::defect("Defect")));
        graph.insert(
            &subject,
            "a",
            Object::Iri(vocab::defect("DiagnosticSpecies")),
        );
        graph.insert(&subject, &vocab::defect("code"), Object::Str(sp.code.clone()));
        graph.insert(
            &subject,
            &vocab::defect("failureClass"),
            Object::Str(sp.failure_class.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("severityPolicy"),
            Object::Str(sp.severity_policy.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("route"),
            Object::Str(sp.route.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("origin"),
            Object::Str(sp.origin.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("actuationBoundary"),
            Object::Str(sp.actuation_boundary.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("receiptRequirement"),
            Object::Str(sp.receipt_requirement.clone()),
        );
        graph.insert(
            &subject,
            &vocab::defect("detectorActive"),
            Object::Typed {
                value: sp.detector_active.to_string(),
                datatype: vocab::xsd("boolean"),
            },
        );
        for surface in &sp.surfaces {
            graph.insert(
                &subject,
                &vocab::defect("surface"),
                Object::Str(surface.clone()),
            );
        }
        events.push(
            ReverseEvent::new(activity::CHEATS_DEFECT, &sp.code, obj_type::DEFECT)
                .with_attr("origin", "diagnostic_species")
                .with_attr("failure_class", sp.failure_class.clone()),
        );
    }
    let species_defects = species.len();

    // ── Ledger-origin defects ────────────────────────────────────────────
    let mut receipt = ReverseReceipt::new("reverse-cheats");
    let resolved_ledger = match ledger_path {
        Some(p) => {
            if !p.exists() {
                return Err(Error::new(&format!(
                    "ledger file does not exist: {}",
                    p.display()
                )));
            }
            Some(p.to_path_buf())
        }
        None => {
            let default = project_root
                .join("docs")
                .join("receipts")
                .join("FAKE_INVENTORY_LEDGER.md");
            default.exists().then_some(default)
        }
    };

    let mut ledger_defects = 0usize;
    if let Some(ledger) = &resolved_ledger {
        let text = std::fs::read_to_string(ledger)
            .map_err(|e| Error::new(&format!("read ledger {}: {e}", ledger.display())))?;
        receipt.add_input("ledger:FAKE_INVENTORY_LEDGER.md", text.as_bytes());

        for defect in parse_ledger(&text) {
            // Honesty gate: only emit a defect for a file that actually exists.
            if !project_root.join(&defect.file).exists() {
                continue;
            }
            let subject = vocab::defect(&format!(
                "ledger__{}__{}",
                local_name(&defect.file),
                defect.line
            ));
            graph.insert(&subject, "a", Object::Iri(vocab::defect("Defect")));
            graph.insert(&subject, "a", Object::Iri(vocab::defect("LedgerEntry")));
            graph.insert(
                &subject,
                &vocab::defect("ledgerFile"),
                Object::Str(defect.file.clone()),
            );
            graph.insert(
                &subject,
                &vocab::defect("ledgerLine"),
                Object::Typed {
                    value: defect.line.to_string(),
                    datatype: vocab::xsd("integer"),
                },
            );
            graph.insert(
                &subject,
                &vocab::defect("classification"),
                Object::Str(defect.classification.clone()),
            );
            graph.insert(
                &subject,
                &vocab::defect("disposition"),
                Object::Str(defect.disposition.clone()),
            );
            events.push(
                ReverseEvent::new(
                    activity::CHEATS_DEFECT,
                    &format!("{}:{}", defect.file, defect.line),
                    obj_type::DEFECT,
                )
                .with_attr("origin", "ledger")
                .with_attr("classification", defect.classification.clone()),
            );
            ledger_defects += 1;
        }
    }

    // ── Write the defect graph ───────────────────────────────────────────
    let ttl = graph.to_turtle()?;
    let defects_dir = project_root.join(".specify").join("defects");
    std::fs::create_dir_all(&defects_dir)
        .map_err(|e| Error::new(&format!("create defects dir: {e}")))?;
    let defects_ttl = defects_dir.join("defects.ttl");
    std::fs::write(&defects_ttl, ttl.as_bytes())
        .map_err(|e| Error::new(&format!("write defects ttl: {e}")))?;
    receipt.add_output("defects.ttl", ttl.as_bytes());

    // ── Candidate SHACL shapes (one per failure class) ───────────────────
    let shapes_dir = project_root
        .join(".specify")
        .join("shapes")
        .join("candidates");
    std::fs::create_dir_all(&shapes_dir)
        .map_err(|e| Error::new(&format!("create shapes dir: {e}")))?;

    // Group species by failure class (BTreeMap → deterministic order).
    let mut by_class: BTreeMap<String, Vec<&DefectSpeciesInput>> = BTreeMap::new();
    for sp in species {
        by_class
            .entry(sp.failure_class.clone())
            .or_default()
            .push(sp);
    }

    let mut shape_files = Vec::new();
    for (failure_class, members) in &by_class {
        let shape_ttl = build_candidate_shape(failure_class, members)?;
        let path = shapes_dir.join(format!("{}.shacl.ttl", local_name(failure_class)));
        std::fs::write(&path, shape_ttl.as_bytes())
            .map_err(|e| Error::new(&format!("write shape {}: {e}", path.display())))?;
        receipt.add_output(
            &format!("shape:{failure_class}"),
            shape_ttl.as_bytes(),
        );
        shape_files.push(path);
    }

    // ── Receipt ──────────────────────────────────────────────────────────
    let receipt_path = project_root
        .join(".ggen")
        .join("receipts")
        .join(format!("reverse-cheats-{}.json", receipt.operation_id));
    receipt.write_to(&receipt_path)?;

    events.push(
        ReverseEvent::new(activity::CHEATS_COMPLETED, "defects", obj_type::DEFECT)
            .with_attr("species_defects", species_defects.to_string())
            .with_attr("ledger_defects", ledger_defects.to_string())
            .with_attr("triples", graph.len().to_string())
            .with_attr("shapes", shape_files.len().to_string()),
    );
    events.push(
        ReverseEvent::new(
            activity::RECEIPT_EMITTED,
            &receipt.operation_id,
            obj_type::RECEIPT,
        )
        .with_attr("path", relative_path(project_root, &receipt_path)),
    );

    Ok(ReverseCheatsReport {
        defects_ttl,
        shape_files,
        receipt_path,
        species_defects,
        ledger_defects,
        triples: graph.len(),
        events,
    })
}

/// Build one candidate SHACL `NodeShape` for a failure class.
///
/// The shape is real, parseable SHACL that validates the *defect graph* (every
/// `defect:DiagnosticSpecies` must carry a `defect:code`), and is explicitly
/// marked a candidate — it is not claimed to be an enforced gate.
fn build_candidate_shape(failure_class: &str, members: &[&DefectSpeciesInput]) -> Result<String> {
    let mut codes: Vec<String> = members.iter().map(|m| m.code.clone()).collect();
    codes.sort();
    let mut surfaces: Vec<String> = members
        .iter()
        .flat_map(|m| m.surfaces.iter().cloned())
        .collect();
    surfaces.sort();
    surfaces.dedup();
    let severity = members
        .iter()
        .map(|m| shacl_severity(&m.severity_policy))
        .max_by_key(|s| severity_rank(s))
        .unwrap_or("sh:Info");

    let mut ts = TripleSet::new();
    ts.add_prefix(vocab::SH, vocab::SH_NS);
    ts.add_prefix(vocab::DEFECT, vocab::DEFECT_NS);
    ts.add_prefix(vocab::RDFS, vocab::RDFS_NS);
    ts.add_prefix(vocab::XSD, vocab::XSD_NS);

    let shape = vocab::defect(&format!("shape_{}", local_name(failure_class)));
    let prop = vocab::defect(&format!("shape_{}__code", local_name(failure_class)));

    ts.insert(&shape, "a", Object::Iri(vocab::sh("NodeShape")));
    ts.insert(
        &shape,
        "rdfs:label",
        Object::Str(format!("{failure_class} (candidate)")),
    );
    ts.insert(
        &shape,
        "rdfs:comment",
        Object::Str(format!(
            "Candidate shape derived from {}. NOT yet enforced as a gate. Law surfaces: {}.",
            codes.join(", "),
            surfaces.join(", ")
        )),
    );
    ts.insert(
        &shape,
        &vocab::sh("targetClass"),
        Object::Iri(vocab::defect("DiagnosticSpecies")),
    );
    ts.insert(&shape, &vocab::sh("severity"), Object::Iri(severity.to_string()));
    ts.insert(&shape, &vocab::sh("property"), Object::Iri(prop.clone()));

    ts.insert(&prop, "a", Object::Iri(vocab::sh("PropertyShape")));
    ts.insert(&prop, &vocab::sh("path"), Object::Iri(vocab::defect("code")));
    ts.insert(
        &prop,
        &vocab::sh("minCount"),
        Object::Typed {
            value: "1".to_string(),
            datatype: vocab::xsd("integer"),
        },
    );
    ts.insert(
        &prop,
        &vocab::sh("datatype"),
        Object::Iri(vocab::xsd("string")),
    );

    ts.to_turtle()
}

fn shacl_severity(severity_policy: &str) -> &'static str {
    match severity_policy {
        "error" | "release_blocking" => "sh:Violation",
        "warning" => "sh:Warning",
        _ => "sh:Info",
    }
}

fn severity_rank(severity: &str) -> u8 {
    match severity {
        "sh:Violation" => 2,
        "sh:Warning" => 1,
        _ => 0,
    }
}

/// Parse the fake-inventory ledger markdown table into candidate defects.
///
/// Only rows whose first cell is a backticked `` `file:line` `` are returned;
/// header rows, separators, and prose are ignored. No row is fabricated.
pub fn parse_ledger(text: &str) -> Vec<LedgerDefect> {
    let mut defects = Vec::new();
    for line in text.lines() {
        let trimmed = line.trim();
        if !trimmed.starts_with('|') {
            continue;
        }
        let cells: Vec<&str> = trimmed.split('|').map(str::trim).collect();
        // Leading/trailing empty cells from the bounding pipes ⇒ need ≥ 6 parts
        // for the 5-column table: ["", file:line, pattern, class, live, disp, ""].
        if cells.len() < 6 {
            continue;
        }
        let file_cell = cells[1].trim_matches('`').trim();
        let Some((file, line_str)) = file_cell.rsplit_once(':') else {
            continue;
        };
        let Ok(line_no) = line_str.trim().parse::<u32>() else {
            continue; // header ("file:line"), separator ("---"), or prose
        };
        let classification = cells[3].replace('*', "").trim().to_string();
        let disposition = cells[5].to_string();
        if file.is_empty() || classification.is_empty() {
            continue;
        }
        defects.push(LedgerDefect {
            file: file.to_string(),
            line: line_no,
            classification,
            disposition,
        });
    }
    defects
}

/// Map a string to characters safe inside an IRI local name.
fn local_name(input: &str) -> String {
    input
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c,
            _ => '_',
        })
        .collect()
}

fn relative_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ledger_extracts_only_real_rows() {
        let text = "\
| file:line | pattern | classification | live-path? | disposition |
|---|---|---|---|---|
| `crates/foo/bar.rs:245` | \"simulated\" | DORMANT | N | Non-compiled crate. |
| `crates/ggen-cli/src/cmds/wizard.rs:449` | decorative | **LIVE-PATH-MUST-FIX** | **Y** | prints success without work |
| not a table row, prose |
";
        let defects = parse_ledger(text);
        assert_eq!(defects.len(), 2, "header + separator + prose must be skipped");
        assert_eq!(defects[0].file, "crates/foo/bar.rs");
        assert_eq!(defects[0].line, 245);
        assert_eq!(defects[0].classification, "DORMANT");
        assert_eq!(defects[1].file, "crates/ggen-cli/src/cmds/wizard.rs");
        assert_eq!(defects[1].line, 449);
        assert_eq!(
            defects[1].classification, "LIVE-PATH-MUST-FIX",
            "bold markers must be stripped"
        );
    }

    #[test]
    fn shacl_severity_maps_policies() {
        assert_eq!(shacl_severity("error"), "sh:Violation");
        assert_eq!(shacl_severity("release_blocking"), "sh:Violation");
        assert_eq!(shacl_severity("warning"), "sh:Warning");
        assert_eq!(shacl_severity("unknown"), "sh:Info");
    }
}
