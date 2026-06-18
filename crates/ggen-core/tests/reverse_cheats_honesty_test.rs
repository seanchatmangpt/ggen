//! Honesty test for `ggen reverse cheats` (ARM 2: diagnostics + ledger → defect RDF).
//!
//! Chicago TDD: real temp filesystem, real markdown ledger, real RDF. Proves:
//! - real output: the defect graph + candidate shapes parse in oxigraph;
//! - traceability: every ledger defect maps to a real, on-disk `file:line`;
//!   a cited file that does not exist is DROPPED, never fabricated;
//! - candidate honesty: shapes are marked "NOT yet enforced" and trace to a
//!   real failure class;
//! - fail-loud sabotage: missing explicit ledger / empty species → `Err`.
//!
//! The "exactly 11 species, all codes ∈ registry" check lives in the ggen-cli
//! test where the authoritative `species_registry()` is available.

use std::path::PathBuf;

use ggen_core::graph::Graph;
use ggen_core::reverse::{extract_defects, DefectSpeciesInput, ReverseReceipt};

fn temp_project() -> PathBuf {
    let root = std::env::temp_dir().join(format!("ggen_reverse_cheats_{}", uuid::Uuid::new_v4()));
    std::fs::create_dir_all(&root).expect("create project root");
    root
}

fn synthetic_species() -> Vec<DefectSpeciesInput> {
    vec![
        DefectSpeciesInput {
            code: "GGEN-TPL-001".to_string(),
            failure_class: "unbound_projection".to_string(),
            surfaces: vec!["ggen.toml".to_string(), "SPARQL".to_string(), "Tera".to_string()],
            severity_policy: "error".to_string(),
            route: "source_law_repair".to_string(),
            origin: "test".to_string(),
            actuation_boundary: "inspect_only".to_string(),
            receipt_requirement: "diagnostic_receipt".to_string(),
            detector_active: true,
        },
        DefectSpeciesInput {
            code: "GGEN-QUERY-002".to_string(),
            failure_class: "blindspot".to_string(),
            surfaces: vec!["SPARQL".to_string()],
            severity_policy: "warning".to_string(),
            route: "advisory".to_string(),
            origin: "test".to_string(),
            actuation_boundary: "inspect_only".to_string(),
            receipt_requirement: "diagnostic_receipt".to_string(),
            detector_active: true,
        },
    ]
}

#[test]
fn cheats_emits_parseable_defects_and_candidate_shapes() {
    let proj = temp_project();
    // A real file the ledger will cite (must exist for its defect to be admitted).
    let real_rel = "crates/x/src/foo.rs";
    std::fs::create_dir_all(proj.join("crates/x/src")).expect("mkdir");
    std::fs::write(proj.join(real_rel), "pub fn f() {}\n").expect("write real file");

    // A ledger with one real-file row and one phantom-file row.
    let ledger = proj.join("ledger.md");
    std::fs::write(
        &ledger,
        format!(
            "\
| file:line | pattern | classification | live-path? | disposition |
|---|---|---|---|---|
| `{real_rel}:1` | decorative | **LIVE-PATH-MUST-FIX** | **Y** | prints success without work |
| `crates/ghost/does_not_exist.rs:99` | phantom | TEST-OR-BENIGN | N | should be dropped |
"
        ),
    )
    .expect("write ledger");

    let species = synthetic_species();
    let report = extract_defects(&proj, &species, Some(&ledger)).expect("cheats succeeds");

    // Species defects: exactly what we passed in. Ledger: only the existing file.
    assert_eq!(report.species_defects, 2);
    assert_eq!(
        report.ledger_defects, 1,
        "phantom file row must be dropped, not fabricated"
    );

    // The defect graph is real, parseable RDF carrying the species codes.
    let ttl = std::fs::read_to_string(&report.defects_ttl).expect("read defects ttl");
    Graph::load_from_string(&ttl).expect("defect graph must parse as real RDF");
    assert!(ttl.contains("\"GGEN-TPL-001\""), "species code present");
    assert!(ttl.contains("defect:DiagnosticSpecies"), "species typed");
    assert!(ttl.contains("defect:LedgerEntry"), "ledger entry typed");
    assert!(ttl.contains(real_rel), "real cited file present");
    assert!(
        !ttl.contains("does_not_exist.rs"),
        "phantom file must not appear in the graph"
    );

    // Candidate shapes: one per failure class, each real RDF and clearly a candidate.
    assert_eq!(report.shape_files.len(), 2, "one shape per failure class");
    for shape in &report.shape_files {
        let s = std::fs::read_to_string(shape).expect("read shape");
        Graph::load_from_string(&s).expect("candidate shape must parse as real SHACL/RDF");
        assert!(
            s.contains("NOT yet enforced"),
            "shape must be marked a candidate, not an active gate: {}",
            shape.display()
        );
        assert!(s.contains("sh:NodeShape"), "shape is a real NodeShape");
    }

    // The receipt is honest: real ledger input hash + output hashes.
    let raw = std::fs::read_to_string(&report.receipt_path).expect("read receipt");
    let receipt: ReverseReceipt = serde_json::from_str(&raw).expect("parse receipt");
    assert_eq!(receipt.operation, "reverse-cheats");
    assert!(
        receipt.input_hashes.contains_key("ledger:FAKE_INVENTORY_LEDGER.md"),
        "receipt binds the ledger: {:?}",
        receipt.input_hashes
    );
    assert!(
        receipt.output_hashes.contains_key("defects.ttl"),
        "receipt lists the defect graph output"
    );

    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn missing_explicit_ledger_fails_loudly() {
    let proj = temp_project();
    let missing = proj.join("nope.md");
    let result = extract_defects(&proj, &synthetic_species(), Some(&missing));
    assert!(
        result.is_err(),
        "an explicitly provided but missing ledger must error"
    );
    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn empty_species_fails_loudly() {
    let proj = temp_project();
    let result = extract_defects(&proj, &[], None);
    assert!(result.is_err(), "no species must fail loudly");
    let _ = std::fs::remove_dir_all(&proj);
}

#[test]
fn species_only_run_without_ledger_still_emits_defects() {
    // With no ledger present, the run still emits the species defect graph.
    let proj = temp_project();
    let report = extract_defects(&proj, &synthetic_species(), None).expect("species-only run");
    assert_eq!(report.species_defects, 2);
    assert_eq!(report.ledger_defects, 0);
    assert!(report.defects_ttl.exists());
    let _ = std::fs::remove_dir_all(&proj);
}
