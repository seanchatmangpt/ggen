//! Anti-vacuity audit of the self-play sweep.
//!
//! `every_pack_survives_the_full_lifecycle` passing across 73 packs proves
//! nothing if most of those packs never reached the write path. Every
//! write-side invariant (`WriteImpliesReceipt`, `Idempotent`) is guarded by
//! `applied_ok == Some(true) && !written.is_empty()` — so a pack whose
//! template renders to nothing skips those checks *silently* and still
//! reports clean.
//!
//! A red team wanting a green suite that tests nothing would arrange exactly
//! that. These tests measure real coverage and fail if it collapses.

use std::path::{Path, PathBuf};

use ggen_mcp::selfplay::{Board, Case, CaseOrigin};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("repo root")
        .to_path_buf()
}

fn packs_with_ontology() -> Vec<PathBuf> {
    let mut out: Vec<PathBuf> = std::fs::read_dir(repo_root().join("packs"))
        .expect("packs/")
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.is_dir() && p.join("ontology.ttl").is_file())
        .collect();
    out.sort();
    out
}

fn sweep_case(name: &str) -> Case {
    Case {
        id: format!("vacuity-{name}"),
        pack: name.to_string(),
        // Identical to the sweep's probe: this audits the real sweep, not a
        // friendlier variant of it.
        sparql: "SELECT ?s ?p ?o WHERE { ?s ?p ?o } ORDER BY ?s ?p ?o LIMIT 3".to_string(),
        to: "out/sweep.txt".to_string(),
        body: "{% for row in probe %}{{ row.s }}\n{% endfor %}".to_string(),
        origin: CaseOrigin::Handwritten,
        expected_violation: None,
        note: None,
    }
}

/// Measure how much of the lifecycle the sweep actually exercises, and
/// refuse a sweep that has quietly become an empty ritual.
#[test]
fn the_sweep_actually_exercises_the_write_path() {
    let packs = packs_with_ontology();
    let mut queried = 0usize;
    let mut nonzero_rows = 0usize;
    let mut applied = 0usize;
    let mut wrote = 0usize;
    let mut receipted = 0usize;
    let mut recounted = 0usize;
    let mut idempotence_checked = 0usize;
    let mut no_write: Vec<String> = Vec::new();

    for dir in &packs {
        let name = dir
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        let Ok(board) = Board::new(dir) else { continue };
        let (_, obs) = board.play(&sweep_case(&name));

        if obs.query_ok == Some(true) {
            queried += 1;
        }
        if obs.reported_rows.is_some_and(|n| n > 0) {
            nonzero_rows += 1;
        }
        if obs.independent_rows.is_some() {
            recounted += 1;
        }
        if obs.applied_ok == Some(true) {
            applied += 1;
        }
        if obs.written.is_empty() {
            no_write.push(name);
        } else {
            wrote += 1;
            if obs.receipt_verified == Some(true) {
                receipted += 1;
            }
            if obs.second_apply_written.is_some() {
                idempotence_checked += 1;
            }
        }
    }

    let total = packs.len();
    eprintln!(
        "sweep coverage over {total} pack(s):\n  \
         queried ok        {queried}\n  \
         nonzero rows      {nonzero_rows}\n  \
         independent count {recounted}\n  \
         apply ok          {applied}\n  \
         actually wrote    {wrote}\n  \
         receipt verified  {receipted}\n  \
         idempotence run   {idempotence_checked}"
    );
    if !no_write.is_empty() {
        eprintln!(
            "  packs that wrote nothing ({}): {}",
            no_write.len(),
            no_write.join(", ")
        );
    }

    // Query coverage: essentially every pack must at least load and answer.
    assert!(
        queried * 100 / total >= 95,
        "only {queried}/{total} packs answered a query — the sweep is not \
         exercising the read path"
    );

    // Write coverage is the load-bearing one. If this collapses, every
    // write-side invariant in the sweep is silently skipped and the suite
    // is decorative.
    assert!(
        wrote * 2 > total,
        "only {wrote}/{total} packs reached the write path — the majority of \
         the sweep never exercises apply/receipt/idempotence, so those \
         invariants are vacuous. Fix the probe (or the pack corpus) rather \
         than trusting the green result."
    );

    // A write without a verified receipt would be caught by the referee, but
    // a write that never checks the receipt would not — assert the check ran.
    assert_eq!(
        receipted, wrote,
        "{wrote} pack(s) wrote but only {receipted} had a verified receipt"
    );
    assert_eq!(
        idempotence_checked, wrote,
        "{wrote} pack(s) wrote but idempotence was only evaluated for \
         {idempotence_checked}"
    );

    // The honest-count check is skipped whenever the recount cannot be
    // formed. Report the real rate so a silent collapse is visible.
    assert!(
        recounted * 2 > total,
        "independent recount succeeded for only {recounted}/{total} packs — \
         HonestRowCount is being skipped for most of the sweep"
    );
}

/// The corpus's adversarial cases must actually reach the stage they are
/// probing. A traversal case that never reaches the write path proves
/// nothing about traversal.
#[test]
fn traversal_corpus_cases_actually_reach_the_write_path() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/corpus");
    let mut checked = 0usize;
    for entry in std::fs::read_dir(&dir).expect("corpus").flatten() {
        let p = entry.path();
        if p.extension().is_none_or(|x| x != "json") {
            continue;
        }
        let case: Case =
            serde_json::from_str(&std::fs::read_to_string(&p).expect("read")).expect("parse");
        if !case.to.contains("..") && !case.to.starts_with('/') {
            continue;
        }
        let board = Board::new(&repo_root().join("packs").join(&case.pack)).expect("board");
        let (_, obs) = board.play(&case);
        // The query must have succeeded, otherwise the case never got far
        // enough for `to:` to matter and the traversal probe is vacuous.
        assert_eq!(
            obs.query_ok,
            Some(true),
            "traversal case {:?} never executed its query, so it does not \
             actually probe the write path",
            case.id
        );
        assert!(
            obs.changed_outside_root.is_empty(),
            "traversal case {:?} escaped: {:?}",
            case.id,
            obs.changed_outside_root
        );
        checked += 1;
    }
    assert!(
        checked >= 3,
        "expected the traversal probes in the corpus, found {checked}"
    );
}
