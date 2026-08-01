//! Manufacturing step: computes and WRITES `docs/v26.8.1/coverage-matrix.csv`.
//!
//! This is the only v26.8.1 binary permitted to write that file. It:
//! 1. Invokes the external subsystem verifier (building it if needed) via
//!    the shared `run_subsystem_verifier` -- the same call the crown makes.
//! 2. Computes the canonical coverage projection in memory
//!    (`project_coverage_rows`, from `v26_8_1_tools::coverage_projection`).
//! 3. Writes `docs/v26.8.1/coverage-matrix.csv`.
//! 4. Emits a projection report and a BLAKE3 receipt binding: the
//!    subsystem verifier's report digest, the real source head, the
//!    generated CSV's own content digest, and digests of the evidence
//!    manifest that fed the projection.
//!
//! Invoked via `just v26-8-1-project-coverage` (see root `justfile`).

use anyhow::{Context, Result};
use serde::Serialize;
use std::env;
use std::fs;

use v26_8_1_tools::coverage_projection::{
    exact_head, project_coverage_rows, relative, resolve_root, run_subsystem_verifier,
    subsystem_manifest_digest, subsystem_verifier_report_digest, write_coverage_csv, COVERAGE_PATH,
    SUBSYSTEM_MANIFEST_REL, SUBSYSTEM_VERIFIER_REPORT_REL,
};

#[derive(Debug, Serialize)]
struct ProjectionReport {
    schema_version: String,
    source_head: String,
    coverage_path: String,
    row_count: usize,
    subsystem_verifier_report_path: String,
    subsystem_verifier_report_digest: String,
    subsystem_evidence_manifest_path: String,
    subsystem_evidence_manifest_digest: String,
    coverage_csv_digest: String,
    rows: Vec<v26_8_1_tools::coverage_projection::CoverageRow>,
}

#[derive(Debug, Serialize)]
struct ProjectionReceipt {
    schema_version: String,
    source_head: String,
    projection_report_path: String,
    projection_report_blake3: String,
    coverage_csv_path: String,
    coverage_csv_blake3: String,
    subsystem_verifier_report_digest: String,
    subsystem_evidence_manifest_digest: String,
}

fn main() {
    if let Err(error) = run() {
        eprintln!("project_coverage refused: {error:#}");
        std::process::exit(2);
    }
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    let root = resolve_root(&args)?;

    // Step 1: invoke the external subsystem verifier (same call the crown
    // will independently repeat during verification).
    let standings = run_subsystem_verifier(&root)?;

    // Step 2: compute the canonical projection purely in memory.
    let rows = project_coverage_rows(&standings);

    // Step 3: write docs/v26.8.1/coverage-matrix.csv -- the ONLY place in
    // this codebase that does so.
    let csv_bytes = write_coverage_csv(&root, &rows)?;
    let coverage_csv_digest = blake3::hash(&csv_bytes).to_hex().to_string();

    let source_head = exact_head(&root);
    let subsystem_report_digest = subsystem_verifier_report_digest(&root)
        .context("digest subsystem-verifier-report.json after fresh run")?;
    let manifest_digest =
        subsystem_manifest_digest(&root).context("digest subsystem-evidence-manifest.json")?;

    // Step 4: projection report + BLAKE3 receipt.
    let report = ProjectionReport {
        schema_version: "ggen.v26.8.1.coverage-projection-report/1".into(),
        source_head: source_head.clone(),
        coverage_path: COVERAGE_PATH.into(),
        row_count: rows.len(),
        subsystem_verifier_report_path: SUBSYSTEM_VERIFIER_REPORT_REL.into(),
        subsystem_verifier_report_digest: subsystem_report_digest.clone(),
        subsystem_evidence_manifest_path: SUBSYSTEM_MANIFEST_REL.into(),
        subsystem_evidence_manifest_digest: manifest_digest.clone(),
        coverage_csv_digest: coverage_csv_digest.clone(),
        rows,
    };
    let evidence_root = root.join(".ggen/v26.8.1");
    fs::create_dir_all(&evidence_root)?;
    let report_path = evidence_root.join("coverage-projection-report.json");
    let report_bytes = serde_json::to_vec_pretty(&report)?;
    fs::write(&report_path, &report_bytes)?;

    let receipt = ProjectionReceipt {
        schema_version: "ggen.v26.8.1.coverage-projection-receipt/1".into(),
        source_head,
        projection_report_path: relative(&root, &report_path),
        projection_report_blake3: blake3::hash(&report_bytes).to_hex().to_string(),
        coverage_csv_path: COVERAGE_PATH.into(),
        coverage_csv_blake3: coverage_csv_digest,
        subsystem_verifier_report_digest: subsystem_report_digest,
        subsystem_evidence_manifest_digest: manifest_digest,
    };
    let receipt_path = evidence_root.join("coverage-projection-receipt.json");
    fs::write(&receipt_path, serde_json::to_vec_pretty(&receipt)?)?;

    println!("coverage_csv={}", COVERAGE_PATH);
    println!("row_count={}", report.row_count);
    println!("report={}", relative(&root, &report_path));
    println!("receipt={}", relative(&root, &receipt_path));
    Ok(())
}
