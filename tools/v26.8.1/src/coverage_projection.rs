//! Coverage-matrix projection: the ONE shared implementation of "how
//! `docs/v26.8.1/coverage-matrix.csv` is computed from the subsystem
//! verifier's real per-subsystem output".
//!
//! # Manufacturing vs. verification split
//!
//! Before this module existed, `tools/v26.8.1/src/main.rs` (the "crown"
//! verifier) both computed AND wrote `coverage-matrix.csv` during what was
//! supposed to be pure verification -- an observer mutating the artifact it
//! observes. This module is the fix: the projection logic lives here, in
//! one place, as a pure function with no filesystem side effects
//! (`project_coverage_rows`). Two binaries consume it:
//!
//! - `tools/v26.8.1/src/bin/project_coverage.rs` (manufacturing): calls
//!   `project_coverage_rows`, then actually writes the result to
//!   `docs/v26.8.1/coverage-matrix.csv` via `write_coverage_csv`, plus a
//!   projection report + BLAKE3 receipt.
//! - `tools/v26.8.1/src/main.rs` (crown, verification): calls the exact
//!   same `project_coverage_rows`, serializes it with the exact same
//!   `serialize_coverage_csv`, and BYTE-COMPARES the result against
//!   whatever is currently on disk. It never writes. A mismatch is refused
//!   as `GENERATED_COVERAGE_DRIFT`, never silently repaired.
//!
//! Because both binaries call the identical function, the two paths cannot
//! drift from each other by construction -- there is only one
//! implementation to keep in sync with itself.
//!
//! # Canonical per-subsystem metadata
//!
//! The `document` / `authority_sources` / `implementation_sources` columns
//! are NOT derived from the subsystem verifier's report (which only knows
//! about `subsystem` / `standing` / legacy-closure counts) and are
//! deliberately NOT read from the current `coverage-matrix.csv` on disk
//! either -- reading them from the file being verified would make the
//! projection partially self-referential: a sabotaged disk file could feed
//! its own corruption back into the "expected" computation and the crown
//! would silently agree with the sabotage. Instead they are canonical,
//! versioned data baked into this module (`CANONICAL_SUBSYSTEMS`),
//! transcribed once from the human-curated subsystem/document mapping
//! (matching `tools/v26.8.1/subsystem_evidence_manifest.py`'s `SUBSYSTEMS`
//! dict, which is the authority for which docs map to which subsystem).
//! Row order is likewise canonical (the order below), not the order found
//! in any existing file -- this is what lets the crown detect
//! non-canonically reordered rows as drift.

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

pub const COVERAGE_PATH: &str = "docs/v26.8.1/coverage-matrix.csv";
pub const SUBSYSTEM_MANIFEST_REL: &str = ".ggen/v26.8.1/subsystem-evidence-manifest.json";
pub const SUBSYSTEM_VERIFIER_REPORT_REL: &str = ".ggen/v26.8.1/subsystem-verifier-report.json";
pub const SUBSYSTEM_VERIFIER_SOURCE_REL: &str = "tools/v26.8.1/src/bin/subsystem_verifier.rs";

/// One row of `docs/v26.8.1/coverage-matrix.csv`.
#[derive(Debug, Deserialize, Serialize, Clone, PartialEq, Eq)]
pub struct CoverageRow {
    pub document: String,
    pub subsystem: String,
    pub authority_sources: String,
    pub implementation_sources: String,
    pub verifier: String,
    pub legacy_disposition: String,
    pub standing: String,
}

/// Deserialized shape of `subsystem_verifier`'s own report -- the ONLY
/// source of subsystem standing this projection trusts.
#[derive(Debug, Deserialize, Clone)]
pub struct SubsystemVerifierStanding {
    pub subsystem: String,
    pub standing: String,
    pub legacy_total: usize,
    pub legacy_unknown: usize,
    pub legacy_fully_closed: bool,
    pub reasons: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct SubsystemVerifierReport {
    pub schema_version: String,
    pub subsystems: Vec<SubsystemVerifierStanding>,
}

/// Canonical (document, subsystem, authority_sources, implementation_sources)
/// metadata, in canonical row order. Transcribed from
/// `tools/v26.8.1/subsystem_evidence_manifest.py`'s `SUBSYSTEMS` dict order
/// and the pre-existing `docs/v26.8.1/coverage-matrix.csv` content (the
/// human-curated authority/implementation source mapping did not change;
/// only its custody -- disk file vs. compiled-in constant -- did).
pub const CANONICAL_SUBSYSTEMS: &[(&str, &str, &str, &str)] = &[
    (
        "00-governance/01-canon-and-authority.md",
        "governance",
        "AGENTS.md|README.md|CLAUDE.md",
        "docs|.claude|.specify",
    ),
    (
        "10-system/12-workspace-crate-map.md",
        "system",
        "Cargo.toml",
        "crates/*|src/*",
    ),
    (
        "20-engine/21-sync-pipeline.md",
        "engine",
        "README.md|CLAUDE.md",
        "crates/ggen-engine/src/sync.rs",
    ),
    (
        "30-graph/32-oxigraph-integration.md",
        "graph",
        "Cargo.toml",
        "crates/ggen-graph|crates/ggen-engine|crates/praxis-graphlaw",
    ),
    (
        "40-projection/41-tera-integration.md",
        "projection",
        "Cargo.toml|README.md",
        "crates/ggen-engine|templates",
    ),
    (
        "50-evidence/51-receipt-schema.md",
        "evidence",
        "AGENTS.md|README.md",
        "crates/ggen-engine|crates/ggen-graph|crates/praxis-core",
    ),
    (
        "60-products/61-cli-surface.md",
        "products",
        "CLAUDE.md|specs/014-ggen-core-replacement/contracts/cli-command-surface.md",
        "crates/ggen-cli|crates/ggen-engine/src/verbs",
    ),
    (
        "70-verification/71-verification-constitution.md",
        "verification",
        "AGENTS.md|justfile",
        "tests|crates/ggen-cheat-scanner|scripts/ci",
    ),
    (
        "80-economics/83-integrated-pipeline-model.md",
        "economics",
        "docs/v26.8.1/80-economics",
        "benches|crates/ggen-engine",
    ),
    (
        "90-legacy/93-capability-equivalence-matrix.md",
        "legacy",
        "specs/014-ggen-core-replacement|git-history",
        "all-subsystems",
    ),
];

/// Aggregate legacy-disposition marker for the coverage-matrix projection.
/// "UNKNOWN" whenever any mapped legacy capability has an unresolved
/// disposition, or none is mapped at all; "PRESERVED" only when every
/// mapped legacy capability's disposition is resolved.
pub fn aggregate_legacy_disposition(standing: &SubsystemVerifierStanding) -> &'static str {
    if standing.legacy_total > 0 && standing.legacy_fully_closed {
        "PRESERVED"
    } else {
        "UNKNOWN"
    }
}

/// Pure, in-memory computation of the canonical coverage-matrix projection.
/// No filesystem I/O. Given the same `standings`, always returns the same
/// rows in the same order -- this determinism is what the crown's
/// byte-compare depends on.
pub fn project_coverage_rows(standings: &[SubsystemVerifierStanding]) -> Vec<CoverageRow> {
    let by_subsystem: std::collections::BTreeMap<&str, &SubsystemVerifierStanding> = standings
        .iter()
        .map(|s| (s.subsystem.as_str(), s))
        .collect();
    let mut projected = Vec::with_capacity(CANONICAL_SUBSYSTEMS.len());
    for (document, subsystem, authority_sources, implementation_sources) in CANONICAL_SUBSYSTEMS {
        let (standing, legacy_disposition, verifier) = match by_subsystem.get(subsystem) {
            Some(s) => (
                s.standing.clone(),
                aggregate_legacy_disposition(s).to_owned(),
                SUBSYSTEM_VERIFIER_SOURCE_REL.to_owned(),
            ),
            None => (
                "UNKNOWN".to_owned(),
                "UNKNOWN".to_owned(),
                "UNASSIGNED".to_owned(),
            ),
        };
        projected.push(CoverageRow {
            document: (*document).to_owned(),
            subsystem: (*subsystem).to_owned(),
            authority_sources: (*authority_sources).to_owned(),
            implementation_sources: (*implementation_sources).to_owned(),
            verifier,
            legacy_disposition,
            standing,
        });
    }
    projected
}

/// Serialize coverage rows to CSV bytes, in memory, no filesystem I/O.
/// Both the manufacturing binary and the crown call this on the exact same
/// `project_coverage_rows` output, so the bytes either binary would write
/// are byte-identical by construction.
pub fn serialize_coverage_csv(rows: &[CoverageRow]) -> Result<Vec<u8>> {
    let mut writer = csv::Writer::from_writer(Vec::new());
    for row in rows {
        writer.serialize(row)?;
    }
    writer.flush()?;
    writer
        .into_inner()
        .context("flush in-memory coverage-matrix CSV writer")
}

/// Write freshly-projected rows to `docs/v26.8.1/coverage-matrix.csv`.
/// ONLY the manufacturing binary (`project_coverage`) calls this. The crown
/// must never call this function.
pub fn write_coverage_csv(root: &Path, rows: &[CoverageRow]) -> Result<Vec<u8>> {
    let bytes = serialize_coverage_csv(rows)?;
    fs::write(root.join(COVERAGE_PATH), &bytes)
        .with_context(|| format!("write {}", COVERAGE_PATH))?;
    Ok(bytes)
}

/// Read the current on-disk coverage-matrix.csv bytes verbatim (no
/// deserialize/reserialize round-trip) so a byte-compare against a freshly
/// serialized expectation is meaningful even for whitespace/ordering
/// differences a `Vec<CoverageRow>` comparison alone would not catch.
pub fn read_coverage_csv_bytes(root: &Path) -> Result<Vec<u8>> {
    fs::read(root.join(COVERAGE_PATH)).with_context(|| format!("read {}", COVERAGE_PATH))
}

/// Resolve the repository root the same way every v26.8.1 binary does:
/// explicit `--root <path>`, else walk up from cwd looking for the marker
/// files.
pub fn resolve_root(args: &[String]) -> Result<PathBuf> {
    let explicit = args
        .windows(2)
        .find(|pair| pair[0] == "--root")
        .map(|pair| PathBuf::from(&pair[1]));
    let mut current = explicit.unwrap_or(std::env::current_dir()?);
    loop {
        if current.join("Cargo.toml").is_file() && current.join("AGENTS.md").is_file() {
            return current
                .canonicalize()
                .context("canonicalize repository root");
        }
        if !current.pop() {
            bail!("repository root not found; pass --root <path>");
        }
    }
}

pub fn relative(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

/// Runs (building first if needed) `tools/v26.8.1/src/bin/subsystem_verifier.rs`
/// against `root` and returns its independently-derived per-subsystem
/// standings. Shared by the manufacturing binary and the crown so both
/// consult the exact same external-verifier invocation.
pub fn run_subsystem_verifier(root: &Path) -> Result<Vec<SubsystemVerifierStanding>> {
    let manifest_path = root.join(SUBSYSTEM_MANIFEST_REL);
    if !manifest_path.is_file() {
        bail!(
            "SUBSYSTEM_MANIFEST_ABSENT: {} not found; run `python3 tools/v26.8.1/subsystem_evidence_manifest.py` first",
            relative(root, &manifest_path)
        );
    }
    let tool_root = root.join("tools/v26.8.1");
    let build = Command::new("cargo")
        .args([
            "build",
            "--manifest-path",
            "tools/v26.8.1/Cargo.toml",
            "--bin",
            "subsystem_verifier",
        ])
        .current_dir(root)
        .status()
        .context("spawn cargo build for subsystem_verifier")?;
    if !build.success() {
        bail!("SUBSYSTEM_VERIFIER_BUILD_FAILED: cargo build for subsystem_verifier did not exit 0");
    }
    let binary = tool_root.join("target/debug/subsystem_verifier");
    if !binary.is_file() {
        bail!(
            "SUBSYSTEM_VERIFIER_BINARY_ABSENT: expected {} after build",
            binary.display()
        );
    }
    let output = Command::new(&binary)
        .args(["--root", &root.to_string_lossy()])
        .output()
        .context("spawn subsystem_verifier")?;
    if !output.status.success() {
        // FAIL-CLOSED, not fail-open: a non-zero exit from subsystem_verifier
        // (e.g. WRONG_SOURCE_HEAD, SELF_CERTIFICATION_REFUSED,
        // SUBSYSTEM_MANIFEST_ABSENT) is a real refusal from the external
        // verifier, not an ignorable warning. Previously this branch only
        // printed to stderr and fell through to reading
        // subsystem-verifier-report.json from disk anyway -- which, on a
        // refusal, is a STALE report left over from the last successful run
        // (subsystem_verifier bails before writing a fresh one), so the
        // crown would silently admit against outdated evidence. This is
        // exactly the self-certification bypass the external-verifier
        // architecture exists to prevent.
        bail!(
            "SUBSYSTEM_VERIFIER_REFUSED: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    let report_path = root.join(SUBSYSTEM_VERIFIER_REPORT_REL);
    let report_bytes = fs::read(&report_path).with_context(|| {
        format!(
            "subsystem_verifier report missing at {}",
            relative(root, &report_path)
        )
    })?;
    let report: SubsystemVerifierReport =
        serde_json::from_slice(&report_bytes).context("parse subsystem-verifier-report.json")?;
    if report.schema_version != "ggen.v26.8.1.subsystem-verifier-report/1" {
        bail!(
            "SUBSYSTEM_VERIFIER_SCHEMA_MISMATCH: {}",
            report.schema_version
        );
    }
    Ok(report.subsystems)
}

/// BLAKE3 digest of the current `.ggen/v26.8.1/subsystem-verifier-report.json`
/// bytes on disk, used to bind projection receipts to the exact report they
/// were computed from.
pub fn subsystem_verifier_report_digest(root: &Path) -> Result<String> {
    let bytes = fs::read(root.join(SUBSYSTEM_VERIFIER_REPORT_REL))
        .context("read subsystem-verifier-report.json for digest")?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

/// BLAKE3 digest of the current `.ggen/v26.8.1/subsystem-evidence-manifest.json`
/// bytes on disk.
pub fn subsystem_manifest_digest(root: &Path) -> Result<String> {
    let bytes = fs::read(root.join(SUBSYSTEM_MANIFEST_REL))
        .context("read subsystem-evidence-manifest.json for digest")?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

/// Shape of `.ggen/v26.8.1/coverage-projection-receipt.json`, only the
/// fields the crown needs to cross-check provenance.
#[derive(Debug, Deserialize)]
pub struct CoverageProjectionReceipt {
    pub subsystem_verifier_report_digest: String,
    pub coverage_csv_blake3: String,
}

pub const COVERAGE_PROJECTION_RECEIPT_REL: &str = ".ggen/v26.8.1/coverage-projection-receipt.json";

/// Cross-checks the manufacturing step's own receipt
/// (`.ggen/v26.8.1/coverage-projection-receipt.json`) against the CURRENT,
/// freshly re-derived state: does the receipt's claimed
/// `subsystem_verifier_report_digest` match the digest of the
/// subsystem-verifier report this crown run just consulted? Does its
/// claimed `coverage_csv_blake3` match the CSV currently on disk?
///
/// Returns `Ok(None)` when the receipt is absent (nothing to cross-check
/// yet -- not itself a drift condition, e.g. before manufacturing has ever
/// run) or when everything matches. Returns `Ok(Some(reason))` describing
/// the specific mismatch when the receipt's claimed inputs are stale
/// relative to what is on disk right now -- e.g. a stale/mismatched
/// subsystem-verifier receipt used as the projection's claimed input.
pub fn check_provenance_receipt(
    root: &Path, current_subsystem_report_digest: &str, current_coverage_csv_bytes: &[u8],
) -> Result<Option<String>> {
    let receipt_path = root.join(COVERAGE_PROJECTION_RECEIPT_REL);
    if !receipt_path.is_file() {
        return Ok(None);
    }
    let bytes = fs::read(&receipt_path)
        .with_context(|| format!("read {}", COVERAGE_PROJECTION_RECEIPT_REL))?;
    let receipt: CoverageProjectionReceipt =
        serde_json::from_slice(&bytes).context("parse coverage-projection-receipt.json")?;
    let current_csv_digest = blake3::hash(current_coverage_csv_bytes)
        .to_hex()
        .to_string();

    if receipt.subsystem_verifier_report_digest != current_subsystem_report_digest {
        return Ok(Some(format!(
            "coverage-projection-receipt.json claims subsystem_verifier_report_digest={} but \
             the current subsystem-verifier-report.json digest is {} -- the receipt's claimed \
             input is stale/mismatched relative to the report actually on disk",
            receipt.subsystem_verifier_report_digest, current_subsystem_report_digest
        )));
    }
    if receipt.coverage_csv_blake3 != current_csv_digest {
        return Ok(Some(format!(
            "coverage-projection-receipt.json claims coverage_csv_blake3={} but the current \
             docs/v26.8.1/coverage-matrix.csv digest is {} -- the receipt no longer describes \
             the file actually on disk",
            receipt.coverage_csv_blake3, current_csv_digest
        )));
    }
    Ok(None)
}

pub fn exact_head(root: &Path) -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(root)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .unwrap_or_else(|| "UNKNOWN".into())
}
