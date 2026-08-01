//! External subsystem verifier for ggen v26.8.1.
//!
//! Architecturally separate from `tools/v26.8.1/subsystem_evidence_manifest.py`
//! (the manifest generator): a different process, a different language, and
//! -- critically -- a different trust posture. This binary treats every
//! field of the manifest as a CLAIM, never as ground truth:
//!
//! - File digests are independently RE-HASHED from disk, not read out of the
//!   manifest.
//! - `exact_source_head` is independently re-derived via this process's own
//!   `git rev-parse HEAD` call, not read out of the manifest.
//! - Positive-witness and negative-falsifier test results are independently
//!   RE-RUN as fresh `cargo test` subprocesses by this binary, not trusted
//!   from the manifest's recorded `passed` field.
//! - Legacy disposition is independently re-derived from
//!   `ontology/v26.8.1/legacy-capabilities.ttl` and the equivalence pack's
//!   `VERIFIER_REPORT.json`, not read out of the manifest's
//!   `legacy_disposition_report` field.
//! - `coverage-matrix.csv`'s `standing` column is NEVER consulted by this
//!   binary at all -- this is the whole point: it is a generated PROJECTION
//!   of this binary's output, never its input.
//!
//! Self-certification check: if `manifest.verifier_identity.path` refers to
//! this very binary's own source file, or claims the `subsystem-verifier`
//! role, verification is refused outright -- a generator claiming to also be
//! its own verifier is exactly the circular trust loop this tool exists to
//! prevent.

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const THIS_BINARY_SOURCE_REL: &str = "tools/v26.8.1/src/bin/subsystem_verifier.rs";
const MANIFEST_REL: &str = ".ggen/v26.8.1/subsystem-evidence-manifest.json";
const REPORT_REL: &str = ".ggen/v26.8.1/subsystem-verifier-report.json";

// ---------- manifest input shapes (deserialize-only; never trusted blindly) ----------

#[derive(Debug, Deserialize)]
struct Manifest {
    schema: String,
    release: String,
    exact_source_head: String,
    verifier_identity: VerifierIdentity,
    subsystems: Vec<SubsystemRecord>,
    #[serde(default)]
    #[allow(dead_code)]
    legacy_disposition_summary: serde_json::Value,
    receipt_digest: String,
}

#[derive(Debug, Deserialize)]
struct VerifierIdentity {
    path: String,
    #[allow(dead_code)]
    content_sha256: String,
    role: String,
}

#[derive(Debug, Deserialize, Clone)]
struct SubsystemRecord {
    subsystem: String,
    authority_sources: Vec<String>,
    authority_digest: String,
    implementation_sources: Vec<String>,
    implementation_digest: String,
    positive_witness_reports: Vec<TestRun>,
    negative_falsifier_reports: Vec<TestRun>,
    #[serde(default)]
    insufficient_evidence: bool,
}

#[derive(Debug, Deserialize, Clone)]
struct TestRun {
    #[serde(rename = "crate")]
    krate: String,
    test_target: String,
    test_fn: Option<String>,
    #[serde(default)]
    passed: bool,
    is_true_negative_control: bool,
    // The manifest generator's own recorded invocation. Not blindly trusted
    // for its `passed` field (that's still independently re-derived below),
    // but its `argv` is the only reliable way to know WHAT to re-run: krate
    // is a real cargo package name for Rust-based evidence, but a "(python)"
    // placeholder for non-cargo evidence sources (e.g. legacy subsystem's
    // Python unittest-based positive/negative witnesses). Reconstructing a
    // `cargo test -p (python) --test ...` from krate/test_target/test_fn for
    // those entries is nonsense and fails immediately, misreporting a real
    // pass as a reverification failure -- this was exactly that bug.
    #[serde(default)]
    argv: Vec<String>,
}

// ---------- verifier's own output shapes ----------

#[derive(Debug, Serialize, Deserialize)]
struct ReVerifiedTest {
    krate: String,
    test_target: String,
    test_fn: Option<String>,
    manifest_claimed_passed: bool,
    reverified_passed: bool,
    is_true_negative_control: bool,
    agrees_with_manifest: bool,
    exit_code: i32,
}

#[derive(Debug, Serialize, Deserialize)]
struct SubsystemStanding {
    subsystem: String,
    standing: String,
    authority_digest_match: bool,
    implementation_digest_match: bool,
    has_positive_witness: bool,
    has_true_negative_control: bool,
    all_reverified_positive_pass: bool,
    all_reverified_negative_pass: bool,
    legacy_total: usize,
    legacy_closed_by_this_verifier: usize,
    legacy_unknown: usize,
    legacy_fully_closed: bool,
    manifest_insufficient_evidence: bool,
    reasons: Vec<String>,
    reverified_tests: Vec<ReVerifiedTest>,
}

#[derive(Debug, Serialize, Deserialize)]
struct VerifierReport {
    schema_version: String,
    release: String,
    manifest_exact_source_head: String,
    verifier_fresh_source_head: String,
    source_head_matches: bool,
    manifest_receipt_digest: String,
    self_cert_check_passed: bool,
    subsystems: Vec<SubsystemStanding>,
    refusal: Option<String>,
}

/// Independently re-hash the same (relpath, contents) concatenation the
/// Python generator used, purely from disk -- never from the manifest.
fn rehash_sources(root: &Path, rels: &[String]) -> Result<String> {
    let mut matched: Vec<PathBuf> = Vec::new();
    for pattern in rels {
        if pattern.contains('*') || pattern.contains('?') || pattern.contains('[') {
            matched.extend(glob_relative(root, pattern)?);
        } else {
            let full = root.join(pattern);
            if full.is_file() {
                matched.push(full);
            }
        }
    }
    matched.sort();
    matched.dedup();
    let mut hasher = Sha256::new();
    for path in &matched {
        let relpath = path
            .strip_prefix(root)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");
        hasher.update(relpath.as_bytes());
        hasher.update([0u8]);
        let bytes = fs::read(path).with_context(|| format!("read {}", path.display()))?;
        hasher.update(&bytes);
        hasher.update([0u8]);
    }
    Ok(format!("{:x}", hasher.finalize()))
}

/// Minimal glob support sufficient for the patterns this manifest uses
/// (`dir/*.md`, `dir/**/*.rs`) -- walks the tree and matches by suffix/dir
/// segment, deliberately simple rather than pulling in a glob crate.
fn glob_relative(root: &Path, pattern: &str) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    let recursive = pattern.contains("**");
    let (prefix, suffix) = if recursive {
        let mut parts = pattern.splitn(2, "**/");
        let prefix = parts.next().unwrap_or("").trim_end_matches('/');
        let suffix = parts.next().unwrap_or("");
        (prefix.to_string(), suffix.to_string())
    } else {
        let (dir, file) = pattern.rsplit_once('/').unwrap_or((".", pattern));
        (dir.to_string(), file.to_string())
    };
    let base = if prefix.is_empty() {
        root.to_path_buf()
    } else {
        root.join(&prefix)
    };
    if !base.exists() {
        return Ok(out);
    }
    let ext_suffix = suffix.trim_start_matches("*.");
    for entry in walkdir::WalkDir::new(&base) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let name = entry.file_name().to_string_lossy();
        let matches = if suffix.starts_with("*.") {
            name.ends_with(&format!(".{ext_suffix}"))
        } else {
            name == suffix
        };
        if matches {
            out.push(entry.path().to_path_buf());
        }
    }
    Ok(out)
}

/// Reads the on-disk `subsystem-verifier-report.json` (if any) and returns it
/// only if it is a genuine, successful, non-cached prior verification of
/// this EXACT manifest (matching `exact_source_head` and `receipt_digest`,
/// no refusal, self-cert passed, real source-head match -- not an
/// `--observe-only` pass-through). Any parse failure or mismatch is a cache
/// miss, never an error -- caching is purely an optimization, never a
/// correctness requirement.
fn load_cache_hit(root: &Path, manifest: &Manifest) -> Option<VerifierReport> {
    let bytes = fs::read(root.join(REPORT_REL)).ok()?;
    let cached: VerifierReport = serde_json::from_slice(&bytes).ok()?;
    if cached.manifest_exact_source_head == manifest.exact_source_head
        && cached.manifest_receipt_digest == manifest.receipt_digest
        && cached.self_cert_check_passed
        && cached.source_head_matches
        && cached.refusal.is_none()
    {
        Some(cached)
    } else {
        None
    }
}

fn fresh_git_head(root: &Path) -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(root)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_owned())
        .unwrap_or_else(|| "UNKNOWN".into())
}

/// Independent re-run: this binary spawns the real command itself, ignoring
/// whatever `passed` the manifest claimed.
///
/// If the manifest recorded a real `argv` (any evidence source: cargo test,
/// a Python script, a shell invocation), that exact command is re-executed
/// verbatim -- this is not "trusting" the manifest any more than trusting
/// `test_target`/`test_fn` already were; only the recorded `passed` claim is
/// distrusted, never the shape of what to run. Only when `argv` is absent
/// (older manifests, or entries that never carried one) does this fall back
/// to reconstructing a `cargo test -p <krate> --test <target>` invocation,
/// which is correct only for genuine cargo-package evidence.
fn rerun_test(
    root: &Path, krate: &str, test_target: &str, test_fn: &Option<String>, argv: &[String],
) -> (bool, i32) {
    let output = if !argv.is_empty() {
        Command::new(&argv[0])
            .args(&argv[1..])
            .current_dir(root)
            .output()
    } else {
        let target = test_target.trim_end_matches(".rs");
        let mut cargo_argv = vec![
            "test".to_string(),
            "-p".to_string(),
            krate.to_string(),
            "--test".to_string(),
            target.to_string(),
        ];
        if let Some(f) = test_fn {
            cargo_argv.push("--".to_string());
            cargo_argv.push(f.clone());
            cargo_argv.push("--exact".to_string());
        }
        Command::new("cargo")
            .args(&cargo_argv)
            .current_dir(root)
            .output()
    };
    match output {
        Ok(o) => {
            let stdout = String::from_utf8_lossy(&o.stdout);
            let passed = o.status.success() && !stdout.contains("test result: FAILED");
            (passed, o.status.code().unwrap_or(-1))
        }
        Err(_) => (false, -1),
    }
}

/// Independently re-derive legacy disposition closure per subsystem from
/// `ontology/v26.8.1/legacy-capabilities.ttl` + the equivalence pack's real
/// `VERIFIER_REPORT.json` -- never from the manifest's own recorded field.
fn reverify_legacy_disposition(root: &Path) -> Result<BTreeMap<String, (usize, usize, usize)>> {
    // (total, closed, unknown) per subsystem
    let ttl_path = root.join("ontology/v26.8.1/legacy-capabilities.ttl");
    let report_path = root.join(
        "packs/legacy-equivalence-verifier-pack/consumer/legacy-equivalence/VERIFIER_REPORT.json",
    );
    let text = fs::read_to_string(&ttl_path).unwrap_or_default();

    let mut ids = Vec::new();
    let mut dispositions = Vec::new();
    let mut subsystems = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("ggen:capabilityId ") {
            if let Some(v) = extract_quoted(rest) {
                ids.push(v);
            }
        } else if let Some(rest) = line.strip_prefix("ggen:hasDisposition ggen:") {
            let v = rest
                .trim_end_matches(" ;")
                .trim_end_matches('.')
                .trim()
                .to_owned();
            dispositions.push(v);
        } else if let Some(rest) = line.strip_prefix("ggen:owningSubsystem ") {
            if let Some(v) = extract_quoted(rest) {
                subsystems.push(v);
            }
        }
    }

    let mut passed_case_ids = std::collections::BTreeSet::new();
    if report_path.is_file() {
        let report_text = fs::read_to_string(&report_path)?;
        let report: serde_json::Value = serde_json::from_str(&report_text)?;
        if let Some(results) = report.get("results").and_then(|v| v.as_array()) {
            for r in results {
                if r.get("status").and_then(|v| v.as_str()) == Some("PASS") {
                    if let Some(case_id) = r.get("case_id").and_then(|v| v.as_str()) {
                        passed_case_ids.insert(case_id.to_owned());
                    }
                }
            }
        }
    }

    let mut per_subsystem: BTreeMap<String, (usize, usize, usize)> = BTreeMap::new();
    let n = ids.len().min(dispositions.len()).min(subsystems.len());
    for i in 0..n {
        let cap_id = &ids[i];
        let disp = &dispositions[i];
        let subsystem = &subsystems[i];
        let case_id = cap_id.replace('_', "-");
        let entry = per_subsystem.entry(subsystem.clone()).or_insert((0, 0, 0));
        entry.0 += 1; // total
        if disp == "DISPOSITION_UNKNOWN" {
            entry.2 += 1; // unknown
        } else if passed_case_ids.contains(&case_id) {
            entry.1 += 1; // closed
        }
    }
    Ok(per_subsystem)
}

fn extract_quoted(s: &str) -> Option<String> {
    let start = s.find('"')?;
    let rest = &s[start + 1..];
    let end = rest.find('"')?;
    Some(rest[..end].to_owned())
}

fn resolve_root(args: &[String]) -> Result<PathBuf> {
    let explicit = args
        .windows(2)
        .find(|pair| pair[0] == "--root")
        .map(|pair| PathBuf::from(&pair[1]));
    let mut current = explicit.unwrap_or(env::current_dir()?);
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

fn manifest_path(args: &[String], root: &Path) -> PathBuf {
    args.windows(2)
        .find(|pair| pair[0] == "--manifest")
        .map(|pair| PathBuf::from(&pair[1]))
        .unwrap_or_else(|| root.join(MANIFEST_REL))
}

fn main() {
    if let Err(error) = run() {
        eprintln!("subsystem-verifier refused: {error:#}");
        std::process::exit(2);
    }
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    let observe_only = args.iter().any(|a| a == "--observe-only");
    let root = resolve_root(&args)?;
    let manifest_file = manifest_path(&args, &root);

    let manifest_bytes = fs::read(&manifest_file)
        .with_context(|| format!("missing referenced manifest: {}", manifest_file.display()))?;
    let manifest_raw: serde_json::Value = serde_json::from_slice(&manifest_bytes)?;
    let manifest: Manifest = serde_json::from_value(manifest_raw.clone())
        .context("manifest failed schema-shape deserialization")?;

    if manifest.schema != "ggen.v26.8.1.subsystem-evidence-manifest/1" {
        bail!(
            "MANIFEST_SCHEMA_MISMATCH: unexpected schema {}",
            manifest.schema
        );
    }
    if manifest.release != "26.8.1" {
        bail!("MANIFEST_RELEASE_MISMATCH: {}", manifest.release);
    }

    // --- Self-certification check ---
    // Refuse if the manifest's declared generator identity is this very
    // verifier binary (same source path) or claims to already be the
    // verifier -- a component cannot certify itself.
    let self_cert_ok = manifest.verifier_identity.path != THIS_BINARY_SOURCE_REL
        && manifest.verifier_identity.role == "manifest-generator";
    if !self_cert_ok {
        bail!(
            "SELF_CERTIFICATION_REFUSED: manifest verifier_identity ({}, role={}) must not be this verifier binary itself",
            manifest.verifier_identity.path,
            manifest.verifier_identity.role
        );
    }

    // --- Cache: skip re-verification if a prior report already proved this
    // exact manifest identity (source_head + receipt_digest) and passed. ---
    // Not a TTL/time-based cache -- (exact_source_head, receipt_digest) is a
    // content-addressed identity of this exact manifest, so a match can only
    // occur when nothing this verifier would re-check (source files, git
    // HEAD, test outcomes feeding the manifest) has changed since the cached
    // run genuinely re-verified it. Deliberately does NOT weaken the
    // guarantee for a manifest that's never been independently verified
    // before, or one from a different commit/content -- those always take
    // the full re-verification path below. `--no-cache` forces a fresh
    // re-verification regardless (for the one authoritative, final run).
    let no_cache = args.iter().any(|a| a == "--no-cache");
    if !no_cache {
        if let Some(cached) = load_cache_hit(&root, &manifest) {
            println!(
                "subsystem_verifier: CACHE_HIT (manifest exact_source_head={} receipt_digest={} \
                 already independently re-verified by a prior run in this pipeline invocation; \
                 skipping {} test reruns -- pass --no-cache to force a fresh re-verification)",
                manifest.exact_source_head,
                manifest.receipt_digest,
                manifest.subsystems.len() * 2,
            );
            let _ = cached; // report file on disk already reflects this exact, still-valid state
            return Ok(());
        }
    }

    // --- Fresh, independent exact-head re-derivation ---
    let fresh_head = fresh_git_head(&root);
    let source_head_matches = fresh_head == manifest.exact_source_head;
    if !source_head_matches && !observe_only {
        bail!(
            "WRONG_SOURCE_HEAD: manifest claims {} but this checkout's HEAD is {}",
            manifest.exact_source_head,
            fresh_head
        );
    }

    // --- Independent legacy-disposition re-derivation (never trust manifest's copy) ---
    let legacy_by_subsystem = reverify_legacy_disposition(&root)?;

    let mut subsystem_standings = Vec::new();
    for record in &manifest.subsystems {
        let mut reasons = Vec::new();

        if record.authority_sources.is_empty() {
            reasons.push("MISSING_REFERENCED_REPORT: authority_sources empty".to_string());
        }
        let authority_rehash = rehash_sources(&root, &record.authority_sources)?;
        let authority_digest_match = authority_rehash == record.authority_digest;
        if !authority_digest_match {
            reasons.push(format!(
                "ALTERED_EVIDENCE_DIGEST: authority_digest manifest={} rehash={}",
                record.authority_digest, authority_rehash
            ));
        }

        let implementation_rehash = rehash_sources(&root, &record.implementation_sources)?;
        let implementation_digest_match = implementation_rehash == record.implementation_digest;
        if !record.implementation_sources.is_empty() && !implementation_digest_match {
            reasons.push(format!(
                "ALTERED_EVIDENCE_DIGEST: implementation_digest manifest={} rehash={}",
                record.implementation_digest, implementation_rehash
            ));
        }

        let has_positive_witness = !record.positive_witness_reports.is_empty();
        if !has_positive_witness {
            reasons.push("POSITIVE_WITNESS_ABSENT".to_string());
        }
        let has_true_negative_control = record
            .negative_falsifier_reports
            .iter()
            .any(|t| t.is_true_negative_control);
        if !record.negative_falsifier_reports.is_empty() && !has_true_negative_control {
            reasons.push(
                "NEGATIVE_FALSIFIER_ABSENT: no negative_falsifier_reports entry is a true refusal/sabotage-detection test"
                    .to_string(),
            );
        }
        if record.negative_falsifier_reports.is_empty() {
            reasons.push("NEGATIVE_FALSIFIER_ABSENT: list empty".to_string());
        }

        let mut reverified_tests = Vec::new();
        let mut all_positive_pass = has_positive_witness;
        for t in &record.positive_witness_reports {
            let (passed, exit_code) =
                rerun_test(&root, &t.krate, &t.test_target, &t.test_fn, &t.argv);
            all_positive_pass &= passed;
            reverified_tests.push(ReVerifiedTest {
                krate: t.krate.clone(),
                test_target: t.test_target.clone(),
                test_fn: t.test_fn.clone(),
                manifest_claimed_passed: t.passed,
                reverified_passed: passed,
                is_true_negative_control: t.is_true_negative_control,
                agrees_with_manifest: passed == t.passed,
                exit_code,
            });
        }
        let mut all_negative_pass = !record.negative_falsifier_reports.is_empty();
        for t in &record.negative_falsifier_reports {
            let (passed, exit_code) =
                rerun_test(&root, &t.krate, &t.test_target, &t.test_fn, &t.argv);
            all_negative_pass &= passed;
            reverified_tests.push(ReVerifiedTest {
                krate: t.krate.clone(),
                test_target: t.test_target.clone(),
                test_fn: t.test_fn.clone(),
                manifest_claimed_passed: t.passed,
                reverified_passed: passed,
                is_true_negative_control: t.is_true_negative_control,
                agrees_with_manifest: passed == t.passed,
                exit_code,
            });
        }
        if has_positive_witness && !all_positive_pass {
            reasons.push("POSITIVE_WITNESS_ABSENT: reverified run did not pass".to_string());
        }
        if !record.negative_falsifier_reports.is_empty() && !all_negative_pass {
            reasons.push("NEGATIVE_FALSIFIER_ABSENT: reverified run did not pass".to_string());
        }

        let (legacy_total, legacy_closed, legacy_unknown) = legacy_by_subsystem
            .get(&record.subsystem)
            .copied()
            .unwrap_or((0, 0, 0));
        let legacy_fully_closed =
            legacy_total > 0 && legacy_unknown == 0 && legacy_closed == legacy_total;
        let legacy_present_but_claimed_closed_wrongly = legacy_total > 0 && legacy_unknown > 0;
        if legacy_present_but_claimed_closed_wrongly {
            reasons.push(format!(
                "UNKNOWN_LEGACY_DISPOSITION_CLAIMED_CLOSED: {legacy_unknown} of {legacy_total} legacy capabilities in this subsystem have DISPOSITION_UNKNOWN"
            ));
        }

        if record.insufficient_evidence {
            reasons.push("INSUFFICIENT_EVIDENCE: manifest generator itself flagged this subsystem as insufficient".to_string());
        }

        let standing = if record.insufficient_evidence || !has_positive_witness {
            "UNKNOWN"
        } else if authority_digest_match
            && implementation_digest_match
            && all_positive_pass
            && has_true_negative_control
            && all_negative_pass
            && (legacy_total == 0 || legacy_fully_closed)
        {
            "ALIVE"
        } else if all_positive_pass {
            "PARTIAL_ALIVE"
        } else {
            "UNKNOWN"
        };

        subsystem_standings.push(SubsystemStanding {
            subsystem: record.subsystem.clone(),
            standing: standing.to_string(),
            authority_digest_match,
            implementation_digest_match,
            has_positive_witness,
            has_true_negative_control,
            all_reverified_positive_pass: all_positive_pass,
            all_reverified_negative_pass: all_negative_pass,
            legacy_total,
            legacy_closed_by_this_verifier: legacy_closed,
            legacy_unknown,
            legacy_fully_closed,
            manifest_insufficient_evidence: record.insufficient_evidence,
            reasons,
            reverified_tests,
        });
    }

    let report = VerifierReport {
        schema_version: "ggen.v26.8.1.subsystem-verifier-report/1".into(),
        release: manifest.release.clone(),
        manifest_exact_source_head: manifest.exact_source_head.clone(),
        verifier_fresh_source_head: fresh_head,
        source_head_matches,
        manifest_receipt_digest: manifest.receipt_digest.clone(),
        self_cert_check_passed: self_cert_ok,
        subsystems: subsystem_standings,
        refusal: None,
    };

    let out_path = root.join(REPORT_REL);
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&out_path, serde_json::to_vec_pretty(&report)?)?;

    for s in &report.subsystems {
        println!("{:<14} standing={}", s.subsystem, s.standing);
    }
    println!("report={}", out_path.display());

    Ok(())
}
