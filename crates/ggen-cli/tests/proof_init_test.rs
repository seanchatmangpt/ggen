#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! GALL — `ggen init` is the boundary preparation pier (the candidate-O* foundation).
//!
//! Command-role model: `init`'s role is to PREPARE an O* candidate boundary that
//! `ggen sync` can later actuate (see `gall_sync_actuation.rs` for the actuator pier).
//! Where `sync` is proven by "artifact + receipt", `init` is proven by the DURABLE
//! SCAFFOLD it lays down: real files on disk that constitute a parseable boundary.
//!
//! **Chicago TDD** (no mocks, no test doubles):
//! - REAL CLI process execution via `assert_cmd::Command::cargo_bin("ggen")`
//!   (`init` is a KEPT, NON-feature-gated noun in the default binary — see
//!   `crates/ggen-cli/src/cmds/mod.rs:32`).
//! - REAL filesystem state in a `TempDir`.
//! - State-based verification of OBSERVABLE durable state: the scaffold files that
//!   actually exist on disk after the command, not stdout alone. A command that
//!   exits 0 and prints success while leaving the world unchanged is "decorative
//!   completion" and is a DEFECT, not a passing test.
//!
//! ## Source map (read before editing)
//! - Verb: `crates/ggen-cli/src/cmds/init.rs::init` (`#[verb("init","root")]`).
//!   Params bind snake_case → `--long` flags; bool flags take a value (`--force true`).
//!   Params: path, force, skip_hooks, name, version, description (all Option).
//! - Scaffold (perform_init, init.rs:482-799) creates exactly these files:
//!     ggen.toml, schema/domain.ttl, Makefile, templates/example.txt.tera,
//!     scripts/startup.sh, .gitignore, README.md
//!   and these dirs: schema/, templates/, scripts/.
//!
//! ## OBSERVED real-binary behavior (probed before writing this test — assert ONLY this)
//! - Fresh init: exit 0, JSON status="success", lays the 7 files + 3 dirs above.
//! - No-clobber: re-running WITHOUT --force returns JSON status="error"
//!   ("already initialized") but STILL EXITS 0 (init.rs:502-518 returns
//!   Ok(InitOutput{status:"error"}) — the refusal is in the payload, not the exit
//!   code). It does NOT overwrite (no `transaction` field is emitted).
//! - Force: `--force true` overwrites the 5 ggen-owned files but PRESERVES a
//!   pre-existing user README.md / .gitignore byte-for-byte (init.rs:661-704).
//!
//! `--skip_hooks true` is passed throughout so the test never depends on git being
//! present or mutating a real repo's hooks.

use assert_cmd::Command;
use predicates::prelude::*;
use serde_json::Value;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

/// Real `ggen` binary command.
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// The exact scaffold files `perform_init` lays down on a fresh boundary.
const SCAFFOLD_FILES: &[&str] = &[
    "ggen.toml",
    "schema/domain.ttl",
    "Makefile",
    "templates/example.txt.tera",
    "scripts/startup.sh",
    ".gitignore",
    "README.md",
];

/// The directories `perform_init` creates.
const SCAFFOLD_DIRS: &[&str] = &["schema", "templates", "scripts"];

/// Parse the JSON document init prints on stdout.
///
/// init emits one JSON object on stdout, but the process may also print
/// telemetry/log lines around it (e.g. "OpenTelemetry initialized ..." before,
/// and a span-export ERROR line — which itself contains `{ ... }` — after). So we
/// cannot naively slice first-`{` .. last-`}`. We also must tolerate the JSON being
/// either compact (one line, `serde_json::to_string`) OR pretty-printed across
/// several lines (`to_string_pretty`) depending on the clap-noun-verb version.
///
/// Strategy: for each line that opens an object (`{`), accumulate following lines
/// until the brace depth returns to zero (a complete object — compact or pretty),
/// parse it, and return the first such object carrying a top-level `status` field.
fn stdout_json(bytes: &[u8]) -> Value {
    let s = String::from_utf8_lossy(bytes);
    let lines: Vec<&str> = s.lines().collect();
    for start in 0..lines.len() {
        if !lines[start].trim_start().starts_with('{') {
            continue;
        }
        let mut depth: i32 = 0;
        let mut buf = String::new();
        for line in &lines[start..] {
            buf.push_str(line);
            buf.push('\n');
            // init's JSON values contain no braces, so a plain count is sound here.
            depth += line.matches('{').count() as i32;
            depth -= line.matches('}').count() as i32;
            if depth <= 0 {
                break;
            }
        }
        if let Ok(v) = serde_json::from_str::<Value>(buf.trim()) {
            // The InitOutput object always carries a "status" field.
            if v.get("status").is_some() {
                return v;
            }
        }
    }
    panic!("init stdout had no JSON object carrying a \"status\" field; got: {s}")
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier 1: a fresh `ggen init` PREPARES a real, durable O* candidate boundary.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_prepares_real_durable_scaffold_on_disk() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let assert = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip_hooks", "true"])
        .assert()
        .success(); // fresh init succeeds at the process level

    // The JSON payload reports success and accounts for what it created.
    let json = stdout_json(&assert.get_output().stdout);
    assert_eq!(
        json["status"].as_str(),
        Some("success"),
        "fresh init must report status=success: {json}"
    );

    // PROOF of real, durable work: every scaffold file actually exists on disk.
    // (Not stdout alone — the boundary must be materialized for `ggen sync` to read.)
    for f in SCAFFOLD_FILES {
        assert!(
            root.join(f).is_file(),
            "fresh init must create durable scaffold file {f} on disk"
        );
    }
    for d in SCAFFOLD_DIRS {
        assert!(
            root.join(d).is_dir(),
            "fresh init must create scaffold directory {d}/ on disk"
        );
    }

    // The manifest is the candidate boundary `ggen sync` will read — it must carry
    // the real seeded content, not be an empty placeholder.
    let manifest = fs::read_to_string(root.join("ggen.toml")).expect("read ggen.toml");
    assert!(
        manifest.contains("[project]") && manifest.contains("[ontology]"),
        "ggen.toml must be a real, parseable boundary with [project] and [ontology]: {manifest}"
    );

    // The seed ontology is present so the prepared boundary is non-trivial.
    let ttl = fs::read_to_string(root.join("schema/domain.ttl")).expect("read domain.ttl");
    assert!(
        ttl.contains("@prefix"),
        "schema/domain.ttl must contain a real RDF seed (@prefix ...): {ttl}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier 2: NO-CLOBBER. Re-running init on an already-prepared boundary REFUSES to
// re-scaffold — it does not silently overwrite the candidate O*.
//
// OBSERVED contract: the refusal is reported in the JSON payload (status="error",
// "already initialized") while the process still exits 0. We assert the REAL
// behavior: the refusal signal is present AND the scaffold is left untouched.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_refuses_to_clobber_existing_boundary_without_force() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // Prepare the boundary once.
    ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip_hooks", "true"])
        .assert()
        .success();

    // Mutate a ggen-owned file so we can prove it is NOT clobbered by the re-run.
    let manifest_path = root.join("ggen.toml");
    let sentinel = "# USER-EDITED-SENTINEL — must survive a no-force re-init\n";
    fs::write(&manifest_path, sentinel).expect("overwrite manifest with sentinel");

    // Re-run WITHOUT --force.
    let assert = ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip_hooks", "true"])
        .assert();
    let output = assert.get_output().clone();

    // The refusal must be LOUD in the payload (no fake "success").
    let json = stdout_json(&output.stdout);
    assert_eq!(
        json["status"].as_str(),
        Some("error"),
        "re-init without --force must report status=error (no-clobber): {json}"
    );
    let err = json["error"].as_str().unwrap_or("");
    assert!(
        err.contains("already initialized"),
        "re-init refusal must explain it is already initialized: {err}"
    );

    // PROOF of no-clobber: the user's sentinel content is byte-for-byte intact.
    let after = fs::read_to_string(&manifest_path).expect("read manifest after re-init");
    assert_eq!(
        after, sentinel,
        "no-force re-init must NOT overwrite the existing boundary"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier 3: --force re-scaffolds the ggen-owned boundary BUT PRESERVES the user's
// own files (README.md / .gitignore). This proves init re-prepares the O* surface
// without destroying author-owned content — the durable distinction init makes.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_force_rescaffolds_but_preserves_user_files() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // A real user already has their own README and .gitignore in the directory.
    let user_readme = "# My Real Project\n\nDo not clobber me.\n";
    let user_gitignore = "# my ignores\ntarget/\n*.log\n";
    fs::write(root.join("README.md"), user_readme).expect("write user README");
    fs::write(root.join(".gitignore"), user_gitignore).expect("write user .gitignore");

    // First init (these user files are preserved by init's preserve logic).
    ggen()
        .current_dir(root)
        .args(["init", "--path", ".", "--skip_hooks", "true"])
        .assert()
        .success();

    // Mutate a ggen-owned file, then force re-init: the ggen-owned file is replaced
    // with the canonical seed, while user files stay untouched.
    fs::write(root.join("ggen.toml"), "# stale\n").expect("mutate manifest");

    let assert = ggen()
        .current_dir(root)
        .args([
            "init",
            "--path",
            ".",
            "--skip_hooks",
            "true",
            "--force",
            "true",
        ])
        .assert()
        .success()
        .stdout(predicate::str::contains("\"status\":\"success\""));

    let json = stdout_json(&assert.get_output().stdout);

    // Force overwrote the ggen-owned manifest back to the canonical seed.
    let manifest = fs::read_to_string(root.join("ggen.toml")).expect("read manifest");
    assert!(
        manifest.contains("[project]") && manifest.contains("[ontology]"),
        "--force must restore the canonical ggen.toml seed, not leave the stale stub: {manifest}"
    );

    // PRESERVATION PROOF: user README + .gitignore are byte-for-byte intact.
    assert_eq!(
        fs::read_to_string(root.join("README.md")).expect("read README"),
        user_readme,
        "--force must PRESERVE the user's existing README.md"
    );
    assert_eq!(
        fs::read_to_string(root.join(".gitignore")).expect("read .gitignore"),
        user_gitignore,
        "--force must PRESERVE the user's existing .gitignore"
    );

    // The payload honestly reports the preservation it performed.
    let preserved: Vec<&str> = json["files_preserved"]
        .as_array()
        .map(|a| a.iter().filter_map(|v| v.as_str()).collect())
        .unwrap_or_default();
    assert!(
        preserved.contains(&"README.md") && preserved.contains(&".gitignore"),
        "force re-init must report README.md and .gitignore as preserved: {json}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Pier 4: init into a NESTED, not-yet-existing --path creates the boundary there
// (init.rs:521 create_dir_all). Proves the prepared boundary is addressable, and
// that nothing leaks into the parent working directory.
// ─────────────────────────────────────────────────────────────────────────────
#[test]
fn init_into_nonexistent_subdir_creates_boundary_there() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let nested = "fresh-project";

    assert!(
        !root.join(nested).exists(),
        "precondition: target subdir must not exist yet"
    );

    ggen()
        .current_dir(root)
        .args(["init", "--path", nested, "--skip_hooks", "true"])
        .assert()
        .success();

    // The scaffold lands inside the requested subdir.
    let proj = root.join(nested);
    for f in SCAFFOLD_FILES {
        assert!(
            proj.join(f).is_file(),
            "init --path {nested} must place {f} under the requested subdir"
        );
    }

    // And it does NOT leak ggen.toml into the parent working directory.
    assert!(
        !root.join("ggen.toml").exists(),
        "init --path <subdir> must not scaffold into the parent working dir"
    );
}

/// Sanity: keep `Path` import meaningful even if assertions above change shape.
#[allow(dead_code)]
fn _path_import_anchor(_p: &Path) {}
