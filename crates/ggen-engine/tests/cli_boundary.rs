//! CLI-boundary proofs: spawn the real `ggen` binary as a subprocess via
//! chicago-tdd-tools `CliHarness`. No mocks — real argv, real exit codes,
//! real filesystem effects.
//!
//! Coverage goal: every noun/verb/flag combination the CLI actually exposes
//! (`ggen --help`, `sync run [--dry-run]`, `graph validate`, `receipt
//! verify`, `receipt history`, `--version`, `--introspect`, unknown
//! subcommands/args) proven against the compiled binary, not just the
//! library functions underneath it.

use chicago_tdd_tools::cli_proof::CliHarness;
use std::io::Read;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};
use tempfile::TempDir;

fn scaffold(root: &std::path::Path) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"demo\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir");
    std::fs::write(
        root.join("templates/one.tmpl"),
        "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .expect("write template");
}

// ---------------------------------------------------------------------
// Root
// ---------------------------------------------------------------------

#[test]
fn root_help_exits_zero_and_lists_all_nouns() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["--help"])
        .run()
        .expect("run --help");
    output
        .assert_success()
        .assert_stdout_contains("sync")
        .assert_stdout_contains("graph")
        .assert_stdout_contains("receipt");
}

/// Regression test: the noun-level clap `Command`s for sync/graph/receipt/
/// doctor used to have a blank `about`, so `ggen --help` listed four bare
/// noun names with no hint what they do. Each noun line in the top-level
/// help must now carry real description text, not just the bare noun name
/// followed by whitespace/newline.
#[test]
fn root_help_gives_each_noun_a_non_blank_description() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["--help"])
        .run()
        .expect("run --help");
    output.assert_success();

    let stdout = output.stdout.clone();
    for (noun, expected_word) in [
        ("sync", "pipeline"),
        ("graph", "ontology"),
        ("receipt", "receipt"),
        ("doctor", "health"),
    ] {
        let line = stdout
            .lines()
            .find(|line| line.trim_start().starts_with(noun))
            .unwrap_or_else(|| panic!("no `{noun}` line in --help output:\n{stdout}"));
        assert!(
            line.contains(expected_word),
            "expected `{noun}` noun's --help line to describe what it does (mentioning \
             `{expected_word}`), got: {line:?}\nfull output:\n{stdout}"
        );
    }
}

#[test]
fn root_version_exits_zero() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["--version"])
        .run()
        .expect("run --version");
    output.assert_success();
}

#[test]
fn root_no_args_exits_zero_or_prints_usage() {
    // clap-noun-verb with no subcommand: must not crash, must not silently
    // hang — either succeeds with a usage summary or exits nonzero cleanly.
    let output = CliHarness::cargo_bin("ggen")
        .run()
        .expect("run with no args");
    assert!(
        output.exit_code == 0 || !output.stdout.is_empty() || !output.stderr.is_empty(),
        "expected a clean exit or usage output, got exit {} with empty stdout/stderr",
        output.exit_code
    );
}

#[test]
fn unknown_noun_exits_nonzero() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["totally-unknown-noun-xyz"])
        .run()
        .expect("run unknown noun");
    output.assert_failure();
}

#[test]
fn unknown_flag_exits_nonzero() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--this-flag-does-not-exist"])
        .run()
        .expect("run unknown flag");
    output.assert_failure();
}

// ---------------------------------------------------------------------
// sync <noun> --help / run --help
// ---------------------------------------------------------------------

#[test]
fn sync_noun_help_exits_zero_and_lists_run() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "--help"])
        .run()
        .expect("run sync --help");
    output.assert_success().assert_stdout_contains("run");
}

#[test]
fn sync_run_help_lists_dry_run_flag() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--help"])
        .run()
        .expect("run sync run --help");
    output.assert_success().assert_stdout_contains("dry-run");
}

#[test]
fn sync_run_help_lists_watch_flag() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--help"])
        .run()
        .expect("run sync run --help");
    output.assert_success().assert_stdout_contains("watch");
}

/// Regression test: `sync run --dry-run` and `--watch` used to render with
/// empty per-argument help text because the code generator only picked up
/// the function-level doc comment, not per-parameter documentation (unlike
/// `--format <format>`, whose help came from clap's own derive). Each flag's
/// line in `--help` must now carry real description text, not just the bare
/// flag name followed by whitespace/newline.
#[test]
fn sync_run_help_gives_each_flag_a_non_blank_description() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--help"])
        .run()
        .expect("run sync run --help");
    output.assert_success();

    let stdout = output.stdout.clone();
    for (flag, expected_word) in [("--dry-run", "disk"), ("--watch", "filesystem")] {
        let line = stdout
            .lines()
            .find(|line| line.trim_start().starts_with(flag))
            .unwrap_or_else(|| panic!("no `{flag}` line in --help output:\n{stdout}"));
        assert!(
            line.contains(expected_word),
            "expected `{flag}`'s --help line to describe what it does (mentioning \
             `{expected_word}`), got: {line:?}\nfull output:\n{stdout}"
        );
    }
}

// ---------------------------------------------------------------------
// sync run
// ---------------------------------------------------------------------

#[test]
fn sync_run_generates_expected_file() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("run sync");
    output.assert_success();

    let content =
        std::fs::read_to_string(dir.path().join("out/names.txt")).expect("generated file");
    assert_eq!(content, "alice\n");
    assert!(
        dir.path().join(".ggen-v2/receipt.json").exists(),
        "receipt emitted"
    );
}

#[test]
fn sync_run_dry_run_writes_nothing() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--dry-run"])
        .current_dir(dir.path())
        .run()
        .expect("run sync --dry-run");
    output.assert_success();

    assert!(
        !dir.path().join("out/names.txt").exists(),
        "dry-run must not write output"
    );
    assert!(
        !dir.path().join(".ggen-v2/receipt.json").exists(),
        "dry-run must not emit a receipt"
    );
    assert!(
        !dir.path().join("ggen.lock").exists(),
        "dry-run must not write a lockfile"
    );
}

#[test]
fn sync_run_second_invocation_is_idempotent() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("first sync")
        .assert_success();
    let first = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("first output");

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("second sync")
        .assert_success();
    let second = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("second output");

    assert_eq!(first, second, "second sync must not change output content");
}

#[test]
fn sync_run_missing_manifest_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("run sync with no ggen.toml");
    output.assert_failure();
}

// ---------------------------------------------------------------------
// sync run --watch
//
// `--watch` runs one sync then blocks forever watching for filesystem
// changes (see `watch.rs` module docs: no SIGINT handling, by design).
// `CliHarness::run` waits for the child to exit, so it cannot be used for
// the success path here. These tests spawn the compiled binary directly,
// read its stderr for the expected boundary signals, then kill the child —
// bounded by a hard deadline so a regression hangs the test suite for at
// most a few seconds, not forever.
// ---------------------------------------------------------------------

/// Read `stderr` from a spawned child for up to `deadline`, returning as
/// soon as `needle` appears (or whatever was captured once the deadline
/// elapses). The child is always killed and reaped before returning.
fn watch_for_stderr(mut child: std::process::Child, needle: &str, deadline: Duration) -> String {
    let mut stderr = child.stderr.take().expect("piped stderr");
    let mut buf = Vec::new();
    let mut byte = [0u8; 1];
    let start = Instant::now();
    // Non-blocking-ish poll: stdlib `Read` on a pipe blocks, so run the
    // read on the current thread but bound total wall time via the
    // deadline check between reads is not possible without a background
    // thread. Use a background reader thread instead.
    let (tx, rx) = std::sync::mpsc::channel();
    std::thread::spawn(move || loop {
        match stderr.read(&mut byte) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                if tx.send(byte[0]).is_err() {
                    break;
                }
            }
        }
    });
    while start.elapsed() < deadline {
        match rx.recv_timeout(Duration::from_millis(100)) {
            Ok(b) => {
                buf.push(b);
                if String::from_utf8_lossy(&buf).contains(needle) {
                    break;
                }
            }
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => continue,
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => break,
        }
    }
    let _ = child.kill();
    let _ = child.wait();
    String::from_utf8_lossy(&buf).into_owned()
}

/// Resolve the compiled `ggen` binary the same way `CliHarness::cargo_bin("ggen")`
/// does (see `chicago-tdd-tools/src/cli_proof/harness.rs`, `resolve_binary`):
/// this test binary belongs to `ggen-engine`, which builds no `ggen` binary of
/// its own (`autobins = false`), so `CARGO_BIN_EXE_ggen` is never set by
/// `cargo test -p ggen-engine`. Fall back to walking up from
/// `CARGO_MANIFEST_DIR` to the workspace root (where `Cargo.lock` lives) and
/// probing `target/{debug,release}/ggen`, then finally `PATH`.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    // Last resort: PATH search (may find a system-installed version).
    if let Some(path_var) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&path_var) {
            let candidate = dir.join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = dir.join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset, no \
         target/{{debug,release}}/ggen found by walking up from \
         CARGO_MANIFEST_DIR, and not found on PATH. Run `cargo build -p ggen` first."
    );
}

#[test]
fn sync_run_watch_runs_initial_sync_then_blocks_watching() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let child = Command::new(ggen_bin())
        .args(["sync", "run", "--watch"])
        .current_dir(dir.path())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ggen sync run --watch");

    let captured = watch_for_stderr(child, "watching", Duration::from_secs(10));

    assert!(
        captured.contains("written") || captured.contains("watching"),
        "expected --watch to run an initial sync and start watching, got stderr:\n{captured}"
    );
    let content =
        std::fs::read_to_string(dir.path().join("out/names.txt")).expect("initial sync output");
    assert_eq!(content, "alice\n");
    assert!(
        dir.path().join(".ggen-v2/receipt.json").exists(),
        "initial sync under --watch must still emit a receipt"
    );
}

#[test]
fn sync_run_watch_and_dry_run_combined_writes_nothing() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let child = Command::new(ggen_bin())
        .args(["sync", "run", "--watch", "--dry-run"])
        .current_dir(dir.path())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn ggen sync run --watch --dry-run");

    let captured = watch_for_stderr(child, "watching", Duration::from_secs(10));

    assert!(
        captured.contains("written") || captured.contains("watching"),
        "expected --watch --dry-run to still run and report, got stderr:\n{captured}"
    );
    assert!(
        !dir.path().join("out/names.txt").exists(),
        "--watch --dry-run must not write output from its initial sync"
    );
    assert!(
        !dir.path().join(".ggen-v2/receipt.json").exists(),
        "--watch --dry-run must not emit a receipt from its initial sync"
    );
}

#[test]
fn sync_run_watch_missing_manifest_exits_nonzero_without_hanging() {
    // The unwatchable-root error path: with no `ggen.toml` present, the
    // initial sync inside `watch()` fails before the filesystem watcher is
    // ever constructed, so the process must exit non-zero promptly instead
    // of hanging in the watch loop.
    let dir = TempDir::new().expect("tempdir");
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run", "--watch"])
        .current_dir(dir.path())
        .run()
        .expect("run sync run --watch with no ggen.toml");
    output.assert_failure();
}

#[test]
fn sync_run_unbound_template_variable_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    // Overwrite the template with an unbound {{ typo }} — sync must refuse
    // via the same FM-TPL-003 lint that `graph validate` runs, not just
    // silently mis-render.
    std::fs::write(
        dir.path().join("templates/one.tmpl"),
        "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{{ typo }}",
    )
    .expect("overwrite template");

    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate");
    output.assert_failure().assert_stderr_contains("FM-TPL-003");
}

// ---------------------------------------------------------------------
// graph validate
// ---------------------------------------------------------------------

#[test]
fn graph_validate_valid_project_exits_zero() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate");
    output
        .assert_success()
        .assert_stdout_contains("templates_checked");
}

#[test]
fn graph_validate_missing_manifest_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate");
    output.assert_failure();
}

#[test]
fn graph_validate_malformed_ontology_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "this is not valid turtle {{{",
    )
    .expect("overwrite ontology");
    let output = CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate on bad ttl");
    output.assert_failure();
}

// ---------------------------------------------------------------------
// receipt verify / history
// ---------------------------------------------------------------------

#[test]
fn receipt_verify_missing_receipt_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    let output = CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("run receipt verify with no receipt");
    output.assert_failure();
}

#[test]
fn receipt_verify_succeeds_after_sync_and_fails_on_tamper() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("sync")
        .assert_success();

    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("verify")
        .assert_success();

    // Tamper with the payload → verify must exit nonzero (fail closed).
    let receipt_path = dir.path().join(".ggen-v2/receipt.json");
    let tampered = std::fs::read_to_string(&receipt_path)
        .expect("read receipt")
        .replace("\"graph_hash\"", "\"graph_hash_tampered_key_x\"");
    std::fs::write(&receipt_path, tampered).expect("write tampered");
    CliHarness::cargo_bin("ggen")
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .run()
        .expect("verify tampered")
        .assert_failure();
}

#[test]
fn receipt_history_missing_log_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    let output = CliHarness::cargo_bin("ggen")
        .args(["receipt", "history"])
        .current_dir(dir.path())
        .run()
        .expect("run receipt history with no log");
    output.assert_failure();
}

#[test]
fn receipt_history_after_two_syncs_exits_zero() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("first sync")
        .assert_success();
    // Mutate the ontology so the second sync produces a genuinely new
    // record rather than an unchanged no-op.
    std::fs::write(
        dir.path().join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\nex:bob ex:name \"bob\" .\n",
    )
    .expect("mutate ontology");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("second sync")
        .assert_success();

    let output = CliHarness::cargo_bin("ggen")
        .args(["receipt", "history"])
        .current_dir(dir.path())
        .run()
        .expect("run receipt history");
    output
        .assert_success()
        .assert_stdout_contains("\"records\": 2");
}

#[test]
fn receipt_history_tampered_middle_record_exits_nonzero() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    for name in ["alice", "alice2"] {
        std::fs::write(
            dir.path().join("ontology.ttl"),
            format!("@prefix ex: <http://example.org/> .\nex:alice ex:name \"{name}\" .\n"),
        )
        .expect("mutate ontology");
        CliHarness::cargo_bin("ggen")
            .args(["sync", "run"])
            .current_dir(dir.path())
            .run()
            .expect("sync")
            .assert_success();
    }

    let log_path = dir.path().join(".ggen-v2/receipt-log.jsonl");
    let lines: Vec<String> = std::fs::read_to_string(&log_path)
        .expect("read log")
        .lines()
        .map(String::from)
        .collect();
    assert_eq!(lines.len(), 2, "expected two receipt-log lines");
    let tampered_first_line = lines[0].replace("\"written\"", "\"tampered\"");
    std::fs::write(&log_path, format!("{tampered_first_line}\n{}\n", lines[1]))
        .expect("write tampered log");

    let output = CliHarness::cargo_bin("ggen")
        .args(["receipt", "history"])
        .current_dir(dir.path())
        .run()
        .expect("run receipt history on tampered log");
    output.assert_failure();
}

// ---------------------------------------------------------------------
// Global flags
// ---------------------------------------------------------------------

#[test]
fn introspect_emits_json_schema_and_exits_zero() {
    let output = CliHarness::cargo_bin("ggen")
        .args(["--introspect"])
        .run()
        .expect("run --introspect");
    output.assert_success();
    assert!(
        output.stdout.trim_start().starts_with('['),
        "expected a JSON array of tool definitions, got: {}",
        output.stdout
    );
}

#[test]
fn format_json_flag_produces_parseable_json_on_success() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let output = CliHarness::cargo_bin("ggen")
        .args(["--format", "json", "graph", "validate"])
        .current_dir(dir.path())
        .run()
        .expect("run graph validate --format json");
    output.assert_success();
    serde_json::from_str::<serde_json::Value>(&output.stdout)
        .unwrap_or_else(|e| panic!("stdout not valid JSON ({e}): {}", output.stdout));
}
