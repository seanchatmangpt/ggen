//! Chicago-TDD end-to-end proof for `packs/fortune5-testing-bblock-pack`: a
//! real temp consumer project, a real `ggen.toml`, the real `ggen` binary
//! spawned as a subprocess (`CliHarness`, no library calls, no mocks), real
//! generated files asserted on disk, and real execution of the generated
//! artifacts (bash syntax check, a real Python subprocess run).
//!
//! This pack ships `[[generation.rules]]` (the "declarative-rules" `ggen.toml`
//! schema -- `query = { file = ... }` / `template = { file = ... }` explicit
//! file references, no directory-scanned frontmatter templates). That schema
//! has no `[[packs]]` pack-resolution implementation yet (see
//! `.claude/rules/architecture.md`'s "ggen.toml has two schemas" note and
//! `crate::generation_rules`'s own module doc), so a consumer cannot pull
//! this pack in via `[[packs]]`/`[packs]` the way the frontmatter-schema
//! framework packs in `framework_packs_e2e.rs` do. The real, working
//! consumption path (verified empirically this session) is: the consumer's
//! own declarative-rules `ggen.toml` imports the pack's `ontology.ttl` via
//! `[ontology].imports`, then re-declares generation rules whose
//! `query`/`template` file paths point at the pack's `queries/`/`templates/`
//! directory by relative path -- exactly what this file's `scaffold_consumer`
//! does.
//!
//! Two real, root-cause bugs were found and fixed in the pack's own files
//! while building this checkpoint (not the engine -- both are reproducible
//! with the pack alone, no consumer involved):
//!
//! 1. **Pack unusable via any `[packs]`-style resolution.** The pack's
//!    `templates/` shipped nested three levels deep
//!    (`templates/consumer/testing-bblock/{,suites/}*.tmpl`). Every
//!    successfully-consumed framework pack in this workspace (see
//!    `framework_packs_e2e.rs`) ships templates flat, directly under
//!    `templates/`, because `ggen_engine::pack::resolve_pack_dir` only
//!    `read_dir`s the top level of a pack's `templates/` (non-recursive --
//!    confirmed by reading `crates/ggen-engine/src/pack.rs`), unlike a
//!    project's own `[templates].dir` (`collect_tmpl_paths`, recursive).
//!    Reproduced empirically: referencing this pack from a frontmatter-schema
//!    consumer via `[packs]` failed `[FM-PACK-005] zero templates`. Fixed by
//!    flattening `templates/` to match the rest of the workspace's packs
//!    (same filenames, no subdirectories); the pack's own `ggen.toml`
//!    `template = { file = ... }` paths were updated to match.
//! 2. **The pack was broken even running standalone.**
//!    `queries/testing-bblock.rq` was a bare `SELECT ?version ... LIMIT 1`
//!    with no `ORDER BY`. `[validation].strict_mode` defaults to `true`
//!    (`ggen-config/src/manifest/types.rs`), which turns a missing
//!    `ORDER BY` on a `SELECT` into a hard `E0013` refusal, not a warning.
//!    Reproduced empirically: `ggen sync run --dry-run` from inside
//!    `packs/fortune5-testing-bblock-pack/` itself failed outright with
//!    `error[E0013]: ... SELECT query lacks ORDER BY`. Fixed by adding
//!    `ORDER BY ?version` to the query.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};
use std::process::Command;

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Repository `packs/` directory (relative to this crate's manifest).
fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir");
    for entry in std::fs::read_dir(src).expect("read_dir") {
        let entry = entry.expect("entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy");
        }
    }
}

/// Scaffold a real consumer project next to a real copy of
/// `fortune5-testing-bblock-pack`. The consumer's own declarative-rules
/// `ggen.toml` imports the pack's ontology and re-declares the pack's own
/// generation rules by relative file path -- the real, working consumption
/// path for a declarative-rules pack today (see module doc). Returns
/// `(tempdir, consumer_project_root)`.
fn scaffold_consumer() -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(
        &packs_dir().join("fortune5-testing-bblock-pack"),
        &dir.path().join("fortune5-testing-bblock-pack"),
    );

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(&project).expect("mkdir consumer");
    std::fs::write(project.join("ontology.ttl"), "").expect("write empty consumer ontology");
    std::fs::write(
        project.join("ggen.toml"),
        r#"
[project]
name = "testing-bblock-consumer"
version = "0.1.0"

[ontology]
source = "ontology.ttl"
imports = ["../fortune5-testing-bblock-pack/ontology.ttl"]

[generation]
output_dir = "."

[[generation.rules]]
name = "testing-verifier"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/testing_bblock.py.tmpl" }
output_file = "consumer/testing-bblock/testing_bblock.py"
skip_empty = false
mode = "Overwrite"

[[generation.rules]]
name = "protocol-unit-suite"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/suite.sh.tmpl" }
output_file = "consumer/testing-bblock/suites/protocol-unit.sh"
skip_empty = false
mode = "Overwrite"

[[generation.rules]]
name = "replay-suite"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/suite.sh.tmpl" }
output_file = "consumer/testing-bblock/suites/replay.sh"
skip_empty = false
mode = "Overwrite"

[[generation.rules]]
name = "aggregate-verifier"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/verify-all.sh.tmpl" }
output_file = "consumer/testing-bblock/suites/verify-all.sh"
skip_empty = false
mode = "Overwrite"

[[generation.rules]]
name = "verifier-report-schema"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/verifier-report.schema.json.tmpl" }
output_file = "consumer/testing-bblock/verifier-report.schema.json"
skip_empty = false
mode = "Overwrite"

[[generation.rules]]
name = "testing-bblock-readme"
query = { file = "../fortune5-testing-bblock-pack/queries/testing-bblock.rq" }
template = { file = "../fortune5-testing-bblock-pack/templates/README.md.tmpl" }
output_file = "consumer/testing-bblock/README.md"
skip_empty = false
mode = "Overwrite"
"#,
    )
    .expect("write consumer ggen.toml");
    (dir, project)
}

fn read(project: &Path, relative: &str) -> String {
    std::fs::read_to_string(project.join(relative))
        .unwrap_or_else(|error| panic!("read {relative}: {error}"))
}

/// Core value proof: a real `ggen sync run` subprocess, over a real
/// consumer project importing the pack's ontology and rules by path,
/// manufactures the verifier script, a suite dispatcher script, the
/// aggregate verifier, and the JSON schema -- with real content, not just
/// "the command exited 0".
#[allow(clippy::too_many_lines)]
#[test]
fn testing_bblock_pack_generates_verifier_and_suite_scripts_with_real_content() {
    let (_dir, project) = scaffold_consumer();

    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("spawn ggen sync run")
        .assert_success();

    for relative in [
        "consumer/testing-bblock/testing_bblock.py",
        "consumer/testing-bblock/suites/protocol-unit.sh",
        "consumer/testing-bblock/suites/replay.sh",
        "consumer/testing-bblock/suites/verify-all.sh",
        "consumer/testing-bblock/verifier-report.schema.json",
        "consumer/testing-bblock/README.md",
    ] {
        assert!(project.join(relative).is_file(), "missing {relative}");
    }

    // The verifier script: real BLAKE3 implementation, real suite dispatch
    // table, real CLI subcommands -- content only this pack's template
    // could have produced.
    let verifier = read(&project, "consumer/testing-bblock/testing_bblock.py");
    for needle in [
        "def blake3_hex(data: bytes) -> str:",
        "SUITE_FUNCTIONS: dict[str, Callable[[Path, Path], dict[str, Any]]] = {",
        "\"protocol-unit\": protocol_unit,",
        "\"replay\": replay,",
        "def self_test() -> None:",
        "ggen.testing.verifier-report.v1",
    ] {
        assert!(verifier.contains(needle), "verifier missing {needle}");
    }

    // Suite dispatcher: derives its own suite name from its filename at
    // runtime (basename), so protocol-unit.sh and replay.sh are
    // byte-identical rule output -- proven directly.
    let protocol_unit = read(&project, "consumer/testing-bblock/suites/protocol-unit.sh");
    let replay = read(&project, "consumer/testing-bblock/suites/replay.sh");
    assert_eq!(
        protocol_unit, replay,
        "suite.sh.tmpl is suite-agnostic; per-suite identity comes from \
         $(basename) at runtime, not the template"
    );
    assert!(
        protocol_unit.contains(r#"SUITE="$(basename "${BASH_SOURCE[0]}" .sh)""#),
        "{protocol_unit}"
    );
    assert!(
        protocol_unit.contains(
            r#"exec python3 "$ROOT/consumer/testing-bblock/testing_bblock.py" run "$SUITE""#
        ),
        "{protocol_unit}"
    );

    // Aggregate verifier: runs verify, then rewrites evidence paths relative
    // to the report and rebuilds the BLAKE3 chain before replaying it.
    let verify_all = read(&project, "consumer/testing-bblock/suites/verify-all.sh");
    assert!(
        verify_all.contains("testing_bblock.py\" verify --report"),
        "{verify_all}"
    );
    assert!(verify_all.contains("replay-report"), "{verify_all}");

    // JSON schema is real, parseable JSON declaring the exact nine suites.
    let schema_text = read(
        &project,
        "consumer/testing-bblock/verifier-report.schema.json",
    );
    let schema: serde_json::Value =
        serde_json::from_str(&schema_text).expect("verifier-report.schema.json must be valid JSON");
    assert_eq!(
        schema["properties"]["standing"]["enum"],
        serde_json::json!(["ALIVE", "BLOCKED", "BUILD_BROKEN"])
    );
    let suite_enum = schema["properties"]["suite_order"]["items"]["enum"]
        .as_array()
        .expect("suite_order.items.enum must be an array");
    assert_eq!(suite_enum.len(), 9, "schema: {schema_text}");
    assert!(
        suite_enum.contains(&serde_json::json!("cli-e2e")),
        "schema: {schema_text}"
    );

    // Second sync is idempotent: exit 0, real bytes unchanged.
    let before = std::fs::read(project.join("consumer/testing-bblock/testing_bblock.py"))
        .expect("verifier bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = std::fs::read(project.join("consumer/testing-bblock/testing_bblock.py"))
        .expect("verifier bytes after");
    assert_eq!(before, after, "second sync must leave outputs unchanged");
}

/// Every generated shell script must be syntactically valid bash (`bash -n`)
/// -- catches template corruption `ggen sync run` succeeding would not.
#[test]
fn generated_suite_scripts_are_syntactically_valid_bash() {
    let (_dir, project) = scaffold_consumer();
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("spawn ggen sync run")
        .assert_success();

    for relative in [
        "consumer/testing-bblock/suites/protocol-unit.sh",
        "consumer/testing-bblock/suites/replay.sh",
        "consumer/testing-bblock/suites/verify-all.sh",
    ] {
        let path = project.join(relative);
        let parsed = Command::new("bash")
            .arg("-n")
            .arg(&path)
            .output()
            .unwrap_or_else(|error| panic!("spawn bash -n {relative}: {error}"));
        assert!(
            parsed.status.success(),
            "{relative} failed bash -n: {}",
            String::from_utf8_lossy(&parsed.stderr)
        );
    }
}

/// Strongest proof of real value: the generated `testing_bblock.py` is not
/// just well-formed text -- its `self-test` subcommand is actually executed
/// as a real Python subprocess and must pass, exercising the pack's from-
/// scratch BLAKE3 implementation against real BLAKE3 test vectors and a
/// real subprocess-boundary crossing (success + nonzero-exit cases).
#[test]
fn generated_verifier_self_test_actually_passes_as_a_real_subprocess() {
    let (_dir, project) = scaffold_consumer();
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("spawn ggen sync run")
        .assert_success();

    let verifier = project.join("consumer/testing-bblock/testing_bblock.py");
    let output = Command::new("python3")
        .arg(&verifier)
        .arg("self-test")
        .current_dir(&project)
        .output()
        .expect("spawn python3 testing_bblock.py self-test");
    assert!(
        output.status.success(),
        "self-test exited {:?}\nstdout: {}\nstderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("TESTING_BBLOCK_SELF_TEST standing=PARTIAL_ALIVE blake3=4 subprocess=2"),
        "unexpected self-test stdout: {stdout}"
    );
}
