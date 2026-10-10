//! Chicago-TDD unit court for the hand-written handlers behind the generated
//! clap-noun-verb routes (`ggen_engine::verbs::handlers`): real TempDir
//! fixtures, real handler calls — no mocks, no CLI subprocess.
//!
//! Covered: `handle_graph_validate` (file mode, parse-only), `handle_doctor`
//! (health aggregation over a real synced project), `handle_law_load` /
//! `handle_law_validate` (N3 law engine over a real `[law].rules` project).
//!
//! Known coupling (deliberate, not refactored here): every verb handler
//! resolves its project root from the PROCESS cwd (`project_root()`), so
//! each test chdirs into its TempDir. That is process-global state, so every
//! test in this file serializes on one mutex.
//!
//! Pins per handler: happy-path result shape over a real fixture, typed
//! error paths reachable without the CLI, and call-idempotence (same inputs
//! twice -> same result).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;
use std::sync::Mutex;

use camino::Utf8PathBuf;
use ggen_engine::sync::{sync, SyncOptions};
use ggen_engine::verbs::handlers::{
    handle_doctor, handle_graph_validate, handle_law_load, handle_law_validate,
};
use tempfile::TempDir;

/// `project_root()` reads the process cwd — a single global resource. All
/// chdir-dependent tests lock this; cargo runs tests in one process's
/// threads, so one mutex is enough.
static CWD_LOCK: Mutex<()> = Mutex::new(());

/// chdir into `dir`, run `f`, restore the previous cwd (even on panic).
fn with_cwd<T>(dir: &Path, f: impl FnOnce() -> T) -> T {
    // unwrap_or_else(into_inner): a panic in one test poisons the mutex;
    // later tests only need mutual exclusion, not the poisoned verdict.
    let _guard = CWD_LOCK.lock().unwrap_or_else(|e| e.into_inner());
    let prev = std::env::current_dir().expect("read cwd");
    std::env::set_current_dir(dir).expect("chdir tempdir");
    // Restore cwd even when `f` panics: a leaked cwd into a TempDir makes
    // every later current_dir() call fail once that TempDir is dropped.
    let out = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    std::env::set_current_dir(&prev).expect("restore cwd");
    match out {
        Ok(v) => v,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:rex a ex:Dog .
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  results: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}\n{{ row.name }}\n{% endfor %}\n";

const LAW_TOML: &str = r#"
[project]
name = "lawdemo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[law]
rules = ["rules/animal.n3"]
"#;

const DERIVE_RULE_N3: &str =
    "@prefix ex: <http://example.org/>. {?s a ex:Dog} => {?s a ex:Animal}.";
const DENIAL_RULE_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => false.";

fn write(root: &Path, rel: &str, content: &str) {
    if let Some(parent) = Path::new(rel).parent() {
        std::fs::create_dir_all(root.join(parent)).expect("mkdir");
    }
    std::fs::write(root.join(rel), content).expect("write fixture");
}

/// A real frontmatter-schema project; `sync` it to mint a real receipt for
/// the doctor checks to consume.
fn scaffold_frontmatter(root: &Path) {
    write(root, "ggen.toml", GGEN_TOML);
    write(root, "ontology.ttl", ONTOLOGY);
    write(root, "templates/one.tmpl", TEMPLATE);
}

/// A real frontmatter-schema project with one N3 law rule configured.
fn scaffold_with_law(root: &Path, rule: &str) {
    write(root, "ggen.toml", LAW_TOML);
    write(root, "ontology.ttl", ONTOLOGY);
    write(root, "templates/one.tmpl", TEMPLATE);
    write(root, "rules/animal.n3", rule);
}

// ---------------------------------------------------------------------------
// 1. handle_graph_validate — file mode, parse-only (no shapes)
// ---------------------------------------------------------------------------

/// Happy path: a real Turtle file parses, and the result carries the file
/// path, a positive quad count, and a 32-hex BLAKE3 state hash.
#[test]
fn graph_validate_file_mode_happy_path() {
    let dir = TempDir::new().expect("tempdir");
    write(dir.path(), "good.ttl", ONTOLOGY);

    let report = with_cwd(dir.path(), || {
        handle_graph_validate(vec![Utf8PathBuf::from("good.ttl")], vec![])
    })
    .expect("valid turtle must validate");

    assert_eq!(report["files_checked"], 1);
    assert_eq!(report["files"].as_array().unwrap().len(), 1);
    let file = &report["files"][0];
    assert_eq!(file["path"], "good.ttl");
    let quads = file["quads"].as_u64().unwrap();
    assert!(quads >= 2, "ontology has at least 2 triples, got {quads}");
    assert_eq!(file["hash"].as_str().unwrap().len(), 64);
}

/// Typed error, no panic: an unreadable file and a malformed Turtle file
/// are both accumulated into ONE non-zero execution error naming each
/// offending path (errors are never short-circuited).
#[test]
fn graph_validate_file_mode_accumulates_typed_errors() {
    let dir = TempDir::new().expect("tempdir");
    write(
        dir.path(),
        "broken.ttl",
        "@prefix ex: <http://example.org/> . ex:alice ex:name",
    );

    let err = with_cwd(dir.path(), || {
        handle_graph_validate(
            vec![
                Utf8PathBuf::from("missing.ttl"),
                Utf8PathBuf::from("broken.ttl"),
            ],
            vec![],
        )
    })
    .expect_err("invalid inputs must be a typed error, never a panic");

    let msg = err.to_string();
    assert!(msg.contains("2 of 2 file(s) invalid"), "{msg}");
    assert!(msg.contains("missing.ttl"), "{msg}");
    assert!(msg.contains("broken.ttl"), "{msg}");
}

/// Idempotence: the same input twice produces the byte-identical result
/// (deterministic parse + deterministic BLAKE3 hash, no ambient state).
#[test]
fn graph_validate_file_mode_is_idempotent() {
    let dir = TempDir::new().expect("tempdir");
    write(dir.path(), "good.ttl", ONTOLOGY);

    let (a, b) = with_cwd(dir.path(), || {
        let a =
            handle_graph_validate(vec![Utf8PathBuf::from("good.ttl")], vec![]).expect("first run");
        let b =
            handle_graph_validate(vec![Utf8PathBuf::from("good.ttl")], vec![]).expect("second run");
        (a, b)
    });
    assert_eq!(a, b, "same inputs must give the identical result");
}

// ---------------------------------------------------------------------------
// 2. handle_doctor — health aggregation over a real synced project
// ---------------------------------------------------------------------------

/// Happy path: a real sync mints a receipt; doctor reports healthy with all
/// three checks passing.
#[test]
fn doctor_healthy_after_real_sync() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_frontmatter(dir.path());
    with_cwd(dir.path(), || {
        sync(dir.path(), SyncOptions::default()).expect("real sync must succeed");

        let report = handle_doctor().expect("freshly synced project must be healthy");
        assert_eq!(report["healthy"], true);
        assert_eq!(report["checks"]["lockfile_drift"]["status"], "pass");
        assert_eq!(report["checks"]["orphaned_artifacts"]["status"], "pass");
        assert_eq!(report["checks"]["receipt_staleness"]["status"], "pass");
    });
}

/// Deliberately broken project: mutate a receipt-recorded output on disk
/// after the sync. Doctor must fail closed with `receipt_staleness` NAMED
/// in the error (healthy=false is surfaced as a typed non-zero failure,
/// never a cheerful report).
#[test]
fn doctor_names_receipt_staleness_on_tampered_output() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_frontmatter(dir.path());
    with_cwd(dir.path(), || {
        sync(dir.path(), SyncOptions::default()).expect("real sync");

        std::fs::write(dir.path().join("out/names.txt"), "tampered\n").expect("tamper output");

        let err = handle_doctor().expect_err("tampered output must fail doctor");
        let msg = err.to_string();
        assert!(msg.contains("receipt_staleness"), "{msg}");
        assert!(msg.contains("hash-mismatched"), "{msg}");
        assert!(msg.contains("out/names.txt"), "{msg}");
    });
}

/// Typed error, no panic: no `ggen.toml` at all is a hard execution error
/// naming the dispatch failure, not an Ok-with-mystery-shape.
#[test]
fn doctor_on_empty_directory_is_typed_error() {
    let dir = TempDir::new().expect("tempdir");
    with_cwd(dir.path(), || {
        let err = handle_doctor().expect_err("empty dir has no ggen.toml");
        assert!(!err.to_string().is_empty());
    });
}

// ---------------------------------------------------------------------------
// 3. handle_law_load / handle_law_validate — real N3 law engine
// ---------------------------------------------------------------------------

/// Happy path: `[law].rules` with one real N3 file loads and reports the
/// per-file rule count; identical second call is identical (idempotent).
#[test]
fn law_load_happy_path_and_idempotence() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_with_law(dir.path(), DERIVE_RULE_N3);

    let (a, b) = with_cwd(dir.path(), || {
        (
            handle_law_load().expect("law load"),
            handle_law_load().expect("law load again"),
        )
    });
    assert_eq!(a["rule_files"], 1);
    assert_eq!(a["rules_per_file"]["rules/animal.n3"], 1);
    assert_eq!(a, b, "law load must be deterministic across calls");
}

/// Happy path for the validator: a non-denial derive rule over a real
/// ontology materializes clean — conforms=true, one rule loaded, zero
/// shapes checked (no `[law]`/`[validation]` shapes configured).
#[test]
fn law_validate_conforms_with_derive_rule() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_with_law(dir.path(), DERIVE_RULE_N3);

    let report = with_cwd(dir.path(), || handle_law_validate()).expect("derive rule conforms");
    assert_eq!(report["conforms"], true);
    assert_eq!(report["rules_loaded"], 1);
    assert_eq!(report["denials"], 0);
    assert_eq!(report["shapes_checked"], 0);
}

/// Typed law refusal, no CLI: a violated denial rule (`{ body } => false.`)
/// against a real asserted dog must be an `Err` carrying the FM-LAW code —
/// never an Ok report and never a panic.
#[test]
fn law_validate_denial_violation_is_typed_fm_law_error() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_with_law(dir.path(), DENIAL_RULE_N3);

    let err = with_cwd(dir.path(), || {
        handle_law_validate().expect_err("asserted dog under a no-dogs denial must refuse")
    });
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW"), "{msg}");
    assert!(msg.contains("denial"), "{msg}");
}

/// Typed error, no panic: `[law].rules` naming a missing file is a hard
/// execution error naming that file.
#[test]
fn law_load_missing_rule_file_is_typed_error() {
    let dir = TempDir::new().expect("tempdir");
    scaffold_with_law(dir.path(), DERIVE_RULE_N3);
    std::fs::remove_file(dir.path().join("rules/animal.n3")).expect("remove rule");

    let err = with_cwd(dir.path(), || {
        handle_law_load().expect_err("missing rule file must be a typed error")
    });
    let msg = err.to_string();
    assert!(msg.contains("rules/animal.n3"), "{msg}");
    assert!(msg.contains("unreadable"), "{msg}");
}
