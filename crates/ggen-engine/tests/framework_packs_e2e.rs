//! Chicago-TDD end-to-end proofs for the framework packs under `packs/`:
//! real filesystem (`TempDir`), real oxigraph, real Tera, and the real NEW
//! `ggen` binary spawned as a subprocess (`CliHarness`). No mocks.
//!
//! Shared scaffold: copy one framework pack plus a minimal consumer project
//! (referencing the pack via a relative `{ path = "../<pack>" }`) into a
//! fresh TempDir, then drive `ggen sync run` / `ggen graph validate`.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Repository `packs/` directory (relative to this crate's manifest).
fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// Recursively copy `src` into `dst`.
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

/// Copy `pack_name` from `packs/` plus a minimal consumer project into a
/// fresh TempDir; return `(tempdir, project_root)`. The project has an empty
/// local ontology and an empty local templates dir, so only the pack's
/// templates run over the pack's ontology.
fn scaffold_pack_project(pack_name: &str) -> (TempDir, PathBuf) {
    let dir = TempDir::new().expect("tempdir");
    copy_tree(&packs_dir().join(pack_name), &dir.path().join(pack_name));

    let project = dir.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{pack_name} = {{ path = \"../{pack_name}\" }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("write ggen.toml");
    (dir, project)
}

#[test]
fn wasm4pm_compat_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("wasm4pm-compat-pack");

    // (1) First sync via the real binary succeeds.
    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_success();

    // (2) The emission enum landed where such code lives (src/), with
    // content that can only have come from the pack ontology's EventType
    // individuals ("graph_union_hashed" etc.).
    let events = project.join("src/wasm4pm_compat_events.rs");
    assert!(
        events.is_file(),
        "pack must write src/wasm4pm_compat_events.rs"
    );
    let events_src = std::fs::read_to_string(&events).expect("events.rs");
    assert!(
        events_src.contains("pub enum EmittedEventType"),
        "{events_src}"
    );
    assert!(events_src.contains("GraphUnionHashed"), "{events_src}");
    assert!(
        events_src.contains("\"graph_union_hashed\""),
        "{events_src}"
    );
    assert!(
        events_src.contains("pub fn emit_pack_lock_verified"),
        "{events_src}"
    );
    assert!(
        events_src.contains("wasm4pm_compat::ocel::{OCELEvent, OCELRelationship}"),
        "{events_src}"
    );

    // The boundary doc landed too, listing all three ontology event types.
    let doc = std::fs::read_to_string(project.join("docs/wasm4pm_compat_emission.md"))
        .expect("emission doc");
    for name in [
        "graph_union_hashed",
        "pack_lock_verified",
        "receipt_chained",
    ] {
        assert!(doc.contains(name), "doc missing {name}: {doc}");
    }

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.wasm4pm-compat-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    let before = std::fs::read_to_string(&events).expect("events.rs");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = std::fs::read_to_string(&events).expect("events.rs after");
    assert_eq!(before, after, "second sync must leave outputs unchanged");
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// lsp-max-pack: `lm:LintRule` individuals in the pack ontology precipitate
/// one RulePackServer `rules/lsp_max_*.toml` regex rule file per rule plus a
/// docs index; sync is idempotent and `ggen graph validate` passes.
#[test]
fn lsp_max_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("lsp-max-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) One rules/*.toml per LintRule, landed where a RulePackServer
    // would actually load them (no generated/ dir anywhere).
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let unwrap_rule = project.join("rules/lsp_max_lspmax_unwrap_001.toml");
    assert!(unwrap_rule.is_file(), "expected {}", unwrap_rule.display());
    let toml = std::fs::read_to_string(&unwrap_rule).expect("rule toml");
    // Values only the pack ontology could have produced.
    assert!(
        toml.contains("id = \"LSPMAX-UNWRAP-001\""),
        "rule toml: {toml}"
    );
    assert!(toml.contains("severity = \"error\""), "rule toml: {toml}");
    assert!(
        toml.contains(r"pattern = '\.unwrap\(\)|\.expect\('"),
        "rule toml: {toml}"
    );
    assert!(
        toml.contains("RulePackServer handlers return ClassifiedFindings"),
        "rationale must flow from the ontology: {toml}"
    );
    assert!(
        project
            .join("rules/lsp_max_lspmax_rawjson_002.toml")
            .is_file(),
        "second rule file missing"
    );
    assert!(
        project
            .join("rules/lsp_max_lspmax_wallclock_003.toml")
            .is_file(),
        "third rule file missing"
    );
    let index =
        std::fs::read_to_string(project.join("docs/lsp_max_rule_pack.md")).expect("rule index doc");
    assert!(index.contains("`LSPMAX-WALLCLOCK-003`"), "index: {index}");

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.lsp-max-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    let before = std::fs::read(&unwrap_rule).expect("rule bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = std::fs::read(&unwrap_rule).expect("rule bytes after");
    assert_eq!(
        before, after,
        "second sync must leave the rule file unchanged"
    );
}

/// star-toml-pack: `stp:ConfigSection`/`stp:ConfigField` individuals in the
/// pack ontology precipitate a deny_unknown_fields serde config module
/// (`src/star_toml_config.rs`, loaded via `star_toml::load_file`) plus one
/// admission doc per section; sync is idempotent and `graph validate` passes.
#[test]
fn star_toml_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("star-toml-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) The generated config module landed under src/ (no generated/ dir)
    // with values that can only have come from the pack ontology.
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let module = project.join("src/star_toml_config.rs");
    assert!(module.is_file(), "pack must write src/star_toml_config.rs");
    let src = std::fs::read_to_string(&module).expect("config module");
    assert!(src.contains("pub struct AdmissionConfig"), "module: {src}");
    assert!(src.contains("pub struct TelemetryConfig"), "module: {src}");
    assert!(
        src.contains("pub witness_dir: std::path::PathBuf"),
        "module: {src}"
    );
    assert!(src.contains("pub fail_closed: bool"), "module: {src}");
    assert!(
        src.contains("pub exporter_endpoint: String"),
        "module: {src}"
    );
    assert!(src.contains("pub sample_rate: f64"), "module: {src}");
    assert!(
        src.contains("BLAKE3 ConfigWitness envelopes are persisted"),
        "distinctive ontology doc string must flow into the module: {src}"
    );
    assert!(
        src.contains("#[serde(deny_unknown_fields)]"),
        "module: {src}"
    );
    assert!(
        src.contains("star_toml::load_file::<Self>(path)"),
        "module: {src}"
    );

    // Per-section admission docs, path driven by ?sectionLabel.
    let admission_doc = std::fs::read_to_string(project.join("docs/star_toml/admission.md"))
        .expect("admission doc");
    assert!(
        admission_doc.contains("`witness_dir`"),
        "doc: {admission_doc}"
    );
    let telemetry_doc = std::fs::read_to_string(project.join("docs/star_toml/telemetry.md"))
        .expect("telemetry doc");
    assert!(
        telemetry_doc.contains("`sample_rate`"),
        "doc: {telemetry_doc}"
    );

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.star-toml-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    let module_before = std::fs::read(&module).expect("module bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    assert_eq!(
        std::fs::read(&module).expect("module bytes after"),
        module_before,
        "second sync must leave src/star_toml_config.rs unchanged"
    );
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// chicago-tdd-tools-pack: `ctt:CliBoundaryTest` individuals precipitate a
/// CliHarness-based `tests/chicago_tdd_tools_boundary.rs` (real subprocess
/// `#[test]` fns: exit-code / stdout / stderr assertions) plus a boundary
/// doc; sync is idempotent and `ggen graph validate` passes.
#[test]
fn chicago_tdd_tools_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("chicago-tdd-tools-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) The generated boundary test file landed where tests live (no
    // generated/ dir), with content only the pack ontology's
    // ctt:CliBoundaryTest individuals could have produced.
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let boundary = project.join("tests/chicago_tdd_tools_boundary.rs");
    assert!(
        boundary.is_file(),
        "pack must write tests/chicago_tdd_tools_boundary.rs"
    );
    let src = std::fs::read_to_string(&boundary).expect("boundary tests");
    assert!(
        src.contains("use chicago_tdd_tools::cli_proof::CliHarness;"),
        "{src}"
    );
    assert!(src.contains("fn receiptctl_help_lists_verbs()"), "{src}");
    assert!(src.contains("fn receiptctl_version_emits_name()"), "{src}");
    assert!(
        src.contains("fn receiptctl_unknown_verb_fails_closed()"),
        "{src}"
    );
    assert!(
        src.contains("CliHarness::cargo_bin(\"receiptctl\")"),
        "{src}"
    );
    assert!(src.contains(".assert_exit_code(2)"), "{src}");
    assert!(src.contains(".assert_stdout_contains(\"Usage\")"), "{src}");
    assert!(src.contains(".assert_stderr_contains(\"error\")"), "{src}");

    // The companion doc landed too, carrying every ontology-driven axiom.
    let doc = std::fs::read_to_string(project.join("docs/chicago_tdd_tools_boundary.md"))
        .expect("boundary doc");
    assert!(doc.contains("receiptctl_help_lists_verbs"), "{doc}");
    assert!(
        doc.contains("an unknown subcommand exits nonzero with a clap error on stderr"),
        "{doc}"
    );

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock.contains("[packs.chicago-tdd-tools-pack]"),
        "lock: {lock}"
    );
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let src2 = std::fs::read_to_string(&boundary).expect("boundary tests after");
    assert_eq!(src, src2, "second sync must leave outputs unchanged");
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// clap-noun-verb-pack: `cnv:Command` individuals in the pack ontology
/// precipitate a `src/clap_noun_verb_routes.rs` route skeleton (noun-verb
/// `#[verb]` fns calling handler stubs) plus a docs command table; sync is
/// idempotent and `ggen graph validate` passes.
#[test]
fn clap_noun_verb_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("clap-noun-verb-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) Route file landed where such code lives (src/), no generated/ dir.
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let routes_path = project.join("src/clap_noun_verb_routes.rs");
    assert!(
        routes_path.is_file(),
        "pack must write src/clap_noun_verb_routes.rs"
    );
    let routes = std::fs::read_to_string(&routes_path).expect("routes.rs");
    // Values only the pack ontology's cnv:Command individuals could produce.
    assert!(
        routes.contains("Verify the active session token against the stored keyring credential."),
        "distinctive cnv:doc label missing: {routes}"
    );
    assert!(
        routes.contains(r#"#[clap_noun_verb_macros::verb("verify")]"#),
        "verb attribute missing: {routes}"
    );
    assert!(
        routes.contains("fn session_verify() -> Result<serde_json::Value>"),
        "route fn missing: {routes}"
    );
    assert!(
        routes.contains("crate::verbs::handlers::user_create_handler()"),
        "handler stub call missing: {routes}"
    );
    // Noun `--help` descriptions, generated from cnv:Noun individuals —
    // registered via linkme rather than left to the macro's runtime
    // file-scrape fallback (see the template's own comment for why).
    assert!(
        routes.contains(r#"static REGISTER_SESSION_NOUN: fn() = register_session_noun;"#),
        "session noun registration missing: {routes}"
    );
    assert!(
        routes.contains("Manage authenticated session tokens."),
        "session noun about text missing: {routes}"
    );

    // Companion docs table from the second template.
    let doc =
        std::fs::read_to_string(project.join("docs/clap_noun_verb_routes.md")).expect("routes doc");
    assert!(
        doc.contains("| `session` | `login` | `session_login_handler` |"),
        "doc: {doc}"
    );

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.clap-noun-verb-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let routes_again = std::fs::read_to_string(&routes_path).expect("routes.rs after");
    assert_eq!(
        routes, routes_again,
        "second sync must leave the route file unchanged"
    );
}

/// praxis-core-pack: `pxc:RefusalScenario` individuals in the pack ontology
/// precipitate the refusal-taxonomy reference table (Rust const table +
/// markdown doc) mirroring `praxis_core::refusal` categories; sync is
/// idempotent and `ggen graph validate` passes.
#[test]
fn praxis_core_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("praxis-core-pack");

    // (1) First sync via the real binary writes both taxonomy artifacts in
    // place under the consumer project (no generated/ dir).
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );

    let rs_path = project.join("src/praxis_core_refusal_table.rs");
    assert!(
        rs_path.is_file(),
        "pack must write src/praxis_core_refusal_table.rs"
    );
    let rs = std::fs::read_to_string(&rs_path).expect("refusal table");
    // Values that can only have come from the pack's RefusalScenario
    // individuals (mirroring praxis_core::refusal::RefusalScenario::category).
    assert!(
        rs.contains("pub const PRAXIS_CORE_REFUSAL_TAXONOMY"),
        "{rs}"
    );
    assert!(
        rs.contains("Conformance gate rejected the POWL replay projection."),
        "distinctive ontology note must flow into the Rust table: {rs}"
    );
    assert!(rs.contains("scenario: \"WatchdogDrained\""), "{rs}");
    assert!(rs.contains("denial_lane: \"WATCHDOG_DRAINED\""), "{rs}");
    assert!(rs.contains("category: \"temporal\""), "{rs}");

    // The markdown reference table landed too, with all four scenarios.
    let md = std::fs::read_to_string(project.join("docs/praxis_core_refusal_taxonomy.md"))
        .expect("taxonomy doc");
    assert!(
        md.contains("| `AuthorizationDenied` | authorization | `AUTHORIZATION_DENIED` |"),
        "markdown row: {md}"
    );
    for name in [
        "WatchdogDrained",
        "AuthorizationDenied",
        "ResourceExhausted",
        "ConformanceGateFailed",
    ] {
        assert!(md.contains(name), "doc missing {name}: {md}");
    }

    // (2) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(lock.contains("[packs.praxis-core-pack]"), "lock: {lock}");
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (3) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (4) Second sync is idempotent: exit 0, outputs byte-identical.
    let before = std::fs::read(&rs_path).expect("table bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = std::fs::read(&rs_path).expect("table bytes after");
    assert_eq!(before, after, "second sync must leave the table unchanged");
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock idem");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// wasm4pm-cognition-pack: `compat:CognitionBreed` individuals in the pack
/// ontology precipitate a typed breed catalog (`src/w4pm_cognition_catalog.rs`)
/// and a per-breed dispatch-surface skeleton over the stable 6-verb ABI
/// (`src/w4pm_cognition_dispatch.rs`); sync is idempotent and
/// `ggen graph validate` passes.
#[test]
fn wasm4pm_cognition_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("wasm4pm-cognition-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) Catalog landed where such code lives (src/), no generated/ dir,
    // with values only the pack ontology's breed instances could produce.
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let catalog_path = project.join("src/w4pm_cognition_catalog.rs");
    assert!(
        catalog_path.is_file(),
        "pack must write src/w4pm_cognition_catalog.rs"
    );
    let catalog = std::fs::read_to_string(&catalog_path).expect("catalog.rs");
    assert!(catalog.contains("pub enum CognitionBreedId"), "{catalog}");
    assert!(catalog.contains("pub const BREED_CATALOG"), "{catalog}");
    assert!(
        catalog.contains("pub fn from_breed_id(s: &str) -> Option<CognitionBreedId>"),
        "{catalog}"
    );
    // Distinctive breed citation flowed verbatim from the ontology.
    assert!(
        catalog.contains("Fikes, R. E., & Nilsson, N. J. (1971). STRIPS"),
        "STRIPS citation must flow from the ontology: {catalog}"
    );
    for variant in [
        "Strips",
        "Gps",
        "HtnPlanning",
        "PartialOrderPlan",
        "Prolog",
        "BayesianNetwork",
        "DempsterShafer",
        "FuzzyLogic",
        "Mycin",
        "Eliza",
        "Soar",
        "ActR",
        "Hearsay",
    ] {
        assert!(catalog.contains(variant), "catalog missing breed {variant}");
    }
    // breedStatus is CONSTRUCT-derived by design and must never surface here.
    assert!(!catalog.contains("breedStatus"), "{catalog}");

    // Dispatch skeleton: one documented stub per breed delegating to the
    // single hand-completable helper.
    let dispatch_path = project.join("src/w4pm_cognition_dispatch.rs");
    assert!(
        dispatch_path.is_file(),
        "pack must write src/w4pm_cognition_dispatch.rs"
    );
    let dispatch = std::fs::read_to_string(&dispatch_path).expect("dispatch.rs");
    assert!(
        dispatch.contains("pub fn dispatch_cognition_run(breed_id: &str, input_json: &str) -> Result<String, String>"),
        "{dispatch}"
    );
    assert!(
        dispatch.contains("pub fn run_strips(input_json: &str) -> Result<String, String>"),
        "{dispatch}"
    );
    assert!(
        dispatch.contains("dispatch_cognition_run(\"dempster_shafer\", input_json)"),
        "{dispatch}"
    );
    assert!(
        dispatch.contains("Shafer, G. (1976). A Mathematical Theory of Evidence."),
        "Dempster–Shafer citation must flow into the stub doc: {dispatch}"
    );
    assert!(
        dispatch.contains("`cognition_run`"),
        "6-verb ABI reference missing: {dispatch}"
    );

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock.contains("[packs.wasm4pm-cognition-pack]"),
        "lock: {lock}"
    );
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    let before = std::fs::read(&catalog_path).expect("catalog bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    assert_eq!(
        std::fs::read(&catalog_path).expect("catalog bytes after"),
        before,
        "second sync must leave the catalog unchanged"
    );
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}

/// wasm4pm-algorithms-pack: `pi:ProcessIntelligenceAlgorithm` individuals
/// (carried over from the wasm4pm algorithm ontology) precipitate a typed
/// `src/w4pm_algorithms_catalog.rs` (AlgorithmId enum + const CATALOG +
/// by_wasm_export lookup) and a `docs/w4pm_algorithms.md` reference table;
/// sync is idempotent and `ggen graph validate` passes.
#[test]
fn wasm4pm_algorithms_pack_syncs() {
    let (_dir, project) = scaffold_pack_project("wasm4pm-algorithms-pack");

    // (1) First sync via the real binary succeeds.
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // (2) The typed catalog landed where such code lives (src/), with
    // content only the pack ontology's algorithm individuals could produce.
    assert!(
        !project.join("generated").exists(),
        "no generated/ dir allowed"
    );
    let catalog_path = project.join("src/w4pm_algorithms_catalog.rs");
    assert!(
        catalog_path.is_file(),
        "pack must write src/w4pm_algorithms_catalog.rs"
    );
    let catalog = std::fs::read_to_string(&catalog_path).expect("catalog.rs");
    assert!(catalog.contains("pub enum AlgorithmId"), "{catalog}");
    assert!(catalog.contains("InductiveMiner"), "{catalog}");
    assert!(catalog.contains("EtconformancePrecision"), "{catalog}");
    assert!(
        catalog.contains("\"discover_inductive_miner\" => Some(AlgorithmId::InductiveMiner)"),
        "by_wasm_export arm missing: {catalog}"
    );
    assert!(
        catalog.contains("algorithm_id: \"predict_remaining_time\""),
        "{catalog}"
    );
    assert!(
        catalog.contains("wasm_export: \"predict_case_duration\""),
        "wasm export names must be carried over exactly: {catalog}"
    );
    assert!(
        catalog.contains("pub fn by_wasm_export(export: &str)"),
        "{catalog}"
    );

    // The reference doc landed too, carrying real paper citations that can
    // only have come from the ontology (e.g. the 2013 Inductive Miner paper).
    let doc =
        std::fs::read_to_string(project.join("docs/w4pm_algorithms.md")).expect("algorithms doc");
    assert!(
        doc.contains(
            "Leemans, S.J.J., Fahland, D., & van der Aalst, W.M.P. (2013). \
             Discovering Block-Structured Process Models from Event Logs."
        ),
        "inductive miner citation missing: {doc}"
    );
    assert!(
        doc.contains("Munoz-Gama, J., & Carmona, J. (2010)"),
        "precision citation missing: {doc}"
    );
    for label in [
        "Dfg",
        "Alignments",
        "PredictNextActivity",
        "MonteCarloSimulation",
    ] {
        assert!(doc.contains(label), "doc missing {label}: {doc}");
    }

    // (3) ggen.lock records the pack by name with a blake3 hash.
    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    assert!(
        lock.contains("[packs.wasm4pm-algorithms-pack]"),
        "lock: {lock}"
    );
    assert!(lock.contains("content_hash = \"blake3:"), "lock: {lock}");

    // (4) Static lints pass on the project (pack templates included).
    CliHarness::cargo_bin("ggen")
        .args(["graph", "validate"])
        .current_dir(&project)
        .run()
        .expect("run graph validate")
        .assert_success();

    // (5) Second sync is idempotent: exit 0, outputs byte-identical.
    let before = std::fs::read(&catalog_path).expect("catalog bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    let after = std::fs::read(&catalog_path).expect("catalog bytes after");
    assert_eq!(
        before, after,
        "second sync must leave the catalog unchanged"
    );
    let lock2 = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock 2");
    assert_eq!(
        lock, lock2,
        "ggen.lock must be byte-identical across identical runs"
    );
}
