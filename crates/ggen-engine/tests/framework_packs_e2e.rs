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
    // Round 3 widened the import (attributes now ontology-declared), so
    // assert on the two load-bearing types rather than the exact brace list.
    assert!(
        events_src.contains("use wasm4pm_compat::ocel::"),
        "{events_src}"
    );
    assert!(events_src.contains("OCELEvent"), "{events_src}");
    assert!(events_src.contains("OCELRelationship"), "{events_src}");

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
    // Round 3 factored the CliHarness plumbing into a sibling generated
    // runtime module mounted via include!; the boundary file now dispatches
    // through run_boundary_spec instead of importing CliHarness directly.
    assert!(
        src.contains("include!(\"chicago_tdd_tools_boundary_runtime.rs\");"),
        "{src}"
    );
    assert!(src.contains("run_boundary_spec"), "{src}");
    assert!(src.contains("fn receiptctl_help_lists_verbs()"), "{src}");
    assert!(src.contains("fn receiptctl_version_emits_name()"), "{src}");
    assert!(
        src.contains("fn receiptctl_unknown_verb_fails_closed()"),
        "{src}"
    );
    // Round 3 replaced the inline CliHarness calls with declarative
    // BoundarySpec structs dispatched through the runtime module; the
    // unknown-verb exit code was also corrected 2 -> 1 (live-verified
    // against real clap-noun-verb behavior earlier this session).
    assert!(src.contains("binary: \"receiptctl\""), "{src}");
    assert!(src.contains("exit_code: 1"), "{src}");
    assert!(src.contains("stdout_needle: Some(\"Usage\")"), "{src}");
    assert!(src.contains("stderr_needle: Some(\"error\")"), "{src}");

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
    // The attribute form changed twice since this test was written: the
    // macro is imported (`use clap_noun_verb_macros::verb;`) and invoked
    // with an explicit noun ("verify", "session") — the noun override that
    // fixed the filename-based misinference bug.
    assert!(
        routes.contains(r#"#[verb("verify", "session")]"#),
        "verb attribute missing: {routes}"
    );
    // The generated signature spans lines (arg list on its own lines), so
    // assert the parts rather than one exact single-line rendering.
    assert!(
        routes.contains("fn session_verify("),
        "route fn missing: {routes}"
    );
    assert!(
        routes.contains("crate::verbs::handlers::user_create_handler("),
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
    // Round 3 moved the dispatch seam into its own generated file
    // (w4pm_cognition_dispatch_handler.rs) so the per-breed wrappers and the
    // hand-completable handler no longer share one file; assert each half
    // where it now lives.
    let handler_path = project.join("src/w4pm_cognition_dispatch_handler.rs");
    assert!(
        handler_path.is_file(),
        "pack must write src/w4pm_cognition_dispatch_handler.rs"
    );
    let handler = std::fs::read_to_string(&handler_path).expect("dispatch handler");
    assert!(
        handler.contains("pub fn dispatch_cognition_run(breed_id: &str, input_json: &str) -> Result<String, String>"),
        "{handler}"
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

// ── Gap-closure additions (L5 push, tracks A1/A2) ──────────────────────────
//
// A1: pack-shipped `gates/*.rq` SPARQL gate queries are evaluated against
// the union graph at sync time and refuse the sync on violation
// (`FM-PACK-013`) — the engine-level "drift is refused by construction"
// mechanism the maturity audits repeatedly named as missing. (Originally
// implemented as pack `shapes.ttl` SHACL gates; replaced by SPARQL gates so
// the identical gate runs under both graph engines. A leftover shapes.ttl
// is a loud `FM-PACK-012` migration refusal, proven below.)
//
// A2: `[templates] aggregate_modules = true` emits one engine-owned
// `src/ggen_pack_mods.rs` aggregator, closing the per-pack lib.rs-collision
// class (FM-WRITE-008 across 11 packs) at the correct layer: one writer.

/// Write a minimal synthetic pack (pack.toml + ontology.ttl + one trivial
/// template) into `dir/<name>/`, returning nothing — the caller wires it via
/// a relative `{ path = "../<name>" }` like the real scaffold does. The
/// ontology content is caller-supplied so tests can make it violate or
/// conform to a caller-supplied `gates/gate.rq` SPARQL gate query.
fn write_synthetic_pack(dir: &Path, name: &str, ontology_ttl: &str, gate_rq: Option<&str>) {
    let pack = dir.join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.0.1\"\ndescription = \"synthetic test pack\"\n"
        ),
    )
    .expect("write pack.toml");
    std::fs::write(pack.join("ontology.ttl"), ontology_ttl).expect("write pack ontology");
    if let Some(gate) = gate_rq {
        std::fs::create_dir_all(pack.join("gates")).expect("mkdir pack gates");
        std::fs::write(pack.join("gates/gate.rq"), gate).expect("write pack gate");
    }
    // One trivial static template so the pack passes FM-PACK-005 (zero
    // templates refused) without depending on any graph content.
    std::fs::write(
        pack.join("templates/marker.txt.tmpl"),
        format!("---\nto: {name}_marker.txt\nforce: true\n---\npack {name} ran\n"),
    )
    .expect("write pack template");
}

/// Minimal consumer wired to the named synthetic packs, with
/// `aggregate_modules` controllable.
fn scaffold_synthetic_consumer(
    dir: &Path, pack_names: &[&str], aggregate_modules: bool,
) -> PathBuf {
    let project = dir.join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    let packs_lines: String = pack_names
        .iter()
        .map(|n| format!("{n} = {{ path = \"../{n}\" }}\n"))
        .collect();
    let aggregate_line = if aggregate_modules {
        "aggregate_modules = true\n"
    } else {
        ""
    };
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"consumer\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_lines}\n\
             [templates]\ndir = \"templates\"\n{aggregate_line}"
        ),
    )
    .expect("write ggen.toml");
    project
}

/// SPARQL gate: the maxCount-1 equivalent of the old SHACL shape — any
/// ex:Thing carrying two distinct ex:val values is a violation row.
const GATE_MAX_ONE_VAL: &str = r#"# MESSAGE: a Thing must carry exactly one ex:val (mirror-drift guard)
PREFIX ex: <http://example.org/gap#>
SELECT ?thing ?v1 ?v2 WHERE {
  ?thing a ex:Thing ; ex:val ?v1 , ?v2 .
  FILTER(STR(?v1) < STR(?v2))
}
ORDER BY ?thing
"#;

#[test]
fn pack_gate_refuses_a_violating_union_graph() {
    let dir = TempDir::new().expect("tempdir");

    // The pack's OWN facts conform (one ex:val), and it ships the gate.
    write_synthetic_pack(
        dir.path(),
        "guarded-pack",
        r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix ex:  <http://example.org/gap#> .
ex:t1 a ex:Thing ; ex:val "from-pack" .
"#,
        Some(GATE_MAX_ONE_VAL),
    );
    let project = scaffold_synthetic_consumer(dir.path(), &["guarded-pack"], false);

    // The CONSUMER's project ontology asserts a second ex:val on the same
    // subject — exactly the cross-source mirror-drift shape (a multi-valued
    // property arising only in the union) that previously surfaced as a
    // downstream rustc duplicate-variant error, or not at all.
    std::fs::write(
        project.join("ontology.ttl"),
        r#"
@prefix ex: <http://example.org/gap#> .
ex:t1 ex:val "diverged-in-consumer" .
"#,
    )
    .expect("write violating consumer ontology");

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_failure();
    output.assert_stderr_contains("FM-PACK-013");
    output.assert_stderr_contains("guarded-pack");
    // And the refusal happened BEFORE any write: no marker file on disk.
    assert!(
        !project.join("guarded-pack_marker.txt").exists(),
        "a refused sync must not have written any template output"
    );

    // Remove the divergent consumer fact -> the same project syncs clean.
    std::fs::write(project.join("ontology.ttl"), "").expect("clear consumer ontology");
    std::fs::remove_file(project.join("ggen.lock")).ok();
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync 2")
        .assert_success();
    assert!(project.join("guarded-pack_marker.txt").is_file());
}

/// A pack still shipping the legacy SHACL gate file (`shapes.ttl`) is a
/// loud, typed `FM-PACK-012` migration refusal — a file that used to be law
/// must never be silently ignored — and nothing is written.
#[test]
fn legacy_pack_shapes_ttl_is_refused_loudly() {
    let dir = TempDir::new().expect("tempdir");
    write_synthetic_pack(
        dir.path(),
        "legacy-pack",
        "@prefix ex: <http://example.org/gap#> .\n",
        None,
    );
    // The legacy artifact: an (even empty) shapes.ttl next to ontology.ttl.
    std::fs::write(
        dir.path().join("legacy-pack/shapes.ttl"),
        "@prefix sh: <http://www.w3.org/ns/shacl#> .\n",
    )
    .expect("write legacy shapes.ttl");
    let project = scaffold_synthetic_consumer(dir.path(), &["legacy-pack"], false);

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync");
    output.assert_failure();
    output.assert_stderr_contains("FM-PACK-012");
    output.assert_stderr_contains("legacy-pack");
    output.assert_stderr_contains("no longer supported");
    output.assert_stderr_contains("gates/*.rq");
    assert!(
        !project.join("legacy-pack_marker.txt").exists(),
        "a refused sync must not have written any template output"
    );

    // Deleting the legacy file clears the refusal: the same project syncs.
    std::fs::remove_file(dir.path().join("legacy-pack/shapes.ttl")).expect("rm shapes.ttl");
    std::fs::remove_file(project.join("ggen.lock")).ok();
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync 2")
        .assert_success();
    assert!(project.join("legacy-pack_marker.txt").is_file());
}

#[test]
fn aggregate_modules_emits_one_engine_owned_aggregator() {
    let dir = TempDir::new().expect("tempdir");

    // Two packs, each generating its own src/*.rs module — the exact shape
    // that used to force 11 packs into hand-rolled per-pack lib-wiring
    // templates (and an FM-WRITE-008 collision when two targeted lib.rs).
    write_synthetic_pack(
        dir.path(),
        "pack-alpha",
        "@prefix ex: <http://example.org/gap#> .\n",
        None,
    );
    write_synthetic_pack(
        dir.path(),
        "pack-beta",
        "@prefix ex: <http://example.org/gap#> .\n",
        None,
    );
    for (pack, module) in [
        ("pack-alpha", "alpha_catalog"),
        ("pack-beta", "beta_catalog"),
    ] {
        std::fs::write(
            dir.path()
                .join(pack)
                .join("templates")
                .join(format!("{module}.rs.tmpl")),
            format!(
                "---\nto: src/{module}.rs\nforce: true\n---\npub const {}: &str = \"{module}\";\n",
                module.to_uppercase()
            ),
        )
        .expect("write module template");
    }

    let project = scaffold_synthetic_consumer(dir.path(), &["pack-alpha", "pack-beta"], true);
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    // One engine-owned aggregator mounting both packs' modules, sorted.
    let aggregator_path = project.join("src/ggen_pack_mods.rs");
    let aggregator = std::fs::read_to_string(&aggregator_path).expect("aggregator");
    assert!(
        aggregator.contains("#[path = \"alpha_catalog.rs\"]\npub mod alpha_catalog;"),
        "{aggregator}"
    );
    assert!(
        aggregator.contains("#[path = \"beta_catalog.rs\"]\npub mod beta_catalog;"),
        "{aggregator}"
    );
    assert!(
        aggregator.find("alpha_catalog").expect("alpha present")
            < aggregator.find("beta_catalog").expect("beta present"),
        "aggregator entries must be deterministically sorted: {aggregator}"
    );
    // The aggregator must not mount itself.
    assert!(
        !aggregator.contains("ggen_pack_mods.rs\"]"),
        "aggregator must not self-mount: {aggregator}"
    );

    // Idempotent: second sync leaves it byte-identical.
    let before = std::fs::read(&aggregator_path).expect("bytes");
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("second sync")
        .assert_success();
    assert_eq!(before, std::fs::read(&aggregator_path).expect("bytes 2"));
}

#[test]
fn aggregate_modules_off_by_default_emits_no_aggregator() {
    let dir = TempDir::new().expect("tempdir");
    write_synthetic_pack(
        dir.path(),
        "pack-alpha",
        "@prefix ex: <http://example.org/gap#> .\n",
        None,
    );
    std::fs::write(
        dir.path()
            .join("pack-alpha/templates/alpha_catalog.rs.tmpl"),
        "---\nto: src/alpha_catalog.rs\nforce: true\n---\npub const A: &str = \"a\";\n",
    )
    .expect("write module template");

    let project = scaffold_synthetic_consumer(dir.path(), &["pack-alpha"], false);
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(&project)
        .run()
        .expect("run sync")
        .assert_success();

    assert!(
        project.join("src/alpha_catalog.rs").is_file(),
        "module itself still generated"
    );
    assert!(
        !project.join("src/ggen_pack_mods.rs").exists(),
        "no aggregator without the opt-in flag (existing consumers unchanged)"
    );
}
