//! Chicago-TDD end-to-end tests for the declarative `[[generation.rules]]`
//! sync path (`crate::generation_rules`, specs/014-ggen-core-replacement T070):
//! real filesystem (`tempfile::TempDir`), a real oxigraph-backed store, real
//! SPARQL evaluation, real Tera rendering — no mocks — driven exclusively
//! through the public [`ggen_engine::sync::sync`] entry point, the same one
//! `tests/sync_e2e.rs`/`tests/graphlaw_e2e.rs` exercise for the pre-existing
//! frontmatter path.
//!
//! The load-bearing test is
//! [`merge_mode_preserves_hand_edits_across_two_syncs_with_changed_query_data`]:
//! it proves the ported marker-merge algorithm is actually wired into the
//! write decision, not decoration — a naive implementation that always
//! overwrites would destroy the hand-written section; one that never
//! updates would leave stale generated content. Only a correct
//! decide-then-merge implementation passes.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const ONTOLOGY_ALICE_BOB: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
"#;

fn write_manifest(root: &Path, body: &str) {
    let manifest = format!(
        "[project]\nname = \"gendemo\"\nversion = \"0.1.0\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n{body}"
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("write ggen.toml");
}

fn write_ontology(root: &Path, ttl: &str) {
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology.ttl");
}

// ---------------------------------------------------------------------------
// 1. QuerySource::File + TemplateSource::File, static output_file
// ---------------------------------------------------------------------------

/// THE core proof this module exists to deliver: a rule's `query` file is
/// read and actually executed against the real graph, its `template` file
/// is read and actually rendered with the real row data, and the result
/// lands at `output_file` via `sync()` — not a passthrough of any of the
/// three. A decorative implementation (e.g. one that renders the template
/// with an empty context, or never reads the query file) would produce an
/// empty or literal-`{{...}}` file instead of the real names below.
#[test]
fn static_rule_with_file_query_and_file_template_renders_real_query_results() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\nquery = { file = \"queries/names.rq\" }\ntemplate = { file = \"templates/names.tmpl\" }\noutput_file = \"out/names.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);
    std::fs::create_dir_all(dir.path().join("queries")).expect("mkdir queries");
    std::fs::write(
        dir.path().join("queries/names.rq"),
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name",
    )
    .expect("write query file");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(
        dir.path().join("templates/names.tmpl"),
        "{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .expect("write template file");

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    assert!(
        report.skipped.is_empty(),
        "unexpected skips: {:?}",
        report.skipped
    );
    let content = std::fs::read_to_string(dir.path().join("out/names.txt")).expect("read output");
    assert_eq!(
        content, "alice\nbob\n",
        "rendered content must contain the REAL query results, not a stub"
    );

    // The query and template files must be bound into the receipt's input
    // closure — proof they were actually resolved from disk, not inlined
    // or skipped.
    assert_eq!(
        report.closure.get("queries/names.rq").map(String::as_str),
        Some(
            blake3::hash(
                b"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name"
            )
            .to_hex()
            .as_str()
        ),
    );
    assert!(report.closure.contains_key("templates/names.tmpl"));
    assert!(report.closure.contains_key("ontology.ttl"));

    // The receipt on disk binds the real output bytes.
    let raw = std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt exists");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("receipt parses");
    let file_bytes = std::fs::read(dir.path().join("out/names.txt")).expect("output bytes");
    assert_eq!(
        receipt
            .payload
            .outputs
            .get("out/names.txt")
            .map(String::as_str),
        Some(blake3::hash(&file_bytes).to_hex().as_str()),
        "receipt must bind the real rendered output bytes"
    );
}

// ---------------------------------------------------------------------------
// 2. QuerySource::Inline + TemplateSource::Inline, per-row output_file
// ---------------------------------------------------------------------------

/// A templated `output_file` (`.contains("{{")`) must fan out into one
/// write per SPARQL row, each rendered against that row's own data — the
/// per-row branch in `generation_rules::run`. A decorative implementation
/// (e.g. always taking the static branch) would produce exactly one file
/// with the whole `results` array dumped into it instead of two
/// individually-named, individually-rendered files.
#[test]
fn per_row_rule_with_inline_query_and_template_creates_one_file_per_row() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"greetings\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { inline = \"Hello, {{ row.name }}!\" }\noutput_file = \"out/{{ row.name }}.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert_eq!(
        report.written,
        vec![
            std::path::PathBuf::from("out/alice.txt"),
            std::path::PathBuf::from("out/bob.txt"),
        ],
        "one write per row, in query row order"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/alice.txt")).expect("alice"),
        "Hello, alice!"
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/bob.txt")).expect("bob"),
        "Hello, bob!"
    );
}

// ---------------------------------------------------------------------------
// 3. `when:` ASK guard and `skip_empty` — real skips, not silent no-ops
// ---------------------------------------------------------------------------

/// A `when:` ASK that evaluates false against the real graph, and a
/// `skip_empty` rule whose real SELECT returns zero rows, must both be
/// recorded as `Skipped` (with the reason in `decisions`) rather than
/// either erroring or silently vanishing from the report.
#[test]
fn when_guard_false_and_skip_empty_produce_documented_skips_not_writes() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        concat!(
            "[[generation.rules]]\n",
            "name = \"gated\"\n",
            "when = \"ASK { ?s <http://example.org/name> \\\"charlie\\\" }\"\n",
            "query = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name }\" }\n",
            "template = { inline = \"unreachable\" }\n",
            "output_file = \"out/gated.txt\"\n",
            "\n",
            "[[generation.rules]]\n",
            "name = \"empty\"\n",
            "skip_empty = true\n",
            "query = { inline = \"SELECT ?x WHERE { ?x <http://example.org/nope> ?y }\" }\n",
            "template = { inline = \"unreachable\" }\n",
            "output_file = \"out/empty.txt\"\n",
        ),
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert!(
        report.written.is_empty(),
        "neither rule should write: {:?}",
        report.written
    );
    assert_eq!(report.skipped.len(), 2, "{:?}", report.skipped);
    assert!(!dir.path().join("out/gated.txt").exists());
    assert!(!dir.path().join("out/empty.txt").exists());

    let gated_decision = report
        .decisions
        .get("out/gated.txt")
        .expect("decision recorded");
    assert!(
        gated_decision.contains("when guard false"),
        "{gated_decision}"
    );
    let empty_decision = report
        .decisions
        .get("out/empty.txt")
        .expect("decision recorded");
    assert!(empty_decision.contains("skip_empty"), "{empty_decision}");
}

// ---------------------------------------------------------------------------
// 4. GenerationMode::Merge — THE decorative-vs-real proof
// ---------------------------------------------------------------------------

/// The load-bearing proof for `GenerationMode::Merge`: sync once (creates
/// fresh markers around the generated content), hand-edit the manual
/// section exactly as a developer would, change the ontology so the next
/// sync's generated content differs, then sync again. A correct
/// implementation updates only the generated section and preserves the
/// hand-written one byte-for-byte; a naive "always overwrite" or "always
/// skip if exists" implementation fails this — either destroying the
/// manual edit or never picking up the new generated content.
#[test]
fn merge_mode_preserves_hand_edits_across_two_syncs_with_changed_query_data() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"versioned\"\nmode = \"Merge\"\nquery = { inline = \"SELECT ?v WHERE { ?s <http://example.org/version> ?v }\" }\ntemplate = { inline = \"{% for row in results %}fn version() -> &'static str { \\\"{{ row.v }}\\\" }\\n{% endfor %}\" }\noutput_file = \"src/generated.rs\"\n",
    );
    write_ontology(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:proj ex:version \"v1\" .\n",
    );

    // First sync: no existing file, so `merge_sections` wraps the generated
    // body in fresh markers with a placeholder manual section.
    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("src/generated.rs")]
    );
    let after_first =
        std::fs::read_to_string(dir.path().join("src/generated.rs")).expect("read after first");
    assert!(after_first.contains("<<<<<<< GENERATED"));
    assert!(after_first.contains("fn version() -> &'static str { \"v1\" }"));
    assert!(after_first.contains(">>>>>>> MANUAL"));

    // Hand-edit exactly the manual section, as a developer would.
    let hand_edited = after_first.replace(
        "// Add your manual code here",
        "fn hand_written_helper() -> i32 { 42 }",
    );
    assert_ne!(
        hand_edited, after_first,
        "the placeholder must have been present to replace"
    );
    std::fs::write(dir.path().join("src/generated.rs"), &hand_edited).expect("write hand edit");

    // Change the ontology so the next sync's generated content differs.
    write_ontology(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:proj ex:version \"v2\" .\n",
    );

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert_eq!(
        second.written,
        vec![std::path::PathBuf::from("src/generated.rs")],
        "content changed (v1 -> v2), so this must be a real write, not skipped-unchanged"
    );
    let after_second =
        std::fs::read_to_string(dir.path().join("src/generated.rs")).expect("read after second");

    assert!(
        after_second.contains("fn version() -> &'static str { \"v2\" }"),
        "generated section must be updated to the new query result:\n{after_second}"
    );
    assert!(
        !after_second.contains("\"v1\""),
        "stale generated content must not survive:\n{after_second}"
    );
    assert!(
        after_second.contains("fn hand_written_helper() -> i32 { 42 }"),
        "hand-written manual section must survive byte-for-byte across the second sync:\n{after_second}"
    );
}

// ---------------------------------------------------------------------------
// 5. GenerationMode::Create — bootstrap-once semantics
// ---------------------------------------------------------------------------

/// `mode = "Create"` (the default) writes once and then silently leaves a
/// hand-completed file alone on every subsequent sync — the documented
/// bootstrap-scaffold semantics (root `CLAUDE.md`'s "mode=Create Semantics").
#[test]
fn create_mode_writes_once_then_leaves_hand_edits_alone() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"scaffold\"\nmode = \"Create\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name LIMIT 1\" }\ntemplate = { inline = \"{% for row in results %}// scaffold for {{ row.name }}\\n{% endfor %}\" }\noutput_file = \"src/scaffold.rs\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("src/scaffold.rs")]
    );

    // Hand-complete the scaffold.
    std::fs::write(
        dir.path().join("src/scaffold.rs"),
        "// hand-completed, do not touch\n",
    )
    .expect("hand-complete");

    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(
        second.written.is_empty(),
        "mode=Create must never rewrite an existing target: {:?}",
        second.written
    );
    assert_eq!(second.skipped.len(), 1);
    assert!(
        second.skipped[0].1.contains("mode=create"),
        "{}",
        second.skipped[0].1
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("src/scaffold.rs")).expect("read"),
        "// hand-completed, do not touch\n",
        "hand-completed content must be untouched"
    );
}

// ---------------------------------------------------------------------------
// 6. GenerationMode::Overwrite — always replaces on real content change
// ---------------------------------------------------------------------------

#[test]
fn overwrite_mode_replaces_content_and_skips_when_unchanged() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"always\"\nmode = \"Overwrite\"\nquery = { inline = \"SELECT ?v WHERE { ?s <http://example.org/version> ?v }\" }\ntemplate = { inline = \"{% for row in results %}{{ row.v }}{% endfor %}\" }\noutput_file = \"out/version.txt\"\n",
    );
    write_ontology(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:proj ex:version \"v1\" .\n",
    );

    let first = sync(dir.path(), SyncOptions::default()).expect("first sync");
    assert_eq!(
        first.written,
        vec![std::path::PathBuf::from("out/version.txt")]
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/version.txt")).expect("v1"),
        "v1"
    );

    // Second sync, unchanged ontology: content is identical -> Skipped.
    let second = sync(dir.path(), SyncOptions::default()).expect("second sync");
    assert!(second.written.is_empty(), "{:?}", second.written);
    assert!(
        second.skipped[0].1.contains("unchanged"),
        "{}",
        second.skipped[0].1
    );

    // Third sync, changed ontology: content differs -> Written (overwritten).
    write_ontology(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:proj ex:version \"v2\" .\n",
    );
    let third = sync(dir.path(), SyncOptions::default()).expect("third sync");
    assert_eq!(
        third.written,
        vec![std::path::PathBuf::from("out/version.txt")]
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("out/version.txt")).expect("v2"),
        "v2",
        "overwrite mode must replace stale content"
    );
}

// ---------------------------------------------------------------------------
// 7. Deliberately-deferred variants are a typed, loud refusal
// ---------------------------------------------------------------------------

/// `QuerySource::Pack` has no resolution destination yet (see the module
/// doc comment's documented scope cut) — it must fail with a named,
/// specific error, never a silent skip or a decorative empty output.
#[test]
fn unimplemented_query_source_pack_is_a_typed_refusal_not_a_silent_skip() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"needs_pack\"\nquery = { pack = \"some-pack\", output = \"queries\", file = \"x.rq\" }\ntemplate = { inline = \"unreachable\" }\noutput_file = \"out/x.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse, not silently skip");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-006"), "{msg}");
    assert!(msg.contains("some-pack"), "{msg}");
    assert!(msg.contains("not implemented"), "{msg}");
    assert!(!dir.path().join("out/x.txt").exists());
}

/// `TemplateSource::Git` — same typed-refusal contract as `QuerySource::Pack`.
#[test]
fn unimplemented_template_source_git_is_a_typed_refusal() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"needs_git\"\nquery = { inline = \"SELECT ?s WHERE { ?s ?p ?o } LIMIT 1\" }\ntemplate = { git = \"https://example.com/repo.git\", path = \"t.tera\" }\noutput_file = \"out/x.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-007"), "{msg}");
    assert!(msg.contains("not implemented"), "{msg}");
}

// ---------------------------------------------------------------------------
// 8. Duplicate render targets are refused, not last-row-wins
// ---------------------------------------------------------------------------

/// Two rows whose per-row `output_file` collapses onto the same path must
/// refuse the whole run (mirrors `sync_e2e.rs`'s frontmatter-path
/// equivalent) rather than silently letting the second row's write clobber
/// the first's on disk.
#[test]
fn duplicate_render_targets_from_per_row_rule_are_refused() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"collide\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { inline = \"{{ row.name }}\" }\noutput_file = \"out/{{ row.name | length }}.txt\"\n",
    );
    // "anna"/"bert" both have length 4 -> same rendered output_file.
    write_ontology(
        dir.path(),
        "@prefix ex: <http://example.org/> .\nex:a ex:name \"anna\" .\nex:b ex:name \"bert\" .\n",
    );

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-004"), "{msg}");
    assert!(msg.contains("same output"), "{msg}");
    assert!(
        !dir.path().join("out").exists(),
        "refusal must precede any write"
    );
}

// ---------------------------------------------------------------------------
// 9. Receipt chains across two declarative-rules syncs
// ---------------------------------------------------------------------------

/// The declarative-rules path shares `crate::sync::write_receipt` with the
/// frontmatter path (mirrors `graphlaw_e2e.rs`'s
/// `two_runs_same_fixture_same_graph_hash_and_valid_chain`): two
/// consecutive syncs of a `[[generation.rules]]` project produce a valid,
/// linearly-chained receipt history, not two disconnected genesis records.
#[test]
fn two_generation_rules_syncs_chain_receipts() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"names\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\" }\ntemplate = { inline = \"{% for row in results %}{{ row.name }}\\n{% endfor %}\" }\noutput_file = \"out/names.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    sync(dir.path(), SyncOptions::default()).expect("first sync");
    let receipt1: SyncReceipt = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt 1"),
    )
    .expect("parse receipt 1");

    // Second sync of the unchanged project: output is identical, so no file
    // is (re)written, but the receipt is still emitted and must chain.
    sync(dir.path(), SyncOptions::default()).expect("second sync");
    let receipt2: SyncReceipt = serde_json::from_str(
        &std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("receipt 2"),
    )
    .expect("parse receipt 2");

    assert_eq!(
        receipt2.record.prev_chain_hash_hex, receipt1.record.chain_hash_hex,
        "second receipt must chain onto the first"
    );
    let recomputed = receipt2.record.recompute_chain_hash().expect("recompute");
    let hex: String = recomputed.iter().map(|b| format!("{b:02x}")).collect();
    assert_eq!(
        hex, receipt2.record.chain_hash_hex,
        "chain head must verify"
    );
}

// ---------------------------------------------------------------------------
// 10. Empty rendered output is refused, not silently written
// ---------------------------------------------------------------------------

/// A rule whose real SPARQL query returns rows (so `skip_empty` does not
/// apply) but whose template renders to an empty string must be refused —
/// proof [`validate_rendered_body`]-equivalent output validation actually
/// runs on this path, matching ggen-core's E0004 check.
#[test]
fn empty_rendered_body_is_refused_not_silently_written() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[generation.rules]]\nname = \"blank\"\nquery = { inline = \"SELECT ?name WHERE { ?s <http://example.org/name> ?name } LIMIT 1\" }\ntemplate = { inline = \"\" }\noutput_file = \"out/blank.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_ALICE_BOB);

    let err = sync(dir.path(), SyncOptions::default()).expect_err("must refuse empty output");
    let msg = err.to_string();
    assert!(msg.contains("FM-GEN-011"), "{msg}");
    assert!(!dir.path().join("out/blank.txt").exists());
}

// ---------------------------------------------------------------------------
// 11. `[[inference.rules]]` — CONSTRUCT materialization visible to
//     `[[generation.rules]]`, and the `when:` ASK guard
// ---------------------------------------------------------------------------

const ONTOLOGY_REX_DOG: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog .
"#;

/// THE load-bearing proof for inference-rules wiring: `ex:rex a ex:Animal`
/// exists nowhere in the ontology — it can only appear if the
/// `[[inference.rules]]` CONSTRUCT actually ran and its derived triple was
/// folded back into the graph *before* the generation rule's SELECT saw it.
/// A decorative implementation (inference parsed but never executed, the gap
/// this test closes) would render an empty result instead.
#[test]
fn inference_rule_construct_is_visible_to_generation_rule_query() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[inference.rules]]\nname = \"dogs_are_animals\"\nconstruct = \"CONSTRUCT { ?s a <http://example.org/Animal> } WHERE { ?s a <http://example.org/Dog> }\"\norder = 1\n\n[[generation.rules]]\nname = \"animals\"\nquery = { inline = \"SELECT ?s WHERE { ?s a <http://example.org/Animal> }\" }\ntemplate = { inline = \"{% for row in results %}{{ row.s }}\\n{% endfor %}\" }\noutput_file = \"out/animals.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_REX_DOG);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/animals.txt")]
    );
    let content = std::fs::read_to_string(dir.path().join("out/animals.txt")).expect("read output");
    assert_eq!(
        content, "http://example.org/rex\n",
        "generation rule must see the inference-derived fact, not an empty graph"
    );
}

/// An inference rule whose `when:` ASK guard is false must not materialize
/// its CONSTRUCT — proof the guard is actually evaluated, not ignored.
#[test]
fn inference_rule_when_guard_false_skips_construct() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[[inference.rules]]\nname = \"dogs_are_animals\"\nconstruct = \"CONSTRUCT { ?s a <http://example.org/Animal> } WHERE { ?s a <http://example.org/Dog> }\"\norder = 1\nwhen = \"ASK { ?s a <http://example.org/Cat> }\"\n\n[[generation.rules]]\nname = \"animals\"\nquery = { inline = \"SELECT ?s WHERE { ?s a <http://example.org/Animal> }\" }\ntemplate = { inline = \"{% for row in results %}{{ row.s }}\\n{% endfor %}\" }\noutput_file = \"out/animals.txt\"\nskip_empty = true\n",
    );
    write_ontology(dir.path(), ONTOLOGY_REX_DOG);

    let report = sync(dir.path(), SyncOptions::default()).expect("sync");
    assert!(
        report.written.is_empty(),
        "when: guard is false (no ex:Cat) -- inference must not run, so the generation \
         rule's query sees no ex:Animal facts and skip_empty applies: {:?}",
        report.written
    );
}

// ---------------------------------------------------------------------------
// 12. Law gate — N3 rule denial and SHACL shape violation, run from the
//     declarative-rules path
// ---------------------------------------------------------------------------

/// A violated denial rule (`{ body } => false.`) in `[law].rules` refuses the
/// sync before any file is written — proof the law gate (ported from
/// `crate::sync::sync`'s frontmatter-path stage) actually runs for
/// declarative-rules projects too, not just frontmatter ones.
#[test]
fn law_gate_denial_violation_refuses_declarative_rules_sync() {
    const DENIAL_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => false.";
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[law]\nrules = [\"rules/no-dogs.n3\"]\n\n[[generation.rules]]\nname = \"static\"\nquery = { inline = \"SELECT ?s WHERE { ?s a <http://example.org/Dog> }\" }\ntemplate = { inline = \"static\\n\" }\noutput_file = \"out/static.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_REX_DOG);
    std::fs::create_dir_all(dir.path().join("rules")).expect("mkdir rules");
    std::fs::write(dir.path().join("rules/no-dogs.n3"), DENIAL_N3).expect("write denial rule");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("asserted dog under a no-dogs denial must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-016"), "{msg}");
    assert!(msg.contains("DENIED"), "{msg}");
    assert!(
        !dir.path().join("out/static.txt").exists(),
        "gate must precede writes"
    );
}

/// A `[validation].gates` SPARQL gate whose SELECT returns a row against
/// the ontology refuses the sync, naming the offending node (first-row
/// bindings embedded in the error) — proof the gate stage reads from
/// `validation.gates` (not a duplicated `law.gates` field) and actually
/// evaluates, for the declarative-rules path.
#[test]
fn law_gate_violation_refuses_declarative_rules_sync_naming_offending_node() {
    const GATE: &str = "# MESSAGE: every Dog must carry an ex:license\n\
                        PREFIX ex: <http://example.org/>\n\
                        SELECT ?dog WHERE {\n\
                        \x20 ?dog a ex:Dog .\n\
                        \x20 FILTER NOT EXISTS { ?dog ex:license ?lic }\n\
                        }\n\
                        ORDER BY ?dog\n";
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\ngates = [\"gates/dog.rq\"]\n\n[[generation.rules]]\nname = \"static\"\nquery = { inline = \"SELECT ?s WHERE { ?s a <http://example.org/Dog> }\" }\ntemplate = { inline = \"static\\n\" }\noutput_file = \"out/static.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_REX_DOG);
    std::fs::create_dir_all(dir.path().join("gates")).expect("mkdir gates");
    std::fs::write(dir.path().join("gates/dog.rq"), GATE).expect("write gate");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("unlicensed dog must refuse the SPARQL gate");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-018"), "{msg}");
    assert!(
        msg.contains("rex"),
        "refusal must name the offending node (first-row bindings): {msg}"
    );
    assert!(
        msg.contains("every Dog must carry an ex:license"),
        "refusal must carry the gate's MESSAGE text: {msg}"
    );
    assert!(
        !dir.path().join("out/static.txt").exists(),
        "gate must precede writes"
    );
}

/// A legacy non-empty `[validation].shacl` is a loud, typed FM-LAW-017
/// migration refusal — never silently ignored — and nothing is written.
#[test]
fn legacy_validation_shacl_is_refused_loudly() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[validation]\nshacl = [\"shapes/dog.ttl\"]\n\n[[generation.rules]]\nname = \"static\"\nquery = { inline = \"SELECT ?s WHERE { ?s a <http://example.org/Dog> }\" }\ntemplate = { inline = \"static\\n\" }\noutput_file = \"out/static.txt\"\n",
    );
    write_ontology(dir.path(), ONTOLOGY_REX_DOG);
    std::fs::create_dir_all(dir.path().join("shapes")).expect("mkdir shapes");
    std::fs::write(dir.path().join("shapes/dog.ttl"), "# legacy\n").expect("write shapes");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("a non-empty [validation].shacl must refuse loudly");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-017"), "{msg}");
    assert!(msg.contains("no longer supported"), "{msg}");
    assert!(msg.contains("[validation].gates"), "{msg}");
    assert!(
        !dir.path().join("out/static.txt").exists(),
        "refused run must write nothing"
    );
}
