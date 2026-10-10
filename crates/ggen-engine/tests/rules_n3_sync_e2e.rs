//! Chicago-TDD end-to-end tests for the declarative manifest's optional
//! `[rules]` section (v26.10.10 §4.1: `rules.n3` / `rules.datalog`),
//! wired at `sync::sync`'s Stage-0 `DeclarativeRules` dispatch arm:
//!
//! 1. `[rules].n3` forward-chaining derivation is visible to a
//!    `[[generation.rules]]` SPARQL query (inference actually ran).
//! 2. A fired `=> false` denial fuse in a `[rules].n3` file refuses the
//!    sync (typed FM-LAW refusal naming the fuse), never a warning.
//! 3. A `[rules].datalog` entry is a typed UNSUPPORTED refusal.
//! 4. No `[rules]` section (or an empty one) = byte-identical output to
//!    today's behavior — zero drift.
//!
//! Real filesystem (`tempfile::TempDir`), real N3 reasoner, real SPARQL,
//! real Tera render, real write — no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

/// Minimal valid declarative-rules manifest with `extra` spliced in before
/// the `[[generation.rules]]` table.
fn write_manifest(root: &Path, extra: &str) {
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"rules-n3-e2e\"\nversion = \"1.0.0\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             {extra}\n\
             [[generation.rules]]\n\
             name = \"animals\"\n\
             query = {{ inline = \"SELECT ?s WHERE {{ ?s a <http://example.org/Animal> }} ORDER BY ?s\" }}\n\
             template = {{ inline = \"{{% for row in results %}}{{{{ row.s }}}}\\n{{% endfor %}}\" }}\n\
             output_file = \"out/animals.txt\"\n"
        ),
    );
}

/// `ex:rex a ex:Dog` — note `ex:Animal` appears NOWHERE in the ontology,
/// so any `Animal` fact in an output can only come from N3 inference.
const ONTOLOGY_REX_DOG: &str = r"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog .
";

// ---------------------------------------------------------------------------
// 1. `[rules].n3` derivation is visible to the generation-rule query.
// ---------------------------------------------------------------------------

/// THE load-bearing proof for `[rules].n3` wiring: `ex:rex a ex:Animal`
/// exists nowhere in the project — it can only appear if the declared N3
/// rule was resolved, forward-chained, and its derived facts folded into
/// the graph *before* the generation rule's SELECT ran. A decorative
/// implementation (paths parsed but rules never executed) renders an empty
/// result instead.
#[test]
fn rules_n3_derivation_is_visible_to_generation_rule_query() {
    const DERIVE_N3: &str =
        "@prefix ex: <http://example.org/>. {?s a ex:Dog} => {?s a ex:Animal} .";
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[rules]\nn3 = [\"rules/dogs-are-animals.n3\"]\ndatalog = []\n",
    );
    write_ontology(dir.path());
    write(dir.path(), "rules/dogs-are-animals.n3", DERIVE_N3);

    let report = sync(dir.path(), SyncOptions::default()).expect("[rules].n3 sync must succeed");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/animals.txt")]
    );
    let content = std::fs::read_to_string(dir.path().join("out/animals.txt")).expect("read output");
    assert_eq!(
        content, "http://example.org/rex\n",
        "generation rule must see the N3-derived ex:Animal fact, not an empty graph"
    );
}

// ---------------------------------------------------------------------------
// 2. A fired `=> false` denial fuse refuses the sync.
// ---------------------------------------------------------------------------

/// A `[rules].n3` denial fuse (`{ body } => false.`) that fires is a typed
/// FM-LAW refusal naming the fuse — a fired fuse is a refusal, never a
/// warning — and nothing is written.
#[test]
fn rules_n3_denial_fuse_refuses_sync() {
    const DENIAL_N3: &str = "@prefix ex: <http://example.org/>. {?s a ex:Dog} => false.";
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[rules]\nn3 = [\"rules/no-dogs.n3\"]\ndatalog = []\n",
    );
    write_ontology(dir.path());
    write(dir.path(), "rules/no-dogs.n3", DENIAL_N3);

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("asserted dog under a [rules].n3 denial fuse must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-LAW-016"), "{msg}");
    assert!(
        msg.contains("DENIED"),
        "refusal must name the fuse line: {msg}"
    );
    assert!(
        !dir.path().join("out/animals.txt").exists(),
        "refused run must write nothing"
    );
}

// ---------------------------------------------------------------------------
// 3. `[rules].datalog` is a typed UNSUPPORTED refusal.
// ---------------------------------------------------------------------------

/// A `.datalog` entry under `[rules]` is refused loudly with the UNSUPPORTED
/// text surfaced as a sync error — never a silent skip, never a
/// hand-rolled Datalog parser.
#[test]
fn rules_datalog_is_typed_unsupported_refusal() {
    let dir = TempDir::new().expect("tempdir");
    write_manifest(
        dir.path(),
        "[rules]\nn3 = []\ndatalog = [\"rules/legacy.datalog\"]\n",
    );
    write_ontology(dir.path());
    write(dir.path(), "rules/legacy.datalog", "dog(x) <- animal(x).");

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("a [rules].datalog entry must be a typed refusal");
    let msg = err.to_string();
    assert!(msg.contains("UNSUPPORTED"), "{msg}");
    assert!(msg.contains("legacy.datalog"), "{msg}");
    assert!(
        !dir.path().join("out/animals.txt").exists(),
        "refused run must write nothing"
    );
}

// ---------------------------------------------------------------------------
// 4. Zero drift: absent or empty `[rules]` = exact current behavior.
// ---------------------------------------------------------------------------

/// A project with no `[rules]` section and a project with an explicitly
/// empty `[rules]` section produce byte-identical output files — proving
/// the wiring is purely additive. The generation rule selects `ex:Dog` (a
/// plain ontology fact) so both runs render non-empty, comparable output.
#[test]
fn absent_or_empty_rules_section_is_byte_identical() {
    let manifest_for = |root: &Path, extra: &str| {
        write(
            root,
            "ggen.toml",
            &format!(
                "[project]\nname = \"rules-n3-e2e\"\nversion = \"1.0.0\"\n\n\
                 [ontology]\nsource = \"ontology.ttl\"\n\n\
                 {extra}\n\
                 [[generation.rules]]\n\
                 name = \"dogs\"\n\
                 query = {{ inline = \"SELECT ?s WHERE {{ ?s a <http://example.org/Dog> }} ORDER BY ?s\" }}\n\
                 template = {{ inline = \"{{% for row in results %}}{{{{ row.s }}}}\\n{{% endfor %}}\" }}\n\
                 output_file = \"out/dogs.txt\"\n"
            ),
        );
        write(root, "ontology.ttl", ONTOLOGY_REX_DOG);
    };

    let without = TempDir::new().expect("tempdir");
    manifest_for(without.path(), "");
    let with_empty = TempDir::new().expect("tempdir");
    manifest_for(with_empty.path(), "[rules]\nn3 = []\ndatalog = []\n");

    sync(without.path(), SyncOptions::default()).expect("no-[rules] sync");
    sync(with_empty.path(), SyncOptions::default()).expect("empty-[rules] sync");

    let a = std::fs::read_to_string(without.path().join("out/dogs.txt")).expect("read A");
    let b = std::fs::read_to_string(with_empty.path().join("out/dogs.txt")).expect("read B");
    assert_eq!(a, "http://example.org/rex\n", "baseline must be non-empty");
    assert_eq!(
        a, b,
        "an empty [rules] section must not change output bytes"
    );
}

fn write_ontology(root: &Path) {
    write(root, "ontology.ttl", ONTOLOGY_REX_DOG);
}
