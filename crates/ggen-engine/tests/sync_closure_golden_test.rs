//! Golden lock for `SyncReport::closure` (input-closure map): every declared
//! input -- manifest, ontology, template, template `shape:` SHACL file, and
//! `[law].gates` `.rq` gate file -- must appear in the closure keyed by its
//! root-relative path, each value equal to the BLAKE3 hex of the on-disk
//! bytes at sync time; undeclared files must never leak in; the same
//! project in a different directory must produce a byte-identical closure.
//!
//! G4 history: a verifier observed shapes missing from the closure on a
//! stale shared target. Two clean-HEAD runs showed 7/0 -- the behavior was
//! correct but had no dedicated lock, so a stale artifact could masquerade
//! as a regression. This file is that lock.
//!
//! Chicago: real filesystem fixture in a fresh `TempDir` per run, real sync
//! pipeline via the library API, hashes recomputed from disk in-test. No
//! mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeMap;
use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "closure-golden"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[law]
gates = ["gates/allow.rq"]
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog ; ex:name "Rex" .
"#;

// ASK (true = violation). The graph holds no ex:Forbidden individual, so
// the gate passes -- but its bytes still join the closure as a governing
// input.
const GATE_ALLOW: &str = r#"
ASK WHERE { ?s a <http://example.org/Forbidden> . }
"#;

// Conforms: ex:rex carries the required ex:name.
const SHAPE_POLICY: &str = r"
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:DogShape a sh:NodeShape ;
    sh:targetClass ex:Dog ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .
";

const TEMPLATE: &str = "---\nto: out.txt\nshape:\n  - shapes/policy.ttl\n---\ndog: rex\n";

/// Files that MUST be present in the closure, keyed by their root-relative
/// display path as `rel_display` renders them.
const DECLARED_INPUTS: [&str; 5] = [
    "ggen.toml",
    "ontology.ttl",
    "templates/dog.tmpl",
    "shapes/policy.ttl",
    "gates/allow.rq",
];

fn scaffold(root: &Path, readme: bool) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(root.join("shapes")).expect("mkdir shapes");
    std::fs::create_dir_all(root.join("gates")).expect("mkdir gates");
    std::fs::write(root.join("gates/allow.rq"), GATE_ALLOW).expect("write gate");
    std::fs::write(root.join("shapes/policy.ttl"), SHAPE_POLICY).expect("write shape");
    std::fs::write(root.join("templates/dog.tmpl"), TEMPLATE).expect("write template");
    if readme {
        std::fs::write(root.join("README.md"), "stray, undeclared").expect("write readme");
    }
}

fn run_sync(root: &Path) -> BTreeMap<String, String> {
    let report = sync(
        root,
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed: fixture is gate- and shape-conformant");
    report.closure
}

fn blake3_of(root: &Path, rel: &str) -> String {
    let bytes = std::fs::read(root.join(rel)).expect("read declared input");
    blake3::hash(&bytes).to_hex().to_string()
}

#[test]
fn closure_contains_every_declared_input_with_live_hashes() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), true);
    let closure = run_sync(dir.path());

    // (1) every declared input is present, keyed by rel path;
    // (2) each value equals the recomputed blake3 of the on-disk bytes.
    for rel in DECLARED_INPUTS {
        let recorded = closure.get(rel).unwrap_or_else(|| {
            panic!("closure must contain declared input `{rel}`; had {closure:?}")
        });
        assert_eq!(
            recorded,
            &blake3_of(dir.path(), rel),
            "closure hash for `{rel}` must equal live blake3 of on-disk bytes"
        );
    }

    // (4) undeclared files never leak into the closure.
    assert!(
        !closure.contains_key("README.md"),
        "undeclared README.md must not appear in closure: {closure:?}"
    );
    for key in closure.keys() {
        assert!(
            !key.ends_with("README.md"),
            "undeclared file leaked into closure as `{key}`"
        );
    }

    // Non-path bookkeeping entries the pipeline legitimately records.
    for meta in [
        "actuator",
        "project",
        "policy:frontmatter-projection-limits",
    ] {
        assert!(
            closure.contains_key(meta),
            "pipeline bookkeeping entry `{meta}` must stay in closure"
        );
    }
}

#[test]
fn closure_hash_is_live_not_cached_tamper_check() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), false);
    let before = run_sync(dir.path());
    let before_shape = before
        .get("shapes/policy.ttl")
        .expect("shape file in closure before tamper")
        .clone();

    // Tamper: mutate the shape file after the first sync, re-run. The new
    // closure value must track the new bytes (live hash, not a cached one).
    let tampered = format!("{SHAPE_POLICY}\n# tampered audit-line\n");
    std::fs::write(dir.path().join("shapes/policy.ttl"), tampered).expect("tamper shape");

    let after = run_sync(dir.path());
    let after_shape = after
        .get("shapes/policy.ttl")
        .expect("shape file in closure after tamper");

    assert_ne!(
        &before_shape, after_shape,
        "closure value must change when the shape file's bytes change"
    );
    assert_eq!(
        after_shape,
        &blake3_of(dir.path(), "shapes/policy.ttl"),
        "post-tamper closure value must equal the new on-disk bytes' blake3"
    );
}

#[test]
fn closure_is_byte_stable_across_project_directories() {
    let a = TempDir::new().expect("tempdir a");
    let b = TempDir::new().expect("tempdir b");
    scaffold(a.path(), false);
    scaffold(b.path(), false);

    let closure_a = run_sync(a.path());
    let closure_b = run_sync(b.path());

    // (5) identical projects in different temp dirs -> identical closure
    // map: no absolute paths, no timestamps, no machine-dependent values.
    assert_eq!(
        closure_a, closure_b,
        "closure must be a pure function of project bytes, not of project location"
    );
}
