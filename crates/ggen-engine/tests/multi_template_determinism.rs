//! C2 — Multi-template determinism (Chicago TDD, real FS + oxigraph + Tera).
//!
//! Three templates with disjoint outputs, deliberately created on disk in
//! non-lexicographic order, must produce a stable written ordering across
//! runs, byte-identical receipt payloads across two fresh syncs in separate
//! TempDirs, and a fully-unchanged second sync.

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "multi"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob   ex:name "bob" .
ex:carol ex:name "carol" .
"#;

fn template(to: &str) -> String {
    format!(
        "---\nto: {to}\nsparql:\n  people: SELECT ?name WHERE {{ ?s <http://example.org/name> ?name }} ORDER BY ?name\n---\n{{% for row in results %}}{{{{ row.name }}}}\n{{% endfor %}}"
    )
}

/// Scaffold three templates. `creation_order` varies the on-disk creation
/// order (and thus any directory-listing order) between fixtures.
fn scaffold(root: &Path, creation_order: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    for name in creation_order {
        std::fs::write(
            root.join("templates").join(format!("{name}.tmpl")),
            template(&format!("out/{name}.txt")),
        )
        .expect("write template");
    }
}

fn read_receipt(root: &Path) -> SyncReceipt {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("receipt exists");
    serde_json::from_str(&raw).expect("receipt parses")
}

#[test]
fn written_ordering_is_stable_and_lexicographic_regardless_of_creation_order() {
    // Fixture 1: created in lexicographic order; fixture 2: reversed.
    let d1 = TempDir::new().expect("tempdir");
    let d2 = TempDir::new().expect("tempdir");
    scaffold(d1.path(), &["alpha", "beta", "gamma"]);
    scaffold(d2.path(), &["gamma", "beta", "alpha"]);

    let r1 = sync(
        d1.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 1");
    let r2 = sync(
        d2.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 2");

    let expected: Vec<PathBuf> = ["out/alpha.txt", "out/beta.txt", "out/gamma.txt"]
        .iter()
        .map(PathBuf::from)
        .collect();
    assert_eq!(r1.written, expected, "run 1 ordering");
    assert_eq!(
        r2.written, expected,
        "run 2 ordering must not depend on creation order"
    );
}

#[test]
fn receipt_payload_bytes_identical_across_fresh_syncs_of_identical_input() {
    let d1 = TempDir::new().expect("tempdir");
    let d2 = TempDir::new().expect("tempdir");
    scaffold(d1.path(), &["alpha", "beta", "gamma"]);
    scaffold(d2.path(), &["gamma", "alpha", "beta"]);

    // Receipt signing (keys.rs, T063) generates a fresh per-project ed25519
    // key when none exists, which makes `signature_hex` — and therefore the
    // whole receipt file — differ across fresh project roots. Pre-seed both
    // projects with the same key via the documented `.ggen/keys/signing.key`
    // path so the byte-identity assertion exercises everything *except* key
    // provenance. (This assertion predates signing and was only ever
    // satisfiable with a shared key.)
    const SHARED_SEED_HEX: &str =
        "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60";
    for d in [d1.path(), d2.path()] {
        let keys_dir = d.join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir keys");
        std::fs::write(keys_dir.join("signing.key"), SHARED_SEED_HEX).expect("seed signing key");
    }

    sync(
        d1.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 1");
    sync(
        d2.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync 2");

    let p1 = serde_json::to_vec(&read_receipt(d1.path()).payload).expect("payload 1 bytes");
    let p2 = serde_json::to_vec(&read_receipt(d2.path()).payload).expect("payload 2 bytes");
    assert_eq!(
        p1, p2,
        "receipt payload bytes must be identical across fresh syncs"
    );

    // ts_ns is pinned to 0, so the whole chained record is identical too.
    let raw1 = std::fs::read(d1.path().join(RECEIPT_REL_PATH)).expect("receipt 1");
    let raw2 = std::fs::read(d2.path().join(RECEIPT_REL_PATH)).expect("receipt 2");
    assert_eq!(raw1, raw2, "entire receipt file must be byte-identical");
}

#[test]
fn second_sync_of_multi_template_project_is_fully_unchanged() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["gamma", "alpha", "beta"]);

    let first = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first sync");
    assert_eq!(first.written.len(), 3);

    let second = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second sync");
    assert!(
        second.written.is_empty(),
        "second sync wrote: {:?}",
        second.written
    );
    assert_eq!(second.skipped.len(), 3);
    for (path, reason) in &second.skipped {
        assert!(reason.contains("unchanged"), "{}: {reason}", path.display());
    }
    assert_eq!(second.graph_hash_hex, first.graph_hash_hex);
}
