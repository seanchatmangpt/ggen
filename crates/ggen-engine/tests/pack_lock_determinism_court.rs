//! Pack-lock determinism court (gap-closure lane H06, ggen v26.10.20).
//!
//! KPI under proof: "100% bit-for-bit repeatability of pack.lock manifests
//! across platforms". This court proves on ONE machine (darwin/arm64) what is
//! provable locally — rerun stability, path-independence of the digest
//! portion, dialect-reorder behavior, and receipt content-address binding —
//! and documents, with receipts, the fields that make full byte-for-bit
//! cross-machine equality structurally impossible today.
//!
//! Two distinct lock surfaces exist; both are exercised:
//!
//! 1. `ggen.lock` (ggen-engine, TOML, written by `crate::pack::write_lock`
//!    during every real sync) — sorted, timestamp-free, content-hash pinned.
//! 2. `.ggen/packs.lock` (ggen-marketplace, JSON, `PackLockfile`) — carries
//!    wall-clock `installed_at`/`updated_at`; the court proves this surface is
//!    NOT byte-reproducible across runs and names the exact fields.
//!
//! Chicago discipline: real `ggen_engine::sync::sync` runs over real
//! `tempfile::TempDir` fixture packs (with `[[capabilities.dialects]]` per
//! lane V01's surface), sha256 over real bytes, no mocks.
//!
//! PLATFORM STATUS: cross-platform (linux/windows) equality is UNVERIFIED —
//! no second platform was available to this court. Everything asserted here
//! is same-platform; path-separator sensitivity of `PackRef::Path`
//! `source` strings on Windows is a known-open question, not a proven pass.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use ggen_engine::pack::{content_hash, DialectGlob, Pack};
use ggen_engine::sync::{sync, SyncOptions};
use sha2::{Digest as _, Sha256};
use tempfile::TempDir;

// ── fixture construction ────────────────────────────────────────────────────

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex::encode(h.finalize())
}

/// Dialect declarations, in the order the caller wants them written into
/// `pack.toml`. Reordering is court phase 3's mutation.
fn dialect_toml(entries: &[(&str, &str)]) -> String {
    let mut out = String::new();
    for (glob, dialect) in entries {
        writeln!(out, "[[capabilities.dialects]]").unwrap();
        writeln!(out, "glob = \"{glob}\"").unwrap();
        writeln!(out, "dialect = \"{dialect}\"").unwrap();
    }
    out
}

const BASE_DIALECTS: [(&str, &str); 2] = [("*", "turtle"), ("shapes/*", "turtle")];

/// One fixture pack (path pack, `lock` defaults on) plus a trivial project
/// manifest + template, all inside `root`. Returns nothing; the fixture is
/// the directory tree itself.
fn write_fixture(root: &Path, dialect_entries: &[(&str, &str)]) {
    write(
        root,
        "packs/alpha/pack.toml",
        &format!(
            "[pack]\nname = \"alpha\"\nversion = \"1.0.0\"\n\
             description = \"H06 determinism fixture\"\n\n\
             [capabilities]\ntypes = [\"ex:Thing\"]\n\n\
             {}",
            dialect_toml(dialect_entries)
        ),
    );
    write(
        root,
        "packs/alpha/ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "packs/alpha/shapes/shape.ttl",
        "@prefix ex: <http://example.org/> .\nex:shape a ex:Shape .\n",
    );
    write(
        root,
        "packs/alpha/templates/alpha.tmpl",
        "---\nto: out/alpha.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}ALPHA: {{ row.s }}\n{% endfor %}",
    );
    write(
        root,
        "ggen.toml",
        "[project]\nname = \"h06-determinism-court\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [packs]\nalpha = { path = \"packs/alpha\" }\n\n\
         [templates]\ndir = \"templates\"\n",
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:root a ex:Thing .\n",
    );
    write(
        root,
        "templates/root.tmpl",
        "---\nto: out/root.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}ROOT: {{ row.s }}\n{% endfor %}",
    );
}

fn read_lock(root: &Path) -> String {
    std::fs::read_to_string(root.join("ggen.lock")).expect("ggen.lock exists after sync")
}

/// Parse `ggen.lock` TOML into pack name → (source, `content_hash`).
fn parse_lock(raw: &str) -> BTreeMap<String, (String, String)> {
    let doc: toml::Value = toml::from_str(raw).expect("ggen.lock parses as TOML");
    let packs = doc
        .get("packs")
        .expect("ggen.lock has [packs]")
        .as_table()
        .expect("packs table");
    packs
        .iter()
        .map(|(name, v)| {
            let source = v
                .get("source")
                .and_then(|s| s.as_str())
                .unwrap_or_default()
                .to_string();
            let hash = v
                .get("content_hash")
                .and_then(|s| s.as_str())
                .unwrap_or_default()
                .to_string();
            (name.clone(), (source, hash))
        })
        .collect()
}

// ── court phase 1: rerun stability (same root, two syncs) ───────────────────

/// THE core determinism claim: two syncs over the identical fixture in the
/// identical root produce byte-identical `ggen.lock`. The lockfile writer is
/// documented as timestamp-free and sorted; this proves it on real output.
#[test]
fn phase1_rerun_same_root_ggen_lock_byte_identical() {
    let dir = TempDir::new().unwrap();
    write_fixture(dir.path(), &BASE_DIALECTS);

    sync(dir.path(), SyncOptions::default()).expect("first sync succeeds");
    let lock1 = read_lock(dir.path());

    sync(dir.path(), SyncOptions::default()).expect("second sync succeeds");
    let lock2 = read_lock(dir.path());

    assert!(
        !lock1.contains("installed_at") && !lock1.contains("updated_at"),
        "ggen.lock must be timestamp-free; got:\n{lock1}"
    );
    assert_eq!(
        sha256_hex(lock1.as_bytes()),
        sha256_hex(lock2.as_bytes()),
        "two syncs over the same fixture must emit byte-identical ggen.lock.\n\
         --- run 1 ---\n{lock1}\n--- run 2 ---\n{lock2}"
    );
    let parsed = parse_lock(&lock1);
    let (source, hash) = parsed.get("alpha").expect("alpha locked").clone();
    assert!(
        hash.starts_with("blake3:"),
        "content_hash carries its algorithm tag: {hash}"
    );
    assert!(
        source.starts_with("path:"),
        "path-pack source is recorded as `path:<abs>` (see phase 2): {source}"
    );
}

// ── court phase 2: cross-root (path-independence) ───────────────────────────

/// Same fixture set at two DIFFERENT tempdir roots. PASS (stronger than the
/// lane's conservative brief): `ggen.lock` is FULLY byte-identical across
/// roots — `PackRef::Path`'s `source` string records the DECLARED relative
/// path (`path:packs/alpha`), not the absolute resolved root, and
/// `content_hash` walks root-relative paths only. Path-independence is
/// proven, which means the same fixture checked out at different absolute
/// locations (including on other machines, modulo OS path separators —
/// UNVERIFIED cross-platform) produces identical manifests.
#[test]
fn phase2_cross_root_full_file_byte_identical() {
    let dir_a = TempDir::new().unwrap();
    let dir_b = TempDir::new().unwrap();
    assert_ne!(
        dir_a.path(),
        dir_b.path(),
        "tempdirs must be distinct roots"
    );
    write_fixture(dir_a.path(), &BASE_DIALECTS);
    write_fixture(dir_b.path(), &BASE_DIALECTS);

    sync(dir_a.path(), SyncOptions::default()).expect("sync at root A");
    sync(dir_b.path(), SyncOptions::default()).expect("sync at root B");

    let lock_a = read_lock(dir_a.path());
    let lock_b = read_lock(dir_b.path());

    let parsed_a = parse_lock(&lock_a);
    let parsed_b = parse_lock(&lock_b);
    assert_eq!(
        parsed_a.len(),
        parsed_b.len(),
        "same pack set in both roots"
    );

    let (src_a, hash_a) = parsed_a.get("alpha").expect("alpha in A").clone();
    let (src_b, hash_b) = parsed_b.get("alpha").expect("alpha in B").clone();

    assert_eq!(hash_a, hash_b, "content_hash must be path-independent");
    assert_eq!(
        src_a, src_b,
        "source string must record the declared relative path"
    );
    assert!(
        !src_a.contains(dir_a.path().to_str().unwrap()),
        "source must NOT embed the absolute root: {src_a}"
    );
    assert_eq!(
        sha256_hex(lock_a.as_bytes()),
        sha256_hex(lock_b.as_bytes()),
        "ggen.lock must be byte-identical across distinct roots.\n\
         --- root A ---\n{lock_a}\n--- root B ---\n{lock_b}"
    );
}

// ── court phase 3: dialect declaration reorder ──────────────────────────────

/// Reordering the `[[capabilities.dialects]]` globs in `pack.toml`.
///
/// Findings (documented, not fixed):
/// - Resolution stays GREEN and rendering is unchanged for non-overlapping
///   globs — dialect reorder does not change semantics here.
/// - The lockfile hash DOES change: `content_hash` hashes the pack's full
///   source tree including `pack.toml` bytes, and the resolver does NOT
///   canonicalize the dialect declaration order before hashing. So a pure
///   declaration reorder is a lockfile-visible source change. This is
///   deliberate under RFC-GPACK-001 §39 (the digest MUST cover every source
///   artifact) but means two semantically identical packs with differently
///   ordered declarations do NOT produce identical manifests — an
///   order-sensitivity gap for the cross-platform byte-equality KPI.
#[test]
fn phase3_dialect_reorder_changes_lock_semantics_unchanged() {
    let dir_base = TempDir::new().unwrap();
    let dir_rev = TempDir::new().unwrap();
    write_fixture(dir_base.path(), &BASE_DIALECTS);
    let mut reversed = BASE_DIALECTS;
    reversed.reverse();
    write_fixture(dir_rev.path(), &reversed);

    sync(dir_base.path(), SyncOptions::default()).expect("baseline sync");
    sync(dir_rev.path(), SyncOptions::default()).expect("reordered sync");

    let (_, hash_base) = parse_lock(&read_lock(dir_base.path()))
        .get("alpha")
        .expect("alpha in baseline")
        .clone();
    let (_, hash_rev) = parse_lock(&read_lock(dir_rev.path()))
        .get("alpha")
        .expect("alpha in reordered")
        .clone();

    // Rendered output is order-insensitive for non-overlapping globs.
    let out_base =
        std::fs::read_to_string(dir_base.path().join("out/alpha.txt")).expect("baseline render");
    let out_rev =
        std::fs::read_to_string(dir_rev.path().join("out/alpha.txt")).expect("reordered render");
    assert_eq!(
        out_base, out_rev,
        "rendering must not depend on dialect declaration order"
    );

    assert!(
        hash_base != hash_rev,
        "court premise stale: dialect declaration reorder no longer changes \
         content_hash (resolver now canonicalizes before hashing, or pack.toml \
         bytes left the hash set). Update this phase to assert equality."
    );
    // Documented gap: hash covers pack.toml bytes verbatim, so glob order is
    // lockfile-visible. Reported to the lane; NOT fixed here.
}

// ── court phase 4: receipt content-address binding ──────────────────────────

/// The sync receipt (`<root>/.ggen-v2/receipt.json`) records pack name →
/// BLAKE3 content-hash hex. Prove the recorded value matches a fresh,
/// independent recomputation of `content_hash` over the same pack tree —
/// the receipt is a true content address, not a self-asserted label.
#[test]
fn phase4_receipt_pack_hashes_match_recomputation() {
    let dir = TempDir::new().unwrap();
    write_fixture(dir.path(), &BASE_DIALECTS);

    sync(dir.path(), SyncOptions::default()).expect("sync succeeds");

    let receipt_raw =
        std::fs::read_to_string(dir.path().join(".ggen-v2/receipt.json")).expect("receipt written");
    let receipt: serde_json::Value = serde_json::from_str(&receipt_raw).expect("receipt parses");
    let packs = receipt
        .pointer("/payload/packs")
        .and_then(|p| p.as_object())
        .expect("receipt payload.packs object");
    let recorded = packs
        .get("alpha")
        .and_then(|h| h.as_str())
        .expect("alpha hash recorded in receipt");

    // Independent recomputation over the real pack tree.
    let pack = Pack {
        name: "alpha".to_string(),
        version: "1.0.0".to_string(),
        description: "H06 determinism fixture".to_string(),
        dependencies: BTreeMap::new(),
        semantic_types: ["ex:Thing".to_string()].into_iter().collect(),
        provides: std::collections::BTreeSet::default(),
        requires: std::collections::BTreeSet::default(),
        root: dir.path().join("packs/alpha"),
        ontology_path: dir.path().join("packs/alpha/ontology.ttl"),
        extra_ontology_paths: Vec::new(),
        template_paths: vec![PathBuf::from("templates/alpha.tmpl")],
        lock: true,
        dialects: vec![
            DialectGlob {
                glob: "*".to_string(),
                dialect: "turtle".to_string(),
            },
            DialectGlob {
                glob: "shapes/*".to_string(),
                dialect: "turtle".to_string(),
            },
        ],
    };
    let recomputed = content_hash(&pack).expect("content_hash over real pack tree");
    let recomputed_hex = hex::encode(recomputed);

    assert_eq!(
        recorded, recomputed_hex,
        "receipt's pack hash must equal a fresh content_hash recomputation"
    );

    // And the ggen.lock agrees with both — three-way content-address agreement.
    let (_, lock_hash) = parse_lock(&read_lock(dir.path()))
        .get("alpha")
        .unwrap()
        .clone();
    assert_eq!(
        lock_hash,
        format!("blake3:{recomputed_hex}"),
        "ggen.lock, receipt, and recomputation must agree on one content address"
    );
}

// ── court phase 5: the .ggen/packs.lock surface IS byte-reproducible ────────

/// CLOSED (was: receipted gap, lane F03 fix in
/// `crates/ggen-marketplace/src/packs/lockfile.rs`): the marketplace lockfile
/// no longer stamps `installed_at`/`updated_at` with `Utc::now()` — both are
/// `Option<DateTime<Utc>>`, `None` by default, wall-clock only via explicit
/// `touch()`/assignment — and `save()` relativizes `PackSource::Local` paths
/// against the lockfile's own directory before serializing. Two identical
/// add+save sequences at different absolute roots, moments apart, must now
/// emit byte-identical files. Old lockfiles carrying timestamps or absolute
/// paths still parse (serde defaults; values round-trip).
#[test]
fn phase5_marketplace_packs_lock_byte_identical_across_roots_and_runs() {
    use ggen_marketplace::packs::lockfile::{LockedPack, PackLockfile, PackSource};

    let build = |root: &Path| {
        let mut lf = PackLockfile::new("26.10.20");
        lf.add_pack(
            "alpha",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Local {
                    path: root.join("packs/alpha"),
                },
                integrity: Some(format!("sha256-{}", sha256_hex(b"alpha@1.0.0"))),
                installed_at: None,
                dependencies: Vec::new(),
            },
        );
        let path = root.join("packs.lock");
        lf.save(&path).expect("save packs.lock");
        std::fs::read_to_string(&path).expect("read packs.lock")
    };

    let dir_a = TempDir::new().unwrap();
    let dir_b = TempDir::new().unwrap();
    let a = build(&dir_a.path().join(".1"));
    std::thread::sleep(std::time::Duration::from_millis(2));
    let b = build(&dir_b.path().join(".2"));

    assert_eq!(
        sha256_hex(a.as_bytes()),
        sha256_hex(b.as_bytes()),
        "two identical add+save sequences at different roots must emit \
         byte-identical packs.lock.\n--- root A ---\n{a}\n--- root B ---\n{b}"
    );

    let parsed: serde_json::Value = serde_json::from_str(&a).unwrap();
    assert!(
        parsed.get("updated_at").is_none()
            && parsed["packs"]["alpha"].get("installed_at").is_none(),
        "deterministic lockfile omits timestamp fields entirely: {a}"
    );
    assert_eq!(
        parsed["packs"]["alpha"]["source"]["path"].as_str().unwrap(),
        "packs/alpha",
        "local source path must be stored relative to the lockfile root: {a}"
    );

    // Backward compat: an OLD lockfile with wall-clock stamps and an absolute
    // path still parses, and its values round-trip.
    let old = format!(
        r#"{{"packs": {{"alpha": {{"version": "1.0.0", "source": {{"type": "Local", "path": "{}"}},
            "integrity": "sha256-{}", "installed_at": "2024-01-01T00:00:00Z", "dependencies": []}}}},
            "updated_at": "2024-06-01T12:00:00Z", "ggen_version": "4.0.0"}}"#,
        dir_a.path().join("packs/alpha").display(),
        sha256_hex(b"alpha@1.0.0"),
    );
    let old_path = dir_b.path().join("old").join("packs.lock");
    std::fs::create_dir_all(old_path.parent().unwrap()).unwrap();
    std::fs::write(&old_path, &old).unwrap();
    let loaded = PackLockfile::from_file(&old_path).expect("old lockfile parses");
    assert_eq!(
        loaded.updated_at,
        Some(
            chrono::DateTime::parse_from_rfc3339("2024-06-01T12:00:00Z")
                .unwrap()
                .with_timezone(&chrono::Utc)
        ),
        "legacy updated_at must survive parse"
    );
    match &loaded.packs["alpha"].source {
        PackSource::Local { path } => {
            assert!(
                path.is_absolute(),
                "legacy absolute path parses verbatim: {path:?}"
            );
        }
        other => panic!("expected Local source, got {other:?}"),
    }
}
