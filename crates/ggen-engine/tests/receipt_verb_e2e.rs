//! Chicago-TDD e2e coverage for the `ggen receipt verify` verb (zero
//! arguments; verifies the BLAKE3 chain of `.ggen-v2/receipt.json` under the
//! cwd-resolved project root).
//!
//! Real fixture projects, real `ggen sync run` (library entry) receipts, the
//! real `ggen` binary at the CLI boundary via `assert_cmd` (pattern from
//! `config_schema_dispatch_e2e.rs`). No mocks. Covered behaviors:
//!
//! 1. sync then `receipt verify` in the fixture cwd -> exit 0
//! 2. tamper the stored chain hash (payload hash repaired so the CHAIN
//!    check specifically fires) -> non-zero, error names the mismatch
//! 3. tamper a payload field -> non-zero (payload-hash binding)
//! 4. `receipt verify` with no receipt present -> typed not-found-class
//!    error, non-zero, no panic
//! 5. re-sync (chain advance) -> the new head receipt verifies

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// Resolve the real `ggen` binary the same way `receipt_chain_e2e.rs` and
/// `config_schema_dispatch_e2e.rs` do: `CARGO_BIN_EXE_ggen` (set by
/// `cargo test -p ggen-cli-lib`), then the workspace
/// `target/{debug,release}/ggen`, then fail loudly.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
            let mut dir: &Path = manifest_dir.as_path();
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
            for name in ["ggen", "ggen.exe"] {
                let candidate = target.join(profile).join(name);
                if candidate.is_file() {
                    return candidate;
                }
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

fn scaffold(root: &Path, names: &[&str]) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    write_ontology(root, names);
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn write_ontology(root: &Path, names: &[&str]) {
    use std::fmt::Write as _;

    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for name in names {
        let _ = writeln!(ttl, "ex:{name} ex:name \"{name}\" .");
    }
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
}

fn do_sync(root: &Path, names: &[&str]) {
    write_ontology(root, names);
    sync(
        root,
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed");
}

fn receipt_path(root: &Path) -> PathBuf {
    root.join(".ggen-v2").join("receipt.json")
}

fn read_receipt(root: &Path) -> serde_json::Value {
    serde_json::from_str(&std::fs::read_to_string(receipt_path(root)).expect("read receipt.json"))
        .expect("parse receipt.json")
}

fn write_receipt(root: &Path, v: &serde_json::Value) {
    std::fs::write(
        receipt_path(root),
        serde_json::to_string_pretty(v).expect("serialize"),
    )
    .expect("write receipt.json");
}

/// Run the real binary's `receipt verify` from `root` and assert failure,
/// returning the combined stdout+stderr for message assertions.
fn verify_fails(root: &Path) -> String {
    let output = assert_cmd::Command::new(ggen_bin())
        .args(["receipt", "verify"])
        .current_dir(root)
        .env("TMPDIR", "/tmp")
        .output()
        .expect("spawn ggen receipt verify");
    assert!(
        !output.status.success(),
        "receipt verify must fail; stdout: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    let combined = String::from_utf8_lossy(&output.stdout).to_string()
        + &String::from_utf8_lossy(&output.stderr);
    assert!(
        !combined.contains("panicked"),
        "verify must fail with a typed error, not a panic: {combined}"
    );
    combined
}

/// 1. A real sync produces a receipt whose chain the real binary verifies
///    with exit 0, run from the fixture's cwd (the verb's only root input).
#[test]
fn verify_after_real_sync_succeeds() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    do_sync(dir.path(), &["alice"]);
    assert!(
        receipt_path(dir.path()).exists(),
        "sync must emit receipt.json"
    );

    assert_cmd::Command::new(ggen_bin())
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .env("TMPDIR", "/tmp")
        .assert()
        .success();
}

/// 2. Flip a byte in the stored chain hash and REPAIR `payload_hash_hex` to
///    match the new raw document, so the failure is attributable to the
///    chain-integrity check itself, not the earlier payload check. The
///    error must name the mismatch (stored vs recomputed) and exit non-zero.
#[test]
fn tampered_chain_hash_fails_naming_mismatch() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    do_sync(dir.path(), &["alice"]);

    let mut doc = read_receipt(dir.path());
    let chain = doc["record"]["chain_hash_hex"]
        .as_str()
        .expect("chain_hash_hex string")
        .to_string();
    // Flip the first hex char to a different hex digit.
    let flipped: String = chain
        .chars()
        .enumerate()
        .map(|(i, c)| {
            if i == 0 {
                if c == '0' {
                    '1'
                } else {
                    '0'
                }
            } else {
                c
            }
        })
        .collect();
    assert_ne!(flipped, chain, "flip must change the hash");
    doc["record"]["chain_hash_hex"] = serde_json::Value::String(flipped.clone());
    write_receipt(dir.path(), &doc);

    // Repair the payload hash over the exact raw `payload` bytes so check 1
    // passes and check 2 (chain) is what fires. Splice the hex in-place in
    // the raw text (a re-serialization would change the payload bytes the
    // hash is computed over, the same reason `stored_payload_hash` borrows
    // raw bytes).
    // Borrowing probe must sit next to its use of `raw` for the borrow region to read clearly.
    #[allow(clippy::items_after_statements)]
    #[derive(serde::Deserialize)]
    struct Probe<'a> {
        #[serde(borrow)]
        payload: &'a serde_json::value::RawValue,
    }
    let raw = std::fs::read_to_string(receipt_path(dir.path())).expect("read raw receipt");
    let probe: Probe<'_> = serde_json::from_str(&raw).expect("probe payload");
    let new_payload_hash = blake3::hash(probe.payload.get().as_bytes())
        .to_hex()
        .to_string();
    let old_payload_hash = doc["record"]["payload_hash_hex"]
        .as_str()
        .expect("payload hash string")
        .to_string();
    let repaired = raw.replace(&old_payload_hash, &new_payload_hash);
    std::fs::write(receipt_path(dir.path()), repaired).expect("write repaired receipt");

    // The on-disk state must carry the flip and the repair.
    let final_doc = read_receipt(dir.path());
    assert_eq!(
        final_doc["record"]["chain_hash_hex"]
            .as_str()
            .expect("chain"),
        flipped,
        "flipped chain hash must be on disk before verify"
    );
    assert_eq!(
        final_doc["record"]["payload_hash_hex"]
            .as_str()
            .expect("payload hash"),
        new_payload_hash,
        "repaired payload hash must be on disk before verify"
    );

    let out = verify_fails(dir.path());
    assert!(
        out.contains("chain hash mismatch"),
        "error must name the chain mismatch; got: {out}"
    );
    assert!(
        out.contains("mismatch"),
        "error must pair stored vs recomputed; got: {out}"
    );
}

/// 3. Tamper a field inside the payload (receipt stays valid JSON) without
///    touching any hash: the stored `payload_hash_hex` no longer matches the
///    stored payload bytes, so the payload-binding check fails closed.
#[test]
fn tampered_payload_fails_payload_binding() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    do_sync(dir.path(), &["alice"]);

    let mut doc = read_receipt(dir.path());
    let original = doc["payload"].to_string();
    // Mutate an EXISTING payload field's value (the payload schema denies
    // unknown fields): flip a recorded write decision.
    let decision_key = doc["payload"]["decisions"]
        .as_object()
        .expect("decisions object")
        .keys()
        .next()
        .expect("at least one decision")
        .clone();
    doc["payload"]["decisions"][&decision_key] = serde_json::Value::String("tampered".into());
    assert_ne!(doc["payload"].to_string(), original, "payload must change");
    write_receipt(dir.path(), &doc);

    let out = verify_fails(dir.path());
    assert!(
        out.contains("payload hash mismatch"),
        "error must name the payload binding failure; got: {out}"
    );
}

/// 4. `receipt verify` in a directory with no receipt must fail with a
///    typed, not-found-class error (naming the receipt path / unreadable)
///    rather than panicking.
#[test]
fn verify_with_no_receipt_fails_typed_not_panic() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]); // no sync -> no receipt

    let out = verify_fails(dir.path());
    assert!(
        out.contains("receipt") && out.contains("unreadable"),
        "missing receipt must produce a typed not-found-class error naming \
         the receipt path as unreadable; got: {out}"
    );
}

/// 5. Re-sync after the earlier chain tamper overwrites receipt.json with a
///    fresh, advanced head, which the real binary verifies clean.
#[test]
fn resync_after_tamper_produces_verifiable_new_head() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path(), &["alice"]);
    do_sync(dir.path(), &["alice"]);
    let first_chain = read_receipt(dir.path())["record"]["chain_hash_hex"]
        .as_str()
        .expect("chain hash")
        .to_string();

    // Tamper (any byte flip invalidates), then re-sync.
    let mut doc = read_receipt(dir.path());
    doc["record"]["chain_hash_hex"] = serde_json::Value::String(format!("0{}", &first_chain[1..]));
    write_receipt(dir.path(), &doc);
    verify_fails(dir.path());

    do_sync(dir.path(), &["alice", "bob"]);
    let new_chain = read_receipt(dir.path())["record"]["chain_hash_hex"]
        .as_str()
        .expect("chain hash")
        .to_string();
    assert_ne!(new_chain, first_chain, "chain must advance on re-sync");
    assert_ne!(
        new_chain.chars().next().expect("first char"),
        '0',
        "tamper shape gone"
    );

    assert_cmd::Command::new(ggen_bin())
        .args(["receipt", "verify"])
        .current_dir(dir.path())
        .env("TMPDIR", "/tmp")
        .assert()
        .success();
}
