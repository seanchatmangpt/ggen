//! RFC-GPACK-001 §9 — consumer alias ≠ canonical pack name (T09).
//!
//! A consumer MAY alias a pack under a local `[packs]` key that differs from
//! the pack's own `pack.toml` `[pack].name`. Resolution must keep BOTH
//! identities, and the canonical name — never the alias — is what lands in
//! the new identity field of the sync receipt and `ggen.lock`, beside the
//! legacy alias-keyed fields (additive; existing fields keep meaning).
//!
//! §8 correspondence (`Project(I_S)=I_B`) is admission's business and is NOT
//! enforced here: an alias differing from the canonical name is lawful —
//! that is the alias's purpose.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::pack::LOCK_FILE_NAME;
use ggen_engine::sync::{sync, ReceiptPayload, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use tempfile::TempDir;

const PACK_NAME: &str = "acme-payments-pack";
const ALIAS: &str = "payments";

const GGEN_TOML: &str = r#"
[project]
name = "alias-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.payments]
path = "../packs/acme-payments-pack"
"#;

const PACK_TOML: &str = r#"
[pack]
name = "acme-payments-pack"
version = "1.0.0"
description = "payments"
"#;

const PACK_ONTOLOGY: &str = r#"
@prefix dom: <http://example.com/ontology#> .
dom:Payment a dom:DomainClass .
"#;

const PACK_TEMPLATE: &str = "---\nto: src/payment.rs\n---\n// generated\n";

/// The RFC §9 example on disk: pack directory (and pack.toml name)
/// `acme-payments-pack`, aliased as `payments` in the consumer's ggen.toml.
fn scaffold() -> TempDir {
    let base = TempDir::new().expect("tempdir");
    let pack_dir = base.path().join("packs").join(PACK_NAME);
    std::fs::create_dir_all(pack_dir.join("templates")).expect("pack templates dir");
    std::fs::write(pack_dir.join("pack.toml"), PACK_TOML).expect("pack.toml");
    std::fs::write(pack_dir.join("ontology.ttl"), PACK_ONTOLOGY).expect("ontology.ttl");
    std::fs::write(pack_dir.join("templates/payment.rs.tmpl"), PACK_TEMPLATE).expect("template");

    let root = base.path().join("project");
    std::fs::create_dir_all(root.join("templates")).expect("project templates dir");
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("ggen.toml");
    std::fs::write(root.join("ontology.ttl"), "").expect("ontology.ttl");
    base
}

fn read_receipt_payload(base: &Path) -> ReceiptPayload {
    let raw =
        std::fs::read_to_string(base.join("project").join(RECEIPT_REL_PATH)).expect("receipt");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("parse receipt");
    receipt.payload
}

/// T09 test 1 — `consumer_alias_preserved`: the alias `payments` resolves the
/// pack whose canonical name is `acme-payments-pack`; the sync receipt and
/// `ggen.lock` show BOTH, keyed by the alias (legacy fields unchanged), with
/// the canonical name carried in the new additive identity field.
#[test]
fn consumer_alias_preserved() {
    let base = scaffold();
    let root = base.path().join("project");

    let report = sync(&root, SyncOptions::default()).expect("sync must resolve the aliased pack");
    assert!(
        root.join("src/payment.rs").is_file(),
        "the aliased pack's template must actually render: {:?}",
        report.written
    );

    // Report: legacy `packs` map stays keyed by the consumer alias; the
    // canonical name arrives beside it.
    assert!(
        report.packs.contains_key(ALIAS),
        "legacy alias-keyed hash map is unchanged: {:?}",
        report.packs
    );
    assert_eq!(
        report.pack_canonical_names.get(ALIAS).map(String::as_str),
        Some(PACK_NAME),
        "receipt output must record the canonical name beside the alias"
    );

    // Persisted receipt: both identities, alias-keyed (legacy) + canonical.
    let payload = read_receipt_payload(base.path());
    assert!(
        payload.packs.contains_key(ALIAS),
        "receipt payload legacy packs map must stay keyed by the alias: {:?}",
        payload.packs
    );
    assert_eq!(
        payload.pack_canonical_names.get(ALIAS).map(String::as_str),
        Some(PACK_NAME),
        "receipt payload must record canonical_name = {PACK_NAME} beside alias {ALIAS}"
    );

    // Lockfile: the section key stays the alias; canonical_name is recorded
    // beside source/content_hash.
    let lock = std::fs::read_to_string(root.join(LOCK_FILE_NAME)).expect("ggen.lock");
    assert!(
        lock.contains(&format!("[packs.{ALIAS}]")),
        "lockfile section key stays the consumer alias: {lock}"
    );
    assert!(
        lock.contains(&format!("canonical_name = \"{PACK_NAME}\"")),
        "lockfile records the canonical name beside the alias: {lock}"
    );
}

/// T09 test 2 — inversion witness (RFC §96): the assertions here are built
/// so that ANY implementation which conflates alias and canonical name
/// fails. Concretely, an implementation that (a) writes the alias into the
/// canonical_name field, (b) never parses `pack.toml`'s `[pack].name`, or
/// (c) re-keys the legacy maps by the canonical name cannot satisfy the
/// conjunction below, because the fixture's alias (`payments`) and canonical
/// name (`acme-payments-pack`) differ on disk.
#[test]
fn alias_and_canonical_name_conflation_cannot_pass() {
    let base = scaffold();
    let root = base.path().join("project");

    sync(&root, SyncOptions::default()).expect("sync");

    let payload = read_receipt_payload(base.path());

    // (a)/(b) falsifier: the canonical identity recorded must be the
    // pack.toml name, provably read from the pack on disk — never the alias.
    let recorded_canonical: &String = payload
        .pack_canonical_names
        .get(ALIAS)
        .expect("canonical name must be recorded for the aliased pack");
    assert_ne!(
        recorded_canonical, ALIAS,
        "INVERSION: canonical_name field carries the consumer alias — the alias \
         distinction has been conflated away"
    );
    let on_disk_pack_toml =
        std::fs::read_to_string(base.path().join("packs").join(PACK_NAME).join("pack.toml"))
            .expect("re-read pack.toml");
    assert!(
        on_disk_pack_toml.contains(&format!("name = \"{PACK_NAME}\"")),
        "fixture sanity: pack.toml really declares {PACK_NAME}"
    );
    assert_eq!(
        recorded_canonical, PACK_NAME,
        "canonical name must equal the pack.toml [pack].name, byte for byte"
    );

    // (c) falsifier: the legacy alias-keyed fields keep their meaning — the
    // receipt payload's packs map is keyed by the alias, and the canonical
    // name does not appear as a key of either map.
    assert!(
        payload.packs.contains_key(ALIAS),
        "INVERSION: legacy packs map re-keyed by canonical name"
    );
    let no_canonical_keys: Vec<&String> = payload
        .packs
        .keys()
        .chain(payload.pack_canonical_names.keys())
        .filter(|k| *k == PACK_NAME)
        .collect();
    assert!(
        no_canonical_keys.is_empty(),
        "INVERSION: canonical name used as a map key where the alias is the \
         contract: {no_canonical_keys:?}"
    );

    // Same witness at the lockfile layer.
    let lock = std::fs::read_to_string(root.join(LOCK_FILE_NAME)).expect("ggen.lock");
    assert!(
        lock.contains(&format!("canonical_name = \"{PACK_NAME}\"")),
        "INVERSION: lockfile canonical_name is not the pack.toml name"
    );
    assert!(
        !lock.contains(&format!("canonical_name = \"{ALIAS}\"")),
        "INVERSION: lockfile canonical_name carries the alias"
    );
}

/// T09 scope 3 — §8 is admission's business, NOT aliasing: an alias that
/// differs from the canonical name must sync cleanly (no new refusal), and a
/// second sync of the same layout stays green with the identity pair
/// recorded identically (idempotent identity recording).
#[test]
fn differing_alias_must_not_refuse_and_resyncs_clean() {
    let base = scaffold();
    let root = base.path().join("project");

    let first = sync(&root, SyncOptions::default());
    assert!(
        first.is_ok(),
        "an alias differing from the canonical name is lawful and must not refuse: {:?}",
        first.err()
    );
    let second = sync(&root, SyncOptions::default());
    assert!(
        second.is_ok(),
        "re-sync with an unchanged alias must stay green: {:?}",
        second.err()
    );

    let payload = read_receipt_payload(base.path());
    assert_eq!(
        payload.pack_canonical_names.get(ALIAS).map(String::as_str),
        Some(PACK_NAME),
        "identity pair survives the re-sync unchanged"
    );
    assert!(
        payload.packs.contains_key(ALIAS),
        "alias-keyed legacy map survives the re-sync unchanged"
    );
}
