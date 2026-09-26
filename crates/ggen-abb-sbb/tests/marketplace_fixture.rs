//! Cross-repository witness: ggen consumes the exact marketplace projection.
//! The source lock binds this vendored fixture to ggen-marketplace PR #506.

use ggen_abb_sbb::*;
use sha2::{Digest, Sha256};

const MARKETPLACE_FIXTURE: &str = include_str!("../fixtures/marketplace-ea-graph.json");
const LOCAL_FIXTURE: &str = include_str!("../fixtures/ea-graph.json");
const SOURCE_LOCK: &str = include_str!("../fixtures/marketplace-source.json");

/// L1 falsifier (court a91fef6d): the provenance lock is non-vacuous only if the
/// `fixture_sha256` pinned in the lock is recomputed from the vendored bytes. A
/// fixture regenerated from a drifted generator (or hand-edited in place) changes
/// these bytes and is refused here even when it still parses and still matches the
/// local generator output.
fn fixture_sha256_hex(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut hex = String::with_capacity(64);
    for byte in digest {
        hex.push_str(&format!("{byte:02x}"));
    }
    hex
}

#[test]
fn marketplace_fixture_is_the_exact_admitted_projection() {
    let lock: serde_json::Value = serde_json::from_str(SOURCE_LOCK).unwrap();
    assert_eq!(lock["repository"], "seanchatmangpt/ggen-marketplace");
    assert_eq!(lock["pull_request"], 506);
    assert_eq!(
        lock["git_blob_sha"],
        "dce7518e8a1e22f3864a5f957b3fe6809f27df02"
    );
    assert_eq!(
        lock["fixture_sha256"],
        "1e7167ad397343d34e3aa8e657b8fbd73efec018d052b200e87ed0c265b3a0a0"
    );
    assert_eq!(lock["authority"], "NONE");

    // Until the semantic root grows a native RDF loader, the marketplace JSON
    // projection is the executable interchange surface. It must remain byte-identical
    // to the generator-owned fixture, so no hand-edited shadow schema can emerge.
    assert_eq!(MARKETPLACE_FIXTURE, LOCAL_FIXTURE);

    // DoD8 provenance binding, recomputed not asserted: the vendored bytes must hash
    // to the locked digest. Mutant L1 (regenerated fixture, stale lock) dies here.
    assert_eq!(
        fixture_sha256_hex(MARKETPLACE_FIXTURE.as_bytes()),
        lock["fixture_sha256"],
        "vendored marketplace fixture bytes no longer match the locked fixture_sha256"
    );

    let graph = parse_graph(MARKETPLACE_FIXTURE).expect("marketplace projection admits");
    let request = Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: Authority::Construct,
        expected_graph_digest: Some(graph.digest()),
    };
    let admitted = admit(&graph, &request).expect("marketplace SBB qualifies");
    let manufactured = manufacture(
        &admitted,
        &Generator {
            id: "ggen-marketplace-consumer".into(),
            version: "26.9.26".into(),
        },
    )
    .expect("marketplace SBB manufactures");

    assert_eq!(manufactured.receipt.authority, Authority::None);
    assert_eq!(manufactured.receipt.ceiling, Authority::Construct);
    assert_eq!(manufactured.receipt.abb, "abb:event-ingest");
    assert_eq!(manufactured.receipt.sbb, "sbb:ingest-0000");
}
