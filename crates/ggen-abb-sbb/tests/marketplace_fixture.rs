//! Cross-repository witness: ggen consumes the exact marketplace projection.
//! The source lock binds this vendored fixture to ggen-marketplace PR #506.

use ggen_abb_sbb::*;

const MARKETPLACE_FIXTURE: &str = include_str!("../fixtures/marketplace-ea-graph.json");
const LOCAL_FIXTURE: &str = include_str!("../fixtures/ea-graph.json");
const SOURCE_LOCK: &str = include_str!("../fixtures/marketplace-source.json");

#[test]
fn marketplace_fixture_is_the_exact_admitted_projection() {
    let lock: serde_json::Value = serde_json::from_str(SOURCE_LOCK).unwrap();
    assert_eq!(lock["repository"], "seanchatmangpt/ggen-marketplace");
    assert_eq!(lock["pull_request"], 506);
    assert_eq!(
        lock["git_blob_sha"],
        "dce7518e8a1e22f3864a5f957b3fe6809f27df02"
    );
    assert_eq!(lock["authority"], "NONE");

    // Until the semantic root grows a native RDF loader, the marketplace JSON
    // projection is the executable interchange surface. It must remain byte-identical
    // to the generator-owned fixture, so no hand-edited shadow schema can emerge.
    assert_eq!(MARKETPLACE_FIXTURE, LOCAL_FIXTURE);

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
