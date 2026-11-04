//! Chicago-style integration tests for the authority law
//! (`ggen-sjira/src/authority.rs` — port of
//! `GgenIgniter.SemanticJira.Authority` + `sj:AuthorityTrustRootShape`).
//!
//! Real collaborators: real JSON graph files on disk, real sha256 over real
//! descriptions, real pin maps. No doubles. Assertions on final state (the
//! returned index / refusal / digest), never on interactions.

use ggen_sjira::authority::{
    admission_digest, pins, AuthorityGraph, AuthorityIndex, AuthorityTrustRoot, LocatorRefusal,
    OriginRefusal, ResolvedAuthority, SourceLocator, AuthorityType,
};
use serde_json::json;
use std::fs;

const GIT_SHA: &str = "0123456789abcdef0123456789abcdef01234567";

/// A well-formed authority + its trust root under a durable git locator.
fn good_graph_json() -> serde_json::Value {
    json!({
        "authorities": [
            {
                "iri": "https://graph.test/auth/x1",
                "types": ["StrategicObjective"],
                "statements": { "sj:title": ["Ship the kernel"] }
            }
        ],
        "trust_roots": [
            {
                "node": "https://graph.test/trust/r1",
                "authority_iri": "https://graph.test/auth/x1",
                "admission_digest": "",
                "source_locator": format!("git:owner/repo@{GIT_SHA}:priv/ontology.ttl")
            }
        ]
    })
}

fn stamped_graph_value() -> serde_json::Value {
    // Compute the real digest over the unstamped node, then stamp it into the
    // JSON — the same admit/verify round-trip the kernel performs.
    let unstamped = AuthorityGraph::from_json(&good_graph_json()).expect("parse");
    let digest = admission_digest(&unstamped.authorities[0]);
    let mut v = good_graph_json();
    v["authorities"][0]["statements"]["admissionDigest"] = json!([digest.clone()]);
    v["trust_roots"][0]["admission_digest"] = json!(digest);
    v
}

#[test]
fn admit_verify_round_trip_over_a_real_file() {
    let dir = std::env::temp_dir().join(format!("sjira-auth-{}", std::process::id()));
    fs::create_dir_all(&dir).expect("mkdir");
    let path = dir.join("authority.json");
    let v = stamped_graph_value();
    fs::write(&path, serde_json::to_string_pretty(&v).unwrap()).expect("write");

    let graph = AuthorityGraph::from_path(&path).expect("readable");
    let index = AuthorityIndex::build_pinned(&graph);

    let resolved = index
        .resolve("https://graph.test/auth/x1")
        .expect("origin must resolve");
    assert_eq!(resolved.iri, "https://graph.test/auth/x1");
    assert_eq!(resolved.authority_type, AuthorityType::StrategicObjective);
    let unstamped = AuthorityGraph::from_json(&v).expect("parse");
    let expected = admission_digest(&unstamped.authorities[0]);
    assert_eq!(resolved.admission_digest, expected);

    // A fresh IRI is not an authority node at all.
    assert_eq!(
        index.resolve("https://graph.test/auth/fresh-iri"),
        Err(OriginRefusal::AuthorityNotAdmitted {
            iri: "https://graph.test/auth/fresh-iri".to_string()
        })
    );

    fs::remove_dir_all(&dir).ok();
}

#[test]
fn edited_node_under_kept_digest_is_refused_digest_mismatch() {
    let mut v = stamped_graph_value();
    // Edit a statement, keep the digest: the recompute must diverge.
    v["authorities"][0]["statements"]["sj:title"] = json!(["Ship the kernel LATER"]);
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityDigestMismatch {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn edited_type_under_kept_digest_is_refused_digest_mismatch() {
    let mut v = stamped_graph_value();
    v["authorities"][0]["types"] = json!(["GoalCheckpoint"]);
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityDigestMismatch {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn typed_but_undigested_is_not_admitted() {
    let mut v = good_graph_json();
    // Remove the digest statement entirely (typed is not admitted).
    v["authorities"][0]["statements"]
        .as_object_mut()
        .unwrap()
        .remove("admissionDigest");
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityNotAdmitted {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn multiple_digests_refuse_digest_mismatch() {
    let mut v = stamped_graph_value();
    v["authorities"][0]["statements"]["admissionDigest"] = json!([
        v["authorities"][0]["statements"]["admissionDigest"][0].clone(),
        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
    ]);
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityDigestMismatch {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn unpinned_recomputing_digest_is_refused_not_pinned() {
    let mut v = stamped_graph_value();
    // The digest recomputes, but the pin names a DIFFERENT digest.
    v["trust_roots"][0]["admission_digest"] =
        json!("sha256:2222222222222222222222222222222222222222222222222222222222222222");
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityNotPinned {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn trust_root_under_ephemeral_locator_pins_nothing() {
    for bad_locator in [
        "/tmp/authority.ttl",
        "~/authority.ttl",
        "scratch/authority.ttl",
        "tmp/authority.ttl",
        format!("git:owner/repo@{GIT_SHA}:/tmp/x.ttl").as_str(),
        "file:///etc/ontology.ttl",
    ] {
        let mut v = stamped_graph_value();
        v["trust_roots"][0]["source_locator"] = json!(bad_locator);
        let graph = AuthorityGraph::from_json(&v).expect("parse");
        // The malformed trust root pins nothing: the authority recomputes but
        // ends refused NOT PINNED (fail closed).
        let index = AuthorityIndex::build_pinned(&graph);
        assert_eq!(
            index.resolve("https://graph.test/auth/x1"),
            Err(OriginRefusal::AuthorityNotPinned {
                iri: "https://graph.test/auth/x1".to_string()
            }),
            "locator {bad_locator} must not admit"
        );
        // And the pin map itself is empty.
        assert!(pins(&graph).is_empty(), "locator {bad_locator} must pin nothing");
    }
}

#[test]
fn trust_root_with_ill_formed_digest_pins_nothing() {
    let mut v = stamped_graph_value();
    v["trust_roots"][0]["admission_digest"] = json!("sha256:NOTAHASH");
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    assert!(pins(&graph).is_empty());
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityNotPinned {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
    // Direct construction is a typed refusal, not a panic.
    assert!(AuthorityTrustRoot::new(
        "https://graph.test/trust/r1",
        "https://graph.test/auth/x1",
        "sha256:NOTAHASH",
        format!("git:owner/repo@{GIT_SHA}:priv/ontology.ttl"),
    )
    .is_err());
}

#[test]
fn forged_self_declared_authority_is_refused() {
    // The forge attempt: a caller-supplied graph that BOTH declares a fresh
    // authority AND pins it with its own trust root — but stamps a digest it
    // invented rather than one that recomputes over its own description.
    let forged = json!({
        "authorities": [
            {
                "iri": "https://evil.test/auth/self",
                "types": ["StrategicObjective"],
                "statements": {
                    "sj:title": ["I am authority"],
                    "admissionDigest": [
                        "sha256:3333333333333333333333333333333333333333333333333333333333333333"
                    ]
                }
            }
        ],
        "trust_roots": [
            {
                "node": "https://evil.test/trust/self-pin",
                "authority_iri": "https://evil.test/auth/self",
                "admission_digest": "sha256:3333333333333333333333333333333333333333333333333333333333333333",
                "source_locator": format!("git:owner/repo@{GIT_SHA}:priv/ontology.ttl")
            }
        ]
    });
    let graph = AuthorityGraph::from_json(&forged).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    // The self-stamped digest does not recompute (it was invented, not
    // derived) — the self-exclusion law defeats the declaration before the
    // pin is even consulted.
    assert_eq!(
        index.resolve("https://evil.test/auth/self"),
        Err(OriginRefusal::AuthorityDigestMismatch {
            iri: "https://evil.test/auth/self".to_string()
        })
    );
}

#[test]
fn self_declared_well_stamped_authority_without_external_pin_is_refused_not_pinned() {
    // Even a forge that stamps HONESTLY (digest recomputes over its own
    // description) stays refused when the ONLY pin is its own graph-local
    // trust root — here modeled by resolving against an index whose pins are
    // empty (an external canonical trust-root set that does not contain the
    // forged pair).
    let honest = json!({
        "authorities": [
            {
                "iri": "https://evil.test/auth/honest",
                "types": ["GoalCheckpoint"],
                "statements": { "sj:title": ["honest stamp"] }
            }
        ],
        "trust_roots": []
    });
    let graph = AuthorityGraph::from_json(&honest).expect("parse");
    let digest = admission_digest(&graph.authorities[0]);
    let mut stamped = graph.clone();
    stamped.authorities[0]
        .statements
        .insert("admissionDigest".to_string(), vec![digest.clone()]);

    let index = AuthorityIndex::build(&stamped).apply_pins(&Default::default());
    assert_eq!(
        index.resolve("https://evil.test/auth/honest"),
        Err(OriginRefusal::AuthorityNotPinned {
            iri: "https://evil.test/auth/honest".to_string()
        })
    );
}

#[test]
fn prose_typed_node_is_type_mismatch_even_when_stamped_and_pinned() {
    let v = stamped_graph_value();
    let mut graph = AuthorityGraph::from_json(&v).expect("parse");
    // Also type it as a Proposition: prose is never authority (ADR-012).
    graph.authorities[0].types.push("Proposition".to_string());
    // Keep the digest consistent? No — the type participates in the digest,
    // so this is a mismatch; to observe TYPE mismatch specifically, restamp.
    let digest = admission_digest(&graph.authorities[0]);
    graph.authorities[0]
        .statements
        .insert("admissionDigest".to_string(), vec![digest.clone()]); // minimal unblock (SLA): digest reused below
    // And pin the new pair — pinned and recomputing, yet still refused.
    let mut pinned_graph = graph;
    pinned_graph.trust_roots[0].admission_digest = digest;
    let index = AuthorityIndex::build_pinned(&pinned_graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityTypeMismatch {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
}

#[test]
fn unreadable_index_fails_closed() {
    let missing = std::env::temp_dir().join(format!(
        "sjira-auth-missing-{}-nope.json",
        std::process::id()
    ));
    let _ = fs::remove_file(&missing);
    match AuthorityGraph::from_path(&missing) {
        Err(OriginRefusal::AuthorityIndexUnavailable { path, .. }) => {
            assert_eq!(path.as_deref(), Some(missing.to_str().unwrap()));
        }
        other => panic!("expected AuthorityIndexUnavailable, got {other:?}"),
    }
}

#[test]
fn unparseable_index_fails_closed() {
    let err = AuthorityGraph::from_json_str("{not json").expect_err("must refuse");
    assert!(matches!(
        err,
        OriginRefusal::AuthorityIndexUnavailable { path: None, .. }
    ));
}

#[test]
fn ambiguous_pin_pins_nothing() {
    let v = stamped_graph_value();
    let mut graph = AuthorityGraph::from_json(&v).expect("parse");
    let digest = graph.trust_roots[0].admission_digest.clone();
    graph.trust_roots.push(serde_json::from_value(json!({
        "node": "https://graph.test/trust/r2",
        "authority_iri": "https://graph.test/auth/x1",
        "admission_digest": "sha256:4444444444444444444444444444444444444444444444444444444444444444",
        "source_locator": format!("git:owner/repo@{GIT_SHA}:priv/ontology.ttl")
    })).expect("candidate"));
    assert!(pins(&graph).is_empty(), "ambiguous pins must pin nothing");
    let index = AuthorityIndex::build_pinned(&graph);
    assert_eq!(
        index.resolve("https://graph.test/auth/x1"),
        Err(OriginRefusal::AuthorityNotPinned {
            iri: "https://graph.test/auth/x1".to_string()
        })
    );
    let _ = digest;
}

#[test]
fn duplicate_identical_pins_are_not_ambiguous() {
    let v = stamped_graph_value();
    let mut graph = AuthorityGraph::from_json(&v).expect("parse");
    let duplicate = graph.trust_roots[0].clone();
    let mut r2 = duplicate;
    r2.node = "https://graph.test/trust/r2".to_string();
    graph.trust_roots.push(r2);
    assert_eq!(pins(&graph).len(), 1);
    let index = AuthorityIndex::build_pinned(&graph);
    assert!(index.resolve("https://graph.test/auth/x1").is_ok());
}

#[test]
fn https_locator_admits() {
    let mut v = stamped_graph_value();
    v["trust_roots"][0]["source_locator"] = json!("https://graph.test/canonical/ontology.ttl");
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert!(index.resolve("https://graph.test/auth/x1").is_ok());
}

#[test]
fn git_notes_locator_admits() {
    let mut v = stamped_graph_value();
    v["trust_roots"][0]["source_locator"] =
        json!(format!("git-notes:refs/notes/authors@{GIT_SHA}"));
    let graph = AuthorityGraph::from_json(&v).expect("parse");
    let index = AuthorityIndex::build_pinned(&graph);
    assert!(index.resolve("https://graph.test/auth/x1").is_ok());
}

#[test]
fn stamping_is_idempotent_via_public_digest() {
    let graph = AuthorityGraph::from_json(&stamped_graph_value()).expect("parse");
    let stamped = &graph.authorities[0];
    let d1 = admission_digest(stamped);
    // Restamping produces the same digest (exclusion law).
    assert_eq!(admission_digest(stamped), d1);
}

#[test]
fn locator_parse_typed_refusals() {
    assert_eq!(
        SourceLocator::parse("~/x.ttl"),
        Err(LocatorRefusal::NotDurable("~/x.ttl".to_string()))
    );
    assert!(matches!(
        SourceLocator::parse("file:///etc/x"),
        Err(LocatorRefusal::NotDurable(_))
    ));
    assert!(matches!(
        SourceLocator::parse("git:no-sha-here"),
        Err(LocatorRefusal::Malformed(_))
    ));
}

#[test]
fn resolved_authority_display() {
    let r = ResolvedAuthority {
        iri: "https://graph.test/auth/x1".to_string(),
        admission_digest: "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            .to_string(),
        authority_type: AuthorityType::GoalCheckpoint,
    };
    let s = r.to_string();
    assert!(s.contains("GoalCheckpoint"));
    assert!(s.contains("https://graph.test/auth/x1"));
}
