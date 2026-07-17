//! PROJ-302 extension: `schema/frontmatter-schema.ttl` is the closed-vocabulary
//! source of truth for `crate::template::Frontmatter`. This test parses the
//! TTL and asserts the declared field set is an *exact* match against the
//! struct's *actual* fields, reflected via `schemars::schema_for!` — not a
//! hand-maintained mirror list, which is exactly the kind of silently
//! drifting duplicate this ticket exists to rule out.
//!
//! If a field is added to the struct without updating the TTL (or vice
//! versa), this test fails — that is the gate, not the documentation.

use std::collections::BTreeSet;

use ggen_engine::{graph::DeterministicGraph, template::Frontmatter};
use oxigraph::sparql::QueryResults;
use schemars::schema_for;
use serde_json::Value as Json;

const SCHEMA_TTL: &str = include_str!("../schema/frontmatter-schema.ttl");

#[test]
fn frontmatter_fields_match_struct() {
    let graph = DeterministicGraph::new().expect("graph");
    graph
        .insert_turtle(SCHEMA_TTL)
        .expect("schema ttl must parse");

    let query = r#"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?name WHERE {
            ggenspec:Frontmatter ggenspec:hasField ?field .
            ?field ggenspec:name ?name .
        }
    "#;
    let QueryResults::Solutions(solutions) = graph.query(query).expect("sparql eval") else {
        panic!("expected SELECT results");
    };
    let declared: BTreeSet<String> = solutions
        .map(|s| {
            let s = s.expect("solution");
            s.get("name")
                .expect("?name bound")
                .to_string()
                .trim_matches('"')
                .to_string()
        })
        .collect();

    let schema = schema_for!(Frontmatter);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    let actual: BTreeSet<String> = json
        .get("properties")
        .and_then(Json::as_object)
        .expect("Frontmatter schema has a properties object")
        .keys()
        .cloned()
        .collect();

    assert_eq!(
        declared, actual,
        "Frontmatter field set drifted between schema/frontmatter-schema.ttl and the struct"
    );
}
