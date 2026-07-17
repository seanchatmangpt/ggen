//! PROJ-302 extension: `schema/ggen-toml-schema.ttl` is the closed-vocabulary
//! source of truth for `crate::config::GgenConfig` and its nested sections.
//! This test parses the TTL and asserts each section's declared field set is
//! an *exact* match against the struct's *actual* fields, reflected via
//! `schemars::schema_for!` — not a hand-maintained mirror list, which is
//! exactly the kind of silently-drifting duplicate this ticket exists to
//! rule out (the failure mode found in every sibling implementation's
//! LSP/validator during this session's research).
//!
//! If a field is added to a struct without updating the TTL (or vice versa),
//! this test fails — that is the gate, not the documentation.

use std::collections::BTreeSet;

use ggen_engine::{
    config::{GgenConfig, Ontology, PackRef, Project, Templates},
    graph::DeterministicGraph,
};
use oxigraph::sparql::QueryResults;
use schemars::schema_for;
use serde_json::Value as Json;

const SCHEMA_TTL: &str = include_str!("../schema/ggen-toml-schema.ttl");

fn load_schema() -> DeterministicGraph {
    let g = DeterministicGraph::new().expect("graph");
    g.insert_turtle(SCHEMA_TTL).expect("schema ttl must parse");
    g
}

/// Query the field-name set declared for a given section/variant IRI local name.
fn declared_fields(graph: &DeterministicGraph, local_name: &str) -> BTreeSet<String> {
    let query = format!(
        r#"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?name WHERE {{
            ggenspec:{local_name} ggenspec:hasField ?field .
            ?field ggenspec:name ?name .
        }}
        "#
    );
    let QueryResults::Solutions(solutions) = graph.query(&query).expect("sparql eval") else {
        panic!("expected SELECT results");
    };
    solutions
        .map(|s| {
            let s = s.expect("solution");
            s.get("name")
                .expect("?name bound")
                .to_string()
                .trim_matches('"')
                .to_string()
        })
        .collect()
}

/// The top-level property-name set of a `schemars`-generated JSON Schema for
/// `T` — this is real reflection over the actual struct definition, not a
/// hand-typed list.
fn struct_fields<T: schemars::JsonSchema>() -> BTreeSet<String> {
    let schema = schema_for!(T);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    json.get("properties")
        .and_then(Json::as_object)
        .unwrap_or_else(|| {
            panic!(
                "schema for {} has no `properties` object",
                std::any::type_name::<T>()
            )
        })
        .keys()
        .cloned()
        .collect()
}

/// The property-name sets of each variant of the untagged `PackRef` enum.
/// `schemars` represents an untagged enum's variants under `oneOf`/`anyOf`,
/// each with its own `properties` object.
fn pack_ref_variant_fields() -> Vec<BTreeSet<String>> {
    let schema = schema_for!(PackRef);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    let variants = json
        .get("oneOf")
        .or_else(|| json.get("anyOf"))
        .and_then(Json::as_array)
        .expect("PackRef schema must have oneOf/anyOf variants");
    variants
        .iter()
        .map(|v| {
            v.get("properties")
                .and_then(Json::as_object)
                .expect("variant has properties")
                .keys()
                .cloned()
                .collect()
        })
        .collect()
}

#[test]
fn ggen_config_root_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "GgenConfig"),
        struct_fields::<GgenConfig>(),
        "GgenConfig field set drifted between schema/ggen-toml-schema.ttl and the struct"
    );
}

#[test]
fn project_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "Project"),
        struct_fields::<Project>(),
        "Project field set drifted between schema/ggen-toml-schema.ttl and the struct"
    );
}

#[test]
fn ontology_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "Ontology"),
        struct_fields::<Ontology>(),
        "Ontology field set drifted between schema/ggen-toml-schema.ttl and the struct"
    );
}

#[test]
fn templates_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "Templates"),
        struct_fields::<Templates>(),
        "Templates field set drifted between schema/ggen-toml-schema.ttl and the struct"
    );
}

#[test]
fn pack_ref_variants_match_struct() {
    let graph = load_schema();
    let declared_path = declared_fields(&graph, "PackRef-Path");
    let declared_git = declared_fields(&graph, "PackRef-Git");
    let actual_variants = pack_ref_variant_fields();

    assert!(
        actual_variants.contains(&declared_path),
        "no PackRef variant in the struct matches TTL's PackRef-Path field set {declared_path:?}; actual variants: {actual_variants:?}"
    );
    assert!(
        actual_variants.contains(&declared_git),
        "no PackRef variant in the struct matches TTL's PackRef-Git field set {declared_git:?}; actual variants: {actual_variants:?}"
    );
    assert_eq!(
        actual_variants.len(),
        2,
        "PackRef struct has a different number of variants than the TTL declares (expected 2: Path, Git)"
    );
}
