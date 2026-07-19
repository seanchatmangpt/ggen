//! specs/014-ggen-core-replacement, T070: `schema/ggen-manifest-schema.ttl` is the
//! closed-vocabulary source of truth for the `ggen_config::manifest::{GenerationConfig,
//! GenerationRule, QuerySource, TemplateSource, GenerationMode}` types this crate's
//! declarative `[[generation.rules]]` sync path (`crate::generation_rules`) consumes. Same
//! `schemars::schema_for!`-vs-TTL drift-proof contract already used by
//! `tests/ggen_toml_schema_match.rs` for `crate::config::GgenConfig`.
//!
//! Deliberately independent of that file's own `struct_fields`/`pack_ref_variant_fields`
//! helpers (a small, intentional duplication rather than a generalization of the existing
//! regression guard) so this new test cannot regress a file the task's own instructions name
//! as a must-stay-green guard.
//!
//! If a field is added to a struct without updating the TTL (or vice versa), this test
//! fails -- that is the gate, not the documentation.

use std::collections::BTreeSet;

use ggen_config::manifest::{
    GenerationConfig, GenerationMode, GenerationRule, QuerySource, TemplateSource,
};
use ggen_engine::graph::DeterministicGraph;
use oxigraph::sparql::QueryResults;
use schemars::schema_for;
use serde_json::Value as Json;

const SCHEMA_TTL: &str = include_str!("../schema/ggen-manifest-schema.ttl");

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

/// Query the variant-name set declared for a fieldless externally-tagged enum
/// (`ggenspec:variantName`, distinct from `ggenspec:hasField` -- see `GenerationMode`'s
/// TTL entry).
fn declared_variant_names(graph: &DeterministicGraph, local_name: &str) -> BTreeSet<String> {
    let query = format!(
        r#"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?name WHERE {{
            ggenspec:{local_name} ggenspec:variantName ?name .
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

/// The top-level property-name set of a `schemars`-generated JSON Schema for `T`.
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

/// The property-name sets of each struct-shaped variant of an untagged enum. `schemars`
/// represents an untagged enum's variants under `oneOf`/`anyOf`, each with its own
/// `properties` object -- a generalization (over any `T`) of
/// `tests/ggen_toml_schema_match.rs`'s `pack_ref_variant_fields`, kept as an independent copy
/// here per this file's own doc comment.
fn untagged_variant_fields<T: schemars::JsonSchema>() -> Vec<BTreeSet<String>> {
    let schema = schema_for!(T);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    let variants = json
        .get("oneOf")
        .or_else(|| json.get("anyOf"))
        .and_then(Json::as_array)
        .unwrap_or_else(|| {
            panic!(
                "{} schema must have oneOf/anyOf variants",
                std::any::type_name::<T>()
            )
        });
    variants
        .iter()
        .map(|v| {
            v.get("properties")
                .and_then(Json::as_object)
                .unwrap_or_else(|| panic!("variant has no properties: {v:?}"))
                .keys()
                .cloned()
                .collect()
        })
        .collect()
}

/// The variant-name set of a fieldless externally-tagged enum. Verified (not assumed) via
/// `schema_probe`: `schemars` 1.x represents this shape as a top-level `oneOf` array of
/// `{"type": "string", "const": "<Variant>"}` entries, not a bare top-level `"enum"` array.
fn enum_variant_names<T: schemars::JsonSchema>() -> BTreeSet<String> {
    let schema = schema_for!(T);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    json.get("oneOf")
        .and_then(Json::as_array)
        .unwrap_or_else(|| {
            panic!(
                "{} schema must have a top-level oneOf array of const-string variants",
                std::any::type_name::<T>()
            )
        })
        .iter()
        .map(|v| {
            v.get("const")
                .and_then(Json::as_str)
                .unwrap_or_else(|| panic!("oneOf entry has no `const` string: {v:?}"))
                .to_string()
        })
        .collect()
}

#[test]
fn generation_config_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "GenerationConfig"),
        struct_fields::<GenerationConfig>(),
        "GenerationConfig field set drifted between schema/ggen-manifest-schema.ttl and the struct"
    );
}

#[test]
fn generation_rule_fields_match_struct() {
    let graph = load_schema();
    assert_eq!(
        declared_fields(&graph, "GenerationRule"),
        struct_fields::<GenerationRule>(),
        "GenerationRule field set drifted between schema/ggen-manifest-schema.ttl and the struct"
    );
}

#[test]
fn query_source_variants_match_struct() {
    let graph = load_schema();
    let declared_pack = declared_fields(&graph, "QuerySource-Pack");
    let declared_file = declared_fields(&graph, "QuerySource-File");
    let declared_inline = declared_fields(&graph, "QuerySource-Inline");
    let actual_variants = untagged_variant_fields::<QuerySource>();

    assert!(
        actual_variants.contains(&declared_pack),
        "no QuerySource variant matches TTL's QuerySource-Pack field set {declared_pack:?}; actual: {actual_variants:?}"
    );
    assert!(
        actual_variants.contains(&declared_file),
        "no QuerySource variant matches TTL's QuerySource-File field set {declared_file:?}; actual: {actual_variants:?}"
    );
    assert!(
        actual_variants.contains(&declared_inline),
        "no QuerySource variant matches TTL's QuerySource-Inline field set {declared_inline:?}; actual: {actual_variants:?}"
    );
    assert_eq!(
        actual_variants.len(),
        3,
        "QuerySource struct has a different number of variants than the TTL declares (expected 3: Pack, File, Inline)"
    );
}

#[test]
fn template_source_variants_match_struct() {
    let graph = load_schema();
    let declared_pack = declared_fields(&graph, "TemplateSource-Pack");
    let declared_file = declared_fields(&graph, "TemplateSource-File");
    let declared_inline = declared_fields(&graph, "TemplateSource-Inline");
    let declared_git = declared_fields(&graph, "TemplateSource-Git");
    let declared_package = declared_fields(&graph, "TemplateSource-Package");
    let actual_variants = untagged_variant_fields::<TemplateSource>();

    for (label, declared) in [
        ("Pack", &declared_pack),
        ("File", &declared_file),
        ("Inline", &declared_inline),
        ("Git", &declared_git),
        ("Package", &declared_package),
    ] {
        assert!(
            actual_variants.contains(declared),
            "no TemplateSource variant matches TTL's TemplateSource-{label} field set {declared:?}; actual: {actual_variants:?}"
        );
    }
    assert_eq!(
        actual_variants.len(),
        5,
        "TemplateSource struct has a different number of variants than the TTL declares (expected 5: Pack, File, Inline, Git, Package)"
    );
}

#[test]
fn generation_mode_variant_names_match_enum() {
    let graph = load_schema();
    assert_eq!(
        declared_variant_names(&graph, "GenerationMode"),
        enum_variant_names::<GenerationMode>(),
        "GenerationMode variant-name set drifted between schema/ggen-manifest-schema.ttl and the enum"
    );
}
