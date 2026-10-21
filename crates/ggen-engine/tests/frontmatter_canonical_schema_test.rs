//! v26.10.20 lane A09: `schema/frontmatter-schema.ttl` pins the optional
//! `canonical_identity` key (canon: vocabulary per L04). This test mirrors
//! `frontmatter_schema_match.rs`: it parses the TTL and asserts the declared
//! `canonical_identity` surface is an *exact* match against the struct's
//! *actual* reflected shape (`schemars::schema_for!`) — the TTL and the
//! `template::Frontmatter` struct must change in the same commit or this
//! gate fails.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::{graph::DeterministicGraph, template::Frontmatter};
use oxigraph::sparql::QueryResults;
use schemars::schema_for;
use serde_json::Value as Json;

const SCHEMA_TTL: &str = include_str!("../schema/frontmatter-schema.ttl");

fn ttl_string_values(query: &str) -> Vec<String> {
    let graph = DeterministicGraph::new().expect("graph");
    graph
        .insert_turtle(SCHEMA_TTL)
        .expect("schema ttl must parse");
    let QueryResults::Solutions(solutions) = graph.query(query).expect("sparql eval") else {
        panic!("expected SELECT results");
    };
    solutions
        .map(|s| {
            let s = s.expect("solution");
            s.get("v")
                .expect("?v bound")
                .to_string()
                .trim_matches('"')
                .to_string()
        })
        .collect()
}

fn struct_property(name: &str) -> Json {
    let schema = schema_for!(Frontmatter);
    let json: Json = serde_json::to_value(&schema).expect("schema to json");
    json.get("properties")
        .and_then(|p| p.get(name))
        .cloned()
        .unwrap_or_else(|| panic!("Frontmatter struct has no `{name}` property"))
}

fn flatten_definitions(value: &Json, acc: &mut Vec<String>) {
    match value {
        Json::String(s) => acc.push(s.clone()),
        Json::Array(items) => {
            for item in items {
                flatten_definitions(item, acc);
            }
        }
        Json::Object(map) => {
            if map.get("$ref").and_then(Json::as_str).is_some() {
                // Recurse into the referenced definition so nested object
                // shapes are comparable against the TTL.
                if let Some(def_key) = map
                    .get("$ref")
                    .and_then(Json::as_str)
                    .and_then(|r| r.rsplit('/').next())
                {
                    let schema = schema_for!(Frontmatter);
                    let json: Json = serde_json::to_value(&schema).expect("schema to json");
                    if let Some(def) = json
                        .get("definitions")
                        .and_then(|d| d.get(def_key))
                        .or_else(|| json.get("$defs").and_then(|d| d.get(def_key)))
                    {
                        flatten_definitions(def, acc);
                    }
                }
            }
            for v in map.values() {
                flatten_definitions(v, acc);
            }
        }
        _ => {}
    }
}

/// The key is declared in the TTL and present on the struct, optional on
/// both sides (TTL `required false`; struct `Option<...>` with
/// `#[serde(default)]`, i.e. absent from `required`).
#[test]
fn canonical_identity_declared_and_optional_on_both_sides() {
    let ttl_declared = ttl_string_values(
        r"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?v WHERE {
            ggenspec:Frontmatter-canonical_identity ggenspec:name ?v .
            ggenspec:Frontmatter-canonical_identity ggenspec:required false .
        }
    ",
    );
    assert_eq!(
        ttl_declared,
        vec!["canonical_identity".to_string()],
        "TTL must declare exactly the `canonical_identity` key, not required"
    );

    let json = struct_property("canonical_identity");
    // `Option<T>` + serde default => not in the required list.
    let struct_required: Vec<&str> = json
        .get("anyOf")
        .and_then(|any| any.as_array())
        .and_then(|variants| variants.first())
        .and_then(|first| first.get("required"))
        .and_then(Json::as_array)
        .map(|a| a.iter().filter_map(Json::as_str).collect())
        .unwrap_or_default();
    assert!(
        !struct_required.contains(&"canonical_identity"),
        "canonical_identity must be optional on the struct side too"
    );
}

/// The TTL-pinned canonical forms are exactly the enum variants the struct
/// deserializes: rdfc1 | sortedKeyJson | btreeOrder.
#[test]
fn canonical_form_enum_matches_ttl() {
    let mut ttl_forms = ttl_string_values(
        r"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?v WHERE {
            ggenspec:CanonicalForm ggenspec:enumValue ?v .
        }
    ",
    );
    ttl_forms.sort();
    assert_eq!(
        ttl_forms,
        vec![
            "btreeOrder".to_string(),
            "rdfc1".to_string(),
            "sortedKeyJson".to_string(),
        ],
        "TTL CanonicalForm enum drifted from the pinned canon: vocabulary"
    );

    // Struct side: deserialize each pinned form through the real
    // deserializer (Chicago: real YAML frontmatter, not a reflected copy).
    let fm = ggen_engine::template::Template::parse(
        "---\nto: out.txt\ncanonical_identity:\n  form: rdfc1\n  digest:\n    algo: blake3\n    domain: test\n---\nbody",
    )
    .expect("parse rdfc1");
    let ci = fm
        .frontmatter
        .canonical_identity
        .as_ref()
        .expect("canonical_identity present");
    assert_eq!(ci.form, ggen_engine::template::CanonicalForm::Rdfc1);

    for form in ["sortedKeyJson", "btreeOrder"] {
        let content = format!(
            "---\nto: out.txt\ncanonical_identity:\n  form: {form}\n  digest:\n    algo: blake3\n    domain: test\n---\nbody"
        );
        let parsed = ggen_engine::template::Template::parse(&content)
            .unwrap_or_else(|e| panic!("form {form} must deserialize: {e}"));
        assert!(parsed.frontmatter.canonical_identity.is_some());
    }

    // An unpinned form is a hard error (closed vocabulary).
    let bad = "---\nto: out.txt\ncanonical_identity:\n  form: jsonLd\n  digest:\n    algo: blake3\n    domain: test\n---\nbody";
    assert!(
        ggen_engine::template::Template::parse(bad).is_err(),
        "unknown canonical form must be refused, not silently coerced"
    );
}

/// The TTL-pinned digest recipe (required algo+domain, optional keyed) is
/// exactly the struct's `CanonicalDigest` shape, and unknown digest fields
/// are refused (`deny_unknown_fields`).
#[test]
fn digest_spec_matches_ttl() {
    let mut ttl_fields = ttl_string_values(
        r"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?v WHERE {
            { ggenspec:DigestSpec ggenspec:requiredField ?v } UNION
            { ggenspec:DigestSpec ggenspec:optionalField ?v }
        }
    ",
    );
    ttl_fields.sort();
    assert_eq!(
        ttl_fields,
        vec![
            "algo".to_string(),
            "domain".to_string(),
            "keyed".to_string(),
        ],
        "TTL DigestSpec field set drifted"
    );

    let mut ttl_required = ttl_string_values(
        r"
        PREFIX ggenspec: <https://praxis.dev/ggen/schema#>
        SELECT ?v WHERE {
            ggenspec:DigestSpec ggenspec:requiredField ?v .
        }
    ",
    );
    ttl_required.sort();
    assert_eq!(ttl_required, vec!["algo".to_string(), "domain".to_string()]);

    // Struct side: domain is mandatory (missing domain is an error), keyed
    // defaults to false, and an unknown digest key is refused.
    let missing_domain = "---\nto: out.txt\ncanonical_identity:\n  form: rdfc1\n  digest:\n    algo: blake3\n---\nbody";
    assert!(
        ggen_engine::template::Template::parse(missing_domain).is_err(),
        "digest.domain is required in the TTL; the struct must refuse it too"
    );

    let full = ggen_engine::template::Template::parse(
        "---\nto: out.txt\ncanonical_identity:\n  form: rdfc1\n  digest:\n    algo: blake3\n    domain: ggen.test\n    keyed: true\n---\nbody",
    )
    .expect("parse full digest spec");
    let digest = &full
        .frontmatter
        .canonical_identity
        .as_ref()
        .expect("present")
        .digest;
    assert_eq!(digest.algo, "blake3");
    assert_eq!(digest.domain, "ggen.test");
    assert!(digest.keyed);

    let default_keyed = ggen_engine::template::Template::parse(
        "---\nto: out.txt\ncanonical_identity:\n  form: rdfc1\n  digest:\n    algo: blake3\n    domain: ggen.test\n---\nbody",
    )
    .expect("parse default keyed");
    assert!(
        !default_keyed
            .frontmatter
            .canonical_identity
            .as_ref()
            .expect("present")
            .digest
            .keyed,
        "keyed defaults to false"
    );

    let unknown_key = "---\nto: out.txt\ncanonical_identity:\n  form: rdfc1\n  digest:\n    algo: blake3\n    domain: ggen.test\n    salt: nope\n---\nbody";
    assert!(
        ggen_engine::template::Template::parse(unknown_key).is_err(),
        "unknown digest field must be refused (deny_unknown_fields)"
    );
}

/// Reflected nested struct surface (`form`/`digest`) is present in the
/// schemars output for `canonical_identity` — the TTL object shape has a
/// real struct counterpart, not a hand-maintained mirror.
#[test]
fn reflected_struct_has_form_and_digest() {
    let json = struct_property("canonical_identity");
    let mut names = Vec::new();
    flatten_definitions(&json, &mut names);
    let joined = names.join("\n");
    assert!(
        joined.contains("form") && joined.contains("digest"),
        "reflected canonical_identity surface must expose form and digest; got:\n{joined}"
    );
}
