//! v26.10.10 §4.1 net-new schema sections: `[rules]` (n3/datalog rule-file
//! references) and `[pack_sources]` (external pack source bindings) on the
//! DeclarativeRules schema (`crate::manifest::GgenManifest`).
//!
//! Hard invariant: both sections are OPTIONAL — every legacy manifest parses
//! unchanged, and the structural classifier's verdict on existing fixtures is
//! untouched.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_config::manifest::{GgenManifest, ManifestParser};
use ggen_config::ConfigSchemaClassification;

const MINIMAL_MANIFEST: &str = r#"
[project]
name = "my-domain"
version = "1.0.0"

[ontology]
source = "domain/model.ttl"

[[generation.rules]]
name = "structs"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "src/models/out.rs"
"#;

// -- (a) present sections parse and round-trip ------------------------------

#[test]
fn manifest_with_rules_and_pack_sources_parses_and_round_trips() {
    let raw = format!(
        "{MINIMAL_MANIFEST}\n\
         [rules]\nn3 = [\"rules/base.n3\", \"rules/extra.n3\"]\ndatalog = [\"rules/eval.dl\"]\n\n\
         [pack_sources.core]\nsource = \"path\"\nlocation = \"../core-pack\"\n\n\
         [pack_sources.remote]\nsource = \"git\"\nlocation = \"https://example.com/pack.git\"\n"
    );
    let manifest: GgenManifest = ManifestParser::parse_str(&raw).expect("parses");
    let rules = manifest.rules.as_ref().expect("[rules] present");
    assert_eq!(
        rules.n3,
        vec![
            std::path::PathBuf::from("rules/base.n3"),
            std::path::PathBuf::from("rules/extra.n3")
        ]
    );
    assert_eq!(
        rules.datalog,
        vec![std::path::PathBuf::from("rules/eval.dl")]
    );

    let sources = manifest
        .pack_sources
        .as_ref()
        .expect("[pack_sources] present");
    assert_eq!(sources.len(), 2);
    let core = sources.get("core").expect("core binding");
    assert!(matches!(
        core.source,
        ggen_config::manifest::PackSourceKind::Path
    ));
    assert_eq!(core.location, "../core-pack");
    let remote = sources.get("remote").expect("remote binding");
    assert!(matches!(
        remote.source,
        ggen_config::manifest::PackSourceKind::Git
    ));
    assert_eq!(remote.location, "https://example.com/pack.git");

    // Round-trip: serialize back and re-parse, fields survive.
    let reserialized = toml::to_string(&manifest).expect("serializes");
    let reparsed: GgenManifest = ManifestParser::parse_str(&reserialized).expect("re-parses");
    assert_eq!(reparsed.rules, manifest.rules);
    assert_eq!(reparsed.pack_sources, manifest.pack_sources);
}

// -- (b) absent sections default empty --------------------------------------

#[test]
fn absent_rules_and_pack_sources_sections_default_to_none() {
    let manifest: GgenManifest = ManifestParser::parse_str(MINIMAL_MANIFEST).expect("parses");
    assert!(manifest.rules.is_none());
    assert!(manifest.pack_sources.is_none());
}

// -- (c) unknown field inside the new tables is refused ----------------------

#[test]
fn unknown_field_in_rules_table_is_refused() {
    let raw = format!("{MINIMAL_MANIFEST}\n[rules]\nn3 = [\"r.n3\"]\nbogus = 1\n");
    let err = ManifestParser::parse_str(&raw).expect_err("unknown field must be refused");
    assert!(
        err.to_string().to_lowercase().contains("bogus"),
        "expected the parser to name the unknown field, got: {err}"
    );
}

#[test]
fn unknown_field_in_pack_source_binding_is_refused() {
    let raw = format!(
        "{MINIMAL_MANIFEST}\n[pack_sources.core]\nsource = \"path\"\nlocation = \"p\"\nbogus = 1\n"
    );
    let err = ManifestParser::parse_str(&raw).expect_err("unknown field must be refused");
    assert!(
        err.to_string().to_lowercase().contains("bogus"),
        "expected the parser to name the unknown field, got: {err}"
    );
}

#[test]
fn unknown_pack_source_kind_is_refused() {
    let raw = format!(
        "{MINIMAL_MANIFEST}\n[pack_sources.core]\nsource = \"s3\"\nlocation = \"bucket\"\n"
    );
    assert!(
        ManifestParser::parse_str(&raw).is_err(),
        "only path|git are legal"
    );
}

#[test]
fn empty_rule_path_is_refused() {
    let raw = format!("{MINIMAL_MANIFEST}\n[rules]\nn3 = [\"\"]\n");
    let manifest = ManifestParser::parse_str(&raw).expect("deserializes");
    assert!(
        star_toml::Validate::check(&manifest).is_err(),
        "empty rule-file path must fail validation"
    );
}

#[test]
fn empty_pack_source_location_is_refused() {
    let raw =
        format!("{MINIMAL_MANIFEST}\n[pack_sources.core]\nsource = \"path\"\nlocation = \"\"\n");
    let manifest = ManifestParser::parse_str(&raw).expect("deserializes");
    assert!(
        star_toml::Validate::check(&manifest).is_err(),
        "empty pack-source location must fail validation"
    );
}

// -- (d) classifier unchanged for legacy fixtures / additions -----------------

#[test]
fn classifier_declarative_fixture_with_new_tables_still_declarative_rules() {
    let raw = format!(
        "{MINIMAL_MANIFEST}\n[rules]\nn3 = [\"r.n3\"]\n\n[pack_sources.core]\nsource = \"path\"\nlocation = \"p\"\n"
    );
    assert_eq!(
        ggen_config::classify_ggen_toml(&raw),
        ConfigSchemaClassification::DeclarativeRules
    );
}

#[test]
fn classifier_existing_dual_fixtures_unaffected() {
    let fixtures = [
        (
            "schema_dual_declarative_valid.toml",
            ConfigSchemaClassification::DeclarativeRules,
        ),
        (
            "schema_dual_frontmatter_valid.toml",
            ConfigSchemaClassification::Frontmatter,
        ),
    ];
    for (name, expected) in fixtures {
        let path = format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"));
        let raw = std::fs::read_to_string(&path).expect("fixture exists");
        assert_eq!(
            ggen_config::classify_ggen_toml(&raw),
            expected,
            "fixture {name} classification changed"
        );
    }
}
