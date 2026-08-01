//! Physical-fixture proof for `ggen_config::classify_ggen_toml`'s three-way
//! outcome (declarative-rules / frontmatter / ambiguous) over real files on
//! disk, complementing the inline-string coverage already in
//! `crates/ggen-config/src/config_schema.rs`'s own `#[cfg(test)]` module and
//! `crates/ggen-engine/tests/config_schema_dispatch_e2e.rs`'s full `ggen
//! sync`/`ggen doctor` process-boundary tests.
//!
//! `schema_dual_ambiguous_collision.toml` is not a synthetic worst case: it
//! reproduces the exact structural shape found in the real
//! `examples/tai-enterprise-rebuild/ggen.toml` (`[project].version` present
//! alongside a `[packs]` table-of-tables), confirmed live by running `ggen
//! doctor run` against that example directory, which fails today with
//! `[FM-CONFIG-101] ... conflicting structural markers
//! ["declarative:project_version_present", "frontmatter:packref_entry_missing_name",
//! "frontmatter:packs_table_shaped"]`.

use ggen_config::ConfigSchemaClassification;

fn read_fixture(name: &str) -> String {
    let path = format!("{}/tests/fixtures/{name}", env!("CARGO_MANIFEST_DIR"));
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read fixture {path}: {e}"))
}

#[test]
fn declarative_rules_fixture_classifies_as_declarative_rules() {
    let raw = read_fixture("schema_dual_declarative_valid.toml");
    let classification = ggen_config::classify_ggen_toml(&raw);
    assert_eq!(
        classification,
        ConfigSchemaClassification::DeclarativeRules,
        "expected DeclarativeRules, got {classification:?}"
    );
}

#[test]
fn frontmatter_fixture_classifies_as_frontmatter() {
    let raw = read_fixture("schema_dual_frontmatter_valid.toml");
    let classification = ggen_config::classify_ggen_toml(&raw);
    assert_eq!(
        classification,
        ConfigSchemaClassification::Frontmatter,
        "expected Frontmatter, got {classification:?}"
    );
}

#[test]
fn ambiguous_collision_fixture_classifies_as_ambiguous_and_refuses_loudly() {
    let raw = read_fixture("schema_dual_ambiguous_collision.toml");
    let classification = ggen_config::classify_ggen_toml(&raw);
    match &classification {
        ConfigSchemaClassification::Ambiguous { matched } => {
            assert!(
                matched
                    .iter()
                    .any(|m| m == "declarative:project_version_present"),
                "expected the declarative project-version marker in {matched:?}"
            );
            assert!(
                matched.iter().any(|m| m == "frontmatter:packs_table_shaped"),
                "expected the frontmatter packs-table marker in {matched:?}"
            );
        }
        other => panic!("expected Ambiguous, got {other:?}"),
    }
    assert_eq!(classification.code(), ggen_config::CONFIG_SCHEMA_AMBIGUOUS);
}
