//! Proves that every `ggenspec:pathSafe`/`ggenspec:nonEmpty` constraint
//! declared in `schema/ggen-toml-schema.ttl` is actually enforced by
//! `crate::config`'s `star_toml::Validate` impls — not just documented.
//!
//! Each test constructs a `ggen.toml` document that violates exactly one
//! declared constraint and asserts `GgenConfig::from_toml_str` rejects it
//! with `[FM-CONFIG-003]`. A companion positive test confirms a fully valid
//! document loads successfully, so the negative cases aren't vacuous (e.g.
//! failing for an unrelated syntax reason).

use ggen_engine::config::GgenConfig;

const VALID: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology/domain.ttl"

[templates]
dir = "templates"
"#;

#[test]
fn valid_document_loads() {
    let cfg = GgenConfig::from_toml_str(VALID).expect("valid document must load");
    assert_eq!(cfg.project.name, "demo");
}

#[test]
fn empty_project_name_is_rejected() {
    let toml = VALID.replace(r#"name = "demo""#, r#"name = """#);
    let err = GgenConfig::from_toml_str(&toml).expect_err("empty project.name must fail");
    assert!(err.to_string().contains("FM-CONFIG-003"), "{err}");
    assert!(
        err.to_string().contains("project.name") || err.to_string().contains("name"),
        "{err}"
    );
}

#[test]
fn ontology_source_path_traversal_is_rejected() {
    let toml = VALID.replace(
        r#"source = "ontology/domain.ttl""#,
        r#"source = "../escape.ttl""#,
    );
    let err =
        GgenConfig::from_toml_str(&toml).expect_err("path traversal in ontology.source must fail");
    assert!(err.to_string().contains("FM-CONFIG-003"), "{err}");
}

#[test]
fn templates_dir_path_traversal_is_rejected() {
    let toml = VALID.replace(r#"dir = "templates""#, r#"dir = "../../etc""#);
    let err =
        GgenConfig::from_toml_str(&toml).expect_err("path traversal in templates.dir must fail");
    assert!(err.to_string().contains("FM-CONFIG-003"), "{err}");
}

#[test]
fn pack_path_traversal_to_sibling_directory_is_allowed() {
    // Pack paths legitimately reference sibling directories one level up
    // (e.g. `../clap-noun-verb-pack`) — this is the real, tested convention
    // in tests/cross_pack_matrix.rs and every consumer pack surveyed for
    // this ticket, not a traversal attack. Confirms the earlier, incorrect
    // `check_path` traversal rejection was removed.
    let toml = format!("{VALID}\n[packs.sibling]\npath = \"../sibling-pack\"\n");
    GgenConfig::from_toml_str(&toml).expect("sibling-directory pack path must load");
}

#[test]
fn pack_path_empty_is_rejected() {
    let toml = format!("{VALID}\n[packs.evil]\npath = \"\"\n");
    let err = GgenConfig::from_toml_str(&toml).expect_err("empty packs.*.path must fail");
    assert!(err.to_string().contains("FM-CONFIG-003"), "{err}");
}

#[test]
fn pack_git_empty_version_is_rejected() {
    let toml =
        format!("{VALID}\n[packs.evil]\ngit = \"https://example.com/pack.git\"\nversion = \"\"\n");
    let err = GgenConfig::from_toml_str(&toml).expect_err("empty packs.*.version must fail");
    assert!(err.to_string().contains("FM-CONFIG-003"), "{err}");
}
