//! Regression backstop for the workspace version bump procedure (see CLAUDE.md /
//! `.claude/rules/architecture.md`'s Recent Changes and the version-bump task that
//! introduced this test): three independent version strings must agree —
//! `.specify/repo-facts.ttl`'s `rf:workspaceVersion`, root `Cargo.toml`'s
//! `[workspace.package] version`, and `ggen.toml`'s `[project] version`.
//!
//! These three have drifted from each other historically (workspace at 26.7.18
//! while `ggen.toml` sat at 26.7.1, and `crates/ggen-engine/Cargo.toml`'s own
//! version pin lagged even further behind at 26.7.13) precisely because nothing
//! asserted them equal. This is a real state comparison across three actual repo
//! files (via `include_str!`), not a hardcoded literal — editing any one of the
//! three files without the others fails this test.

const REPO_FACTS_TTL: &str = include_str!("../../../.specify/repo-facts.ttl");
const ROOT_CARGO_TOML: &str = include_str!("../../../Cargo.toml");
const GGEN_TOML: &str = include_str!("../../../ggen.toml");

/// Extract the quoted string literal value of `rf:workspaceVersion "X.Y.Z" ;`
/// from the repo-facts TTL.
fn extract_workspace_version_from_ttl(ttl: &str) -> String {
    let needle = "rf:workspaceVersion";
    let idx = ttl
        .find(needle)
        .unwrap_or_else(|| panic!("rf:workspaceVersion not found in repo-facts.ttl"));
    let rest = &ttl[idx + needle.len()..];
    let start = rest
        .find('"')
        .unwrap_or_else(|| panic!("no opening quote after rf:workspaceVersion"));
    let rest = &rest[start + 1..];
    let end = rest
        .find('"')
        .unwrap_or_else(|| panic!("no closing quote after rf:workspaceVersion"));
    rest[..end].to_string()
}

/// Extract the `version = "X.Y.Z"` value from `[workspace.package]` in root Cargo.toml.
fn extract_workspace_package_version(cargo_toml: &str) -> String {
    let section_idx = cargo_toml
        .find("[workspace.package]")
        .unwrap_or_else(|| panic!("[workspace.package] section not found in Cargo.toml"));
    let after_section = &cargo_toml[section_idx..];
    let version_idx = after_section
        .find("version")
        .unwrap_or_else(|| panic!("version field not found in [workspace.package]"));
    let rest = &after_section[version_idx..];
    let start = rest
        .find('"')
        .unwrap_or_else(|| panic!("no opening quote after version ="));
    let rest = &rest[start + 1..];
    let end = rest
        .find('"')
        .unwrap_or_else(|| panic!("no closing quote after version ="));
    rest[..end].to_string()
}

/// Extract the `version = "X.Y.Z"` value from `[project]` in ggen.toml.
fn extract_ggen_toml_project_version(ggen_toml: &str) -> String {
    let section_idx = ggen_toml
        .find("[project]")
        .unwrap_or_else(|| panic!("[project] section not found in ggen.toml"));
    let after_section = &ggen_toml[section_idx..];
    let version_idx = after_section
        .find("version")
        .unwrap_or_else(|| panic!("version field not found in [project]"));
    let rest = &after_section[version_idx..];
    let start = rest
        .find('"')
        .unwrap_or_else(|| panic!("no opening quote after version ="));
    let rest = &rest[start + 1..];
    let end = rest
        .find('"')
        .unwrap_or_else(|| panic!("no closing quote after version ="));
    rest[..end].to_string()
}

#[test]
fn workspace_version_matches_across_repo_facts_cargo_toml_and_ggen_toml() {
    let ttl_version = extract_workspace_version_from_ttl(REPO_FACTS_TTL);
    let cargo_version = extract_workspace_package_version(ROOT_CARGO_TOML);
    let ggen_toml_version = extract_ggen_toml_project_version(GGEN_TOML);

    assert_eq!(
        ttl_version, cargo_version,
        ".specify/repo-facts.ttl's rf:workspaceVersion ({ttl_version}) does not match \
         Cargo.toml's [workspace.package] version ({cargo_version})"
    );
    assert_eq!(
        ttl_version, ggen_toml_version,
        ".specify/repo-facts.ttl's rf:workspaceVersion ({ttl_version}) does not match \
         ggen.toml's [project] version ({ggen_toml_version})"
    );
    assert_eq!(
        cargo_version, ggen_toml_version,
        "Cargo.toml's [workspace.package] version ({cargo_version}) does not match \
         ggen.toml's [project] version ({ggen_toml_version})"
    );
}
