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

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

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

/// Parse the leading numeric `X.Y.Z` out of a version requirement that is an
/// exact self-pin (`"X.Y.Z"` bare-caret or `"=X.Y.Z"`). Range reqs (`>=`, `<`,
/// `,`, `~`, `*`) are intentionally not self-pins and return `None`.
fn exact_pin(req: &str) -> Option<&str> {
    let r = req.trim().trim_start_matches('=');
    let ok = !r.is_empty() && r.chars().all(|c| c.is_ascii_digit() || c == '.');
    if ok { Some(r) } else { None }
}

/// Drift guard (v26.11.15): every exact version pin on an in-workspace path
/// dependency (`ggen-* = { path = ..., version = "X" }` inline or in a
/// `[dependencies.ggen-*]` table) must equal the workspace version. A stale pin
/// (e.g. 26.10.11 left behind after a bump) fails the publish ladder with an
/// unresolvable-version error, so refuse it here.
#[test]
fn no_stale_exact_self_pin_on_workspace_path_dependencies() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let ws_version = extract_workspace_package_version(ROOT_CARGO_TOML);
    let mut manifests = vec![root.join("Cargo.toml")];
    for entry in std::fs::read_dir(root.join("crates")).expect("read crates/") {
        let p = entry.expect("dir entry").path().join("Cargo.toml");
        if p.is_file() {
            manifests.push(p);
        }
    }
    let mut stale = Vec::new();
    let mut checked = 0usize;
    for m in &manifests {
        let text = std::fs::read_to_string(m).expect("read manifest");
        // Section header of a `[dependencies.ggen-x]` style table, if inside one.
        let mut table_is_ws_dep = false;
        let mut table_has_path = false;
        for line in text.lines() {
            let l = line.trim();
            if l.starts_with('[') {
                table_is_ws_dep = l.contains("dependencies.ggen-");
                table_has_path = false;
                continue;
            }
            let (is_dep_line, version_src): (bool, Option<&str>) = if table_is_ws_dep {
                if l.starts_with("path") {
                    table_has_path = true;
                }
                (table_has_path || l.starts_with("version"), l.strip_prefix("version"))
            } else {
                (
                    l.starts_with("ggen-") && l.contains("path =") && l.contains("version ="),
                    l.find("version =").map(|i| &l[i + "version".len()..]),
                )
            };
            if !is_dep_line {
                continue;
            }
            let Some(src) = version_src else { continue };
            let Some(q1) = src.find('"') else { continue };
            let rest = &src[q1 + 1..];
            let Some(q2) = rest.find('"') else { continue };
            if let Some(pin) = exact_pin(&rest[..q2]) {
                checked += 1;
                if pin != ws_version {
                    stale.push(format!("{}: `{}` pins {pin}", m.display(), l));
                }
            }
        }
    }
    assert!(checked >= 8, "scanner found only {checked} exact self-pins; parser regressed");
    assert!(
        stale.is_empty(),
        "stale exact self-pins (workspace version is {ws_version}):\n{}",
        stale.join("\n")
    );
}
