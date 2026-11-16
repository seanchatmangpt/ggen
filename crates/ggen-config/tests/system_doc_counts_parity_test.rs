//! Doc-fact parity court (sibling of `system_crate_map_parity_test.rs`): the
//! numbers `.specify/repo-facts.ttl` publishes (pack count, ggen-mcp tool count,
//! workspace version, per-crate versions) are cross-checked against their real
//! sources on disk so the next drift fails a test instead of staling generated docs.
//! Chicago TDD: real files, plain line scanning, no mocks.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn read(rel: &str) -> String {
    std::fs::read_to_string(root().join(rel)).unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

/// Value of a `rf:<pred> "..."` literal on any line of the facts file (first match).
fn fact(pred: &str) -> String {
    let key = format!("rf:{pred} \"");
    read(".specify/repo-facts.ttl")
        .lines()
        .find_map(|l| {
            let t = l.trim();
            t.strip_prefix(&key).and_then(|r| r.split('"').next()).map(str::to_string)
        })
        .unwrap_or_else(|| panic!("rf:{pred} not found in repo-facts.ttl"))
}

fn workspace_version() -> String {
    let cargo = read("Cargo.toml");
    let line = cargo
        .lines()
        .skip_while(|l| l.trim() != "[workspace.package]")
        .find(|l| l.trim_start().starts_with("version"))
        .expect("workspace.package version");
    line.split('"').nth(1).expect("quoted version").to_string()
}

#[test]
fn pack_count_fact_matches_packs_on_disk() {
    let real = std::fs::read_dir(root().join("packs"))
        .unwrap()
        .filter_map(Result::ok)
        .filter(|e| e.path().join("pack.toml").is_file())
        .count();
    assert!(real > 0, "found zero packs: parser bug");
    assert_eq!(
        fact("packCount"),
        real.to_string(),
        "rf:packCount drifted from `packs/*/pack.toml` count; fix the TTL, then `ggen sync run`"
    );
}

#[test]
fn mcp_tool_count_fact_matches_tool_defs_table() {
    let src = read("crates/ggen-mcp/src/lib.rs");
    let tools: std::collections::BTreeSet<_> = src
        .lines()
        .skip_while(|l| !l.starts_with("tool_defs! {"))
        .filter_map(|l| l.strip_prefix("    \"ggen_").and_then(|r| r.split('"').next()))
        .collect();
    assert!(!tools.is_empty(), "parsed zero tools from tool_defs!: parser bug");
    let words = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
        "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
    ];
    let word = words[tools.len() - 1];
    let ttl = read(".specify/repo-facts.ttl");
    let line = ttl
        .lines()
        .find(|l| l.contains("rf:summaryPurpose \"MCP server (`rmcp`"))
        .expect("ggen-mcp summaryPurpose line");
    assert!(
        line.contains(&format!("{word} tools")),
        "ggen-mcp has {} registered tools ({word}) but repo-facts.ttl says otherwise: {line}",
        tools.len()
    );
}

#[test]
fn workspace_version_fact_matches_cargo_toml() {
    assert_eq!(fact("workspaceVersion"), workspace_version());
}

#[test]
fn crate_version_facts_match_manifests() {
    let ws = workspace_version();
    let ttl = read(".specify/repo-facts.ttl");
    let mut dir: Option<String> = None;
    let mut checked = 0;
    for l in ttl.lines().map(str::trim) {
        if l.contains("a rf:Crate") {
            dir = None;
        }
        if let Some(r) = l.strip_prefix("rf:dir \"") {
            dir = r.split('"').next().map(str::to_string);
        }
        let (Some(d), Some(v)) = (&dir, l.strip_prefix("rf:version \"").and_then(|r| r.split('"').next()))
        else {
            continue;
        };
        let manifest = if d == "ggen" { "Cargo.toml".to_string() } else { format!("crates/{d}/Cargo.toml") };
        if !root().join(&manifest).is_file() {
            continue; // pack individuals etc.
        }
        let text = read(&manifest);
        let pkg: Vec<&str> = text.lines().skip_while(|l| l.trim() != "[package]").skip(1)
            .take_while(|l| !l.trim_start().starts_with('[')).collect();
        let inherits = text.contains("[package.version]") || pkg.iter().any(|l| l.trim().starts_with("version.workspace"));
        let literal = pkg.iter().find_map(|l| l.trim().strip_prefix("version = \"").and_then(|r| r.split('"').next()));
        let effective = if let Some(lit) = literal { lit.to_string() } else if inherits { ws.clone() } else { continue };
        let claimed = if v == "workspace" { ws.clone() } else { v.to_string() };
        assert_eq!(claimed, effective, "rf:version for crate `{d}` drifted from {manifest}");
        checked += 1;
    }
    assert!(checked >= 10, "only {checked} crate versions checked: parser bug");
}
