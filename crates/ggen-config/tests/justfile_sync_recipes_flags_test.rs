//! Contract-drift guard: the `just sync` / `just sync-dry` recipes may only pass flags
//! that the live `ggen sync run` verb actually accepts. The accepted flag set is derived
//! from the verb's own `fn sync_run(...)` parameter list in
//! `crates/ggen-engine/src/verbs/sync.rs` (snake_case param -> `--kebab-case` switch),
//! never hardcoded here. Past failure: recipes passed `--audit true` / `--dry_run true`.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn live_verb_flags() -> Vec<String> {
    let src = std::fs::read_to_string(repo_root().join("crates/ggen-engine/src/verbs/sync.rs"))
        .expect("read live sync verb source");
    let start = src.find("fn sync_run(").expect("sync_run fn present");
    let rest = &src[start + "fn sync_run(".len()..];
    let params = &rest[..rest.find(')').expect("param list closes")];
    params
        .split(',')
        .filter_map(|p| p.split(':').next())
        .map(str::trim)
        .filter(|n| !n.is_empty())
        .map(|n| format!("--{}", n.replace('_', "-")))
        .collect()
}

fn recipe_body(justfile: &str, name: &str) -> Vec<String> {
    let header = format!("{name}:");
    let mut lines = justfile.lines().skip_while(|l| *l != header);
    assert_eq!(lines.next(), Some(header.as_str()), "recipe `{name}` missing");
    lines
        .take_while(|l| l.starts_with(' ') || l.starts_with('\t'))
        .map(|l| l.trim().to_string())
        .collect()
}

/// Validates the recipe uses only live-verb flags; returns whether it passes `--dry-run`.
fn check(name: &str) -> bool {
    let justfile = std::fs::read_to_string(repo_root().join("justfile")).expect("read justfile");
    let accepted = live_verb_flags();
    assert!(accepted.contains(&"--dry-run".to_string()), "derived flags: {accepted:?}");
    let body = recipe_body(&justfile, name);
    assert_eq!(body.len(), 1, "recipe `{name}` must be a single command: {body:?}");
    let toks: Vec<&str> = body[0].split_whitespace().collect();
    let pos = toks.iter().position(|t| *t == "sync").expect("`sync` noun present");
    assert_eq!(toks.get(pos + 1), Some(&"run"), "must call `sync run`: {}", body[0]);
    let flags: Vec<&&str> = toks[pos + 2..].iter().collect();
    for t in &flags {
        assert!(t.starts_with("--"), "bare value `{t}` passed to switch-only verb: {}", body[0]);
        assert!(accepted.contains(&t.to_string()), "flag `{t}` not accepted by live verb {accepted:?}");
    }
    flags.iter().any(|t| **t == "--dry-run")
}

#[test]
fn just_sync_uses_only_live_verb_flags() {
    assert!(!check("sync"), "`just sync` must run the full pipeline, not --dry-run");
}

#[test]
fn just_sync_dry_uses_only_live_verb_flags() {
    assert!(check("sync-dry"), "`just sync-dry` must pass --dry-run");
}

#[test]
fn repo_facts_no_longer_claims_sync_recipes_broken() {
    let ttl = std::fs::read_to_string(repo_root().join(".specify/repo-facts.ttl")).expect("read ttl");
    for cmd in ["`just sync`", "`just sync-dry`"] {
        let at = ttl.find(&format!("rf:command \"{cmd}\"")).expect("recipe row present");
        let row = &ttl[at..at + ttl[at..].find(" .\n").unwrap()];
        assert!(!row.contains("currently broken"), "stale 'broken' claim: {row}");
    }
}
