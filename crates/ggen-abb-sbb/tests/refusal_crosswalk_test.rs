//! WO-05 (v26.11.3, lane G5) — refusal crosswalk court.
//!
//! Text-level court (precedent: ggen-engine's refusal crosswalk courts,
//! e.g. the envelope_refusal_court.rs family): parses the L13 SHACL
//! registry (ontologies/shapes/boundary-refusal.shacl.ttl) br:code set and
//! the generated vocabulary module (crates/ggen-abb-sbb/src/generated/refusals.rs)
//! and asserts set equality in BOTH directions:
//!
//! 1. every registry br:code appears as a generated wire code (the
//!    `Code::X => "<wire>"` match arms; variant and wire are 1:1);
//! 2. every generated wire code is registered in the TTL;
//! 3. every wire code has a `pub fn` constructor with the canonical
//!    snake_case name (dash -> `_`, lowercased);
//! 4. every registry code has a VALUES row in the root ggen.toml rule
//!    `abb-sbb-typed-boundary-refusals` VALUES table — guards the curated variant
//!    mapping against registry drift (a registry code absent from the
//!    rule's VALUES table silently drops out of the render; this arm makes
//!    that loud).
//!
//! Chicago discipline: real files, real parsing, assertions on final state.

use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("manifest two levels under repo root")
        .to_path_buf()
}

fn ttl_codes() -> BTreeSet<String> {
    let path = repo_root().join("ontologies/shapes/boundary-refusal.shacl.ttl");
    let text = fs::read_to_string(&path).expect("read boundary-refusal.shacl.ttl");
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            line.strip_prefix("br:code \"")
                .map(|rest| rest.split('"').next().unwrap_or("").to_string())
        })
        .filter(|c| !c.is_empty())
        .collect()
}

fn generated_wire_codes() -> BTreeSet<String> {
    let path = repo_root().join("crates/ggen-abb-sbb/src/generated/refusals.rs");
    let text = fs::read_to_string(&path).expect("read generated refusals.rs");
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            line.strip_prefix("Code::")
                .and_then(|rest| rest.split_once(" => \""))
                .map(|(_, wire)| wire.split('"').next().unwrap_or("").to_string())
        })
        .filter(|c| !c.is_empty())
        .collect()
}

fn generated_ctors() -> BTreeSet<String> {
    let path = repo_root().join("crates/ggen-abb-sbb/src/generated/refusals.rs");
    let text = fs::read_to_string(&path).expect("read generated refusals.rs");
    text.lines()
        .filter_map(|line| {
            let line = line.trim();
            line.strip_prefix("pub fn ")
                .map(|rest| rest.split('(').next().unwrap_or("").to_string())
        })
        .filter(|c| {
            !c.is_empty() && *c != "new" && *c != "code_str" && *c != "detail" && *c != "emit"
        })
        .collect()
}

fn rule_values_codes() -> BTreeSet<String> {
    let path = rule_values_path();
    let text = fs::read_to_string(&path).expect("read ggen.toml");
    let start = text
        .find("abb-sbb-typed-boundary-refusals")
        .expect("rule abb-sbb-typed-boundary-refusals present in ggen.toml");
    let section = &text[start..];
    let start = section.find("VALUES").expect("VALUES table present");
    let end = section.find("\n  }\n").expect("VALUES table closed");
    let table = &section[start..end];
    let mut out = BTreeSet::new();
    for line in table.lines() {
        if let Some(rest) = line.trim().strip_prefix("(\"") {
            if let Some((code, _)) = rest.split_once('"') {
                out.insert(code.to_string());
            }
        }
    }
    out
}

fn rule_values_path() -> PathBuf {
    repo_root().join("ggen.toml")
}

#[test]
fn court_registry_vs_generated_both_directions() {
    let ttl = ttl_codes();
    let gen = generated_wire_codes();
    let missing_from_generated: Vec<_> = ttl.difference(&gen).collect();
    let extra_in_generated: Vec<_> = gen.difference(&ttl).collect();
    assert!(
        missing_from_generated.is_empty(),
        "registry codes missing from generated refusals.rs: {missing_from_generated:?}"
    );
    assert!(
        extra_in_generated.is_empty(),
        "generated wire codes not in registry: {extra_in_generated:?}"
    );
}

#[test]
fn court_registry_vs_rule_values_both_directions() {
    let ttl = ttl_codes();
    let values = rule_values_codes();
    let missing: Vec<_> = ttl.difference(&values).collect();
    let extra: Vec<_> = values.difference(&ttl).collect();
    assert!(
        missing.is_empty(),
        "registry codes missing from ggen.toml VALUES table: {missing:?}"
    );
    assert!(
        extra.is_empty(),
        "VALUES rows not backed by registry codes: {extra:?}"
    );
}

#[test]
fn court_every_wire_code_has_ctor() {
    let gen_wire = generated_wire_codes();
    let ctors = generated_ctors();
    for code in &gen_wire {
        // Canonical ctor law: leading surface prefix (E-, FD-) stripped, then
        // dash -> underscore, lowercased. Witnessed: E-ENVELOPE-UNDECLARED ->
        // envelope_undeclared, FD-UNDECLARED_INPUT_DIALECT ->
        // undeclared_input_dialect; unprefixed codes pass through unchanged.
        let stripped = code
            .strip_prefix("E-")
            .or_else(|| code.strip_prefix("FD-"))
            .unwrap_or(code);
        let ctor = stripped.to_lowercase().replace('-', "_");
        assert!(
            ctors.contains(&ctor),
            "wire code {code} has no canonical ctor {ctor}"
        );
    }
}
