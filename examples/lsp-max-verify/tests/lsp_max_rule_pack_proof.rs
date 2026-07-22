//! Proof tests for `lsp-max-pack`'s generated rule-pack artifacts
//! (`rules/lsp_max_*.toml` fan-out files + `docs/lsp_max_rule_pack.md`
//! catalog), asserted against literal values hand-transcribed from
//! `packs/lsp-max-pack/ontology.ttl`.
//!
//! Deliberately NO `sparql:` frontmatter key in this template (see
//! `rule_pack.toml.tmpl` / `rule_pack_index.md.tmpl` for the queries that
//! *do* exist): re-deriving the "expected" side via the same SPARQL query
//! that produced those files would make every assertion a tautology
//! (actual == actual, computed twice). Every literal below was
//! transcribed by hand from the ontology, never queried.
//!
//! This pack generates config data (TOML rule files + a markdown catalog)
//! consumed by an external `lsp-max` `RulePackServer`, not a Rust module --
//! so there is nothing to `#[path = "..."]` mount. Instead these tests
//! read the real generated files off disk (relative to the crate root,
//! `cargo test`'s working directory) with the real `toml` and `regex`
//! crates as genuine Chicago-TDD collaborators, and additionally exercise
//! each rule's `pattern` as a real compiled regex against literal
//! positive/negative code snippets -- proving the pattern actually
//! catches (and doesn't over-catch) the code smell it claims to.
//!
//! Fidelity note (verified 2026-07-18 against `~/lsp-max/src/rule_pack_server.rs`,
//! the real target this pack's output is consumed by): the generated
//! `[[rules]]` shape (`id`, `name`, `severity`, `pattern`, `path_globs`,
//! `exclude_globs`, `message`, `rationale`, `eval_budget`) matches
//! `rule_pack_server.rs::Rule` field-for-field, and its `evaluate()`
//! function compiles `pattern` with `regex::Regex` and calls
//! `Regex::find_iter` per line -- there is no AhoCorasick engine anywhere
//! in that path. `ParsedRule` below is intentionally a hand-written
//! structural echo of that real `Rule` type (not an import of it: this
//! generated-artifacts crate has no path/git dependency on `~/lsp-max`),
//! so a field this pack emits but the real struct doesn't have (or vice
//! versa) still won't be caught by `toml::from_str` alone -- this is the
//! honest boundary of what a config-file-shape proof can certify without
//! depending on the real external crate.

use std::fs;
use std::path::Path;

use regex::Regex;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct ParsedRule {
    id: String,
    name: String,
    severity: String,
    pattern: String,
    path_globs: Vec<String>,
    exclude_globs: Vec<String>,
    message: String,
    rationale: String,
    eval_budget: String,
}

#[derive(Debug, Deserialize)]
struct ParsedRulePackFile {
    rules: Vec<ParsedRule>,
}

struct Expected {
    id: &'static str,
    name: &'static str,
    severity: &'static str,
    pattern: &'static str,
    eval_budget: &'static str,
    message: &'static str,
    rationale: &'static str,
    positive: &'static [&'static str],
    negative: &'static [&'static str],
}

const EXPECTED_PATH_GLOBS: &[&str] = &["src/**", "crates/**"];
const EXPECTED_EXCLUDE_GLOBS: &[&str] = &["target/**", ".git/**"];

/// Hand-transcribed from `packs/lsp-max-pack/ontology.ttl`'s 3
/// `lm:LintRule` individuals (`lm:UnwrapRule`, `lm:RawProtocolRule`,
/// `lm:WallClockRule`). Positive/negative snippets were authored for this
/// proof, not lifted from the ontology, to exercise real regex semantics.
const EXPECTED_RULES: &[Expected] = &[
    Expected {
        id: "LSPMAX-UNWRAP-001",
        name: "Unwrap in production path",
        severity: "error",
        pattern: r"\.unwrap\(\)|\.expect\(",
        eval_budget: "sync",
        message: "unwrap()/expect() outside #[cfg(test)] panics the language server; return Result instead.",
        rationale: "An lsp-max server must never panic mid-request; RulePackServer handlers return ClassifiedFindings, not aborts.",
        positive: &["value.unwrap();", "x.expect(\"y\")"],
        negative: &["let unwrap_flag = true;"],
    },
    Expected {
        id: "LSPMAX-RAWJSON-002",
        name: "Raw protocol fallback",
        severity: "warning",
        pattern: r"serde_json::Value|json!\(",
        eval_budget: "background",
        message: "Raw JSON fallback detected; use the generated metaModel.json protocol types.",
        rationale: "Protocol completeness must not be faked with untyped serde_json fallback.",
        positive: &["let v: serde_json::Value = default_value();", "json!({\"a\": 1})"],
        negative: &["let v: MetaModelValue = MetaModelValue::default();"],
    },
    Expected {
        id: "LSPMAX-WALLCLOCK-003",
        name: "Wall clock in server code",
        severity: "error",
        pattern: r"SystemTime::now|Instant::now",
        eval_budget: "sync",
        message: "Wall-clock reads make receipts nondeterministic; thread time in from the transport boundary.",
        rationale: "Deterministic replay of an lsp-max session requires clock-free handler logic.",
        positive: &["let t = SystemTime::now();", "let start = Instant::now();"],
        negative: &["let t = transport_time;"],
    },
];

/// Reads every `rules/lsp_max_*.toml` file present, parsing each with the
/// real `toml` crate. Panics loudly (not a silent empty Vec) if the
/// `rules/` directory itself is missing, since that means sync never ran.
fn read_generated_rule_files() -> Vec<(String, ParsedRule)> {
    let dir = Path::new("rules");
    let entries = fs::read_dir(dir).unwrap_or_else(|e| {
        panic!(
            "expected {} to exist (generated by lsp-max-pack's rule_pack.toml.tmpl): {e}",
            dir.display()
        )
    });

    let mut out = Vec::new();
    for entry in entries {
        let entry = entry.expect("readable rules/ dir entry");
        let path = entry.path();
        let file_name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or_default()
            .to_string();
        if file_name.starts_with("lsp_max_") && file_name.ends_with(".toml") {
            let content = fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
            let parsed: ParsedRulePackFile = toml::from_str(&content)
                .unwrap_or_else(|e| panic!("parsing {} as TOML: {e}", path.display()));
            assert_eq!(
                parsed.rules.len(),
                1,
                "{} must contain exactly one [[rules]] entry (one-file-per-rule fan-out)",
                path.display()
            );
            out.push((file_name, parsed.rules.into_iter().next().unwrap()));
        }
    }
    out
}

#[test]
fn exactly_three_rule_files_are_generated() {
    let files = read_generated_rule_files();
    assert_eq!(
        files.len(),
        3,
        "expected exactly 3 rules/lsp_max_*.toml files (lm:UnwrapRule, lm:RawProtocolRule, lm:WallClockRule), found: {:?}",
        files.iter().map(|(name, _)| name).collect::<Vec<_>>()
    );
}

#[test]
fn every_generated_rule_matches_one_hand_transcribed_expectation() {
    let files = read_generated_rule_files();

    for (file_name, rule) in &files {
        let expected = EXPECTED_RULES
            .iter()
            .find(|e| e.id == rule.id)
            .unwrap_or_else(|| {
                panic!(
                    "{file_name} has id {:?} which is not one of the 3 expected rule ids",
                    rule.id
                )
            });

        assert_eq!(rule.name, expected.name, "name mismatch for {}", expected.id);
        assert_eq!(rule.severity, expected.severity, "severity mismatch for {}", expected.id);
        assert_eq!(rule.pattern, expected.pattern, "pattern mismatch for {}", expected.id);
        assert_eq!(rule.message, expected.message, "message mismatch for {}", expected.id);
        assert_eq!(rule.rationale, expected.rationale, "rationale mismatch for {}", expected.id);
        assert_eq!(
            rule.eval_budget, expected.eval_budget,
            "eval_budget mismatch for {} (ontology-sourced lm:evalBudget, must be \"sync\" or \"background\" to match the real lsp-max EvalBudget enum's #[serde(rename_all = \"snake_case\")])",
            expected.id
        );
        assert_eq!(
            rule.path_globs, EXPECTED_PATH_GLOBS,
            "path_globs mismatch for {} (ontology-sourced lm:pathGlobs, not a template-hardcoded literal)",
            expected.id
        );
        assert_eq!(
            rule.exclude_globs, EXPECTED_EXCLUDE_GLOBS,
            "exclude_globs mismatch for {} (ontology-sourced lm:excludeGlobs, not a template-hardcoded literal)",
            expected.id
        );
    }

    // Every expected id must actually have been found among the generated
    // files -- catches a dropped row, not just a mismatched one.
    for expected in EXPECTED_RULES {
        assert!(
            files.iter().any(|(_, r)| r.id == expected.id),
            "expected rule {} was not found among generated rules/*.toml files",
            expected.id
        );
    }
}

#[test]
fn every_generated_pattern_compiles_as_a_real_regex() {
    let files = read_generated_rule_files();
    assert!(!files.is_empty(), "no generated rule files found to check");
    for (file_name, rule) in &files {
        Regex::new(&rule.pattern).unwrap_or_else(|e| {
            panic!("{file_name}'s pattern {:?} failed to compile as a regex: {e}", rule.pattern)
        });
    }
}

#[test]
fn unwrap_rule_regex_matches_real_unwrap_and_expect_call_sites() {
    let expected = EXPECTED_RULES
        .iter()
        .find(|e| e.id == "LSPMAX-UNWRAP-001")
        .expect("LSPMAX-UNWRAP-001 must be in EXPECTED_RULES");
    let re = Regex::new(expected.pattern).expect("LSPMAX-UNWRAP-001 pattern compiles");

    for snippet in expected.positive {
        assert!(
            re.is_match(snippet),
            "expected pattern {:?} to match unwrap/expect snippet {:?}",
            expected.pattern,
            snippet
        );
    }
    for snippet in expected.negative {
        assert!(
            !re.is_match(snippet),
            "expected pattern {:?} to NOT match non-offending snippet {:?}",
            expected.pattern,
            snippet
        );
    }
}

#[test]
fn raw_protocol_rule_regex_matches_real_serde_json_fallback_usage() {
    let expected = EXPECTED_RULES
        .iter()
        .find(|e| e.id == "LSPMAX-RAWJSON-002")
        .expect("LSPMAX-RAWJSON-002 must be in EXPECTED_RULES");
    let re = Regex::new(expected.pattern).expect("LSPMAX-RAWJSON-002 pattern compiles");

    for snippet in expected.positive {
        assert!(
            re.is_match(snippet),
            "expected pattern {:?} to match raw-protocol snippet {:?}",
            expected.pattern,
            snippet
        );
    }
    for snippet in expected.negative {
        assert!(
            !re.is_match(snippet),
            "expected pattern {:?} to NOT match non-offending snippet {:?}",
            expected.pattern,
            snippet
        );
    }
}

#[test]
fn wall_clock_rule_regex_matches_real_systemtime_and_instant_now_usage() {
    let expected = EXPECTED_RULES
        .iter()
        .find(|e| e.id == "LSPMAX-WALLCLOCK-003")
        .expect("LSPMAX-WALLCLOCK-003 must be in EXPECTED_RULES");
    let re = Regex::new(expected.pattern).expect("LSPMAX-WALLCLOCK-003 pattern compiles");

    for snippet in expected.positive {
        assert!(
            re.is_match(snippet),
            "expected pattern {:?} to match wall-clock snippet {:?}",
            expected.pattern,
            snippet
        );
    }
    for snippet in expected.negative {
        assert!(
            !re.is_match(snippet),
            "expected pattern {:?} to NOT match non-offending snippet {:?}",
            expected.pattern,
            snippet
        );
    }
}

#[test]
fn generated_catalog_markdown_lists_all_three_rules_exactly_once() {
    let path = Path::new("docs/lsp_max_rule_pack.md");
    let content = fs::read_to_string(path).unwrap_or_else(|e| {
        panic!(
            "expected {} to exist (generated by lsp-max-pack's rule_pack_index.md.tmpl): {e}",
            path.display()
        )
    });

    for expected in EXPECTED_RULES {
        assert!(
            content.contains(expected.id),
            "catalog markdown missing rule id {}",
            expected.id
        );
        assert!(
            content.contains(expected.name),
            "catalog markdown missing rule name {:?}",
            expected.name
        );
    }

    let table_row_count = content
        .lines()
        .filter(|line| line.trim_start().starts_with('|') && line.contains("LSPMAX-"))
        .count();
    assert_eq!(
        table_row_count, 3,
        "expected exactly 3 rule rows in the generated markdown catalog table, found {table_row_count}"
    );
}
