//! Chicago tests for r80-01: ontology-driven semantic tokens (full + delta)
//! for ggen-generated Rust surfaces.
//!
//! Real collaborators throughout: the real `.specify/repo-facts.ttl`, the
//! real root `ggen.toml`, the real generated fixture
//! `crates/ggen-cli/src/generated_commands.rs`, a real tree-sitter-rust
//! parse, and a real oxigraph graph parse. No mocks.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]
#[path = "../src/tokens.rs"]
mod tokens;

use tokens::{
    apply_edits, classify_rust, decode, encode, legend_from_facts, parse_ontology_facts, AbsToken,
    TokensEngine,
};

const REPO_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../..");
const REPO_FACTS: &str = "/.specify/repo-facts.ttl";
const GGEN_TOML: &str = "/ggen.toml";
const FIXTURE: &str = "/crates/ggen-cli/src/generated_commands.rs";

fn read_repo_file(rel: &str) -> String {
    std::fs::read_to_string(format!("{REPO_ROOT}{rel}"))
        .unwrap_or_else(|e| panic!("read {rel}: {e}"))
}

fn engine() -> TokensEngine {
    let ttl = read_repo_file(REPO_FACTS);
    let toml = read_repo_file(GGEN_TOML);
    TokensEngine::from_ontology(&ttl, Some(&toml))
}

/// Legend derived from the REAL ontology: rf:Crate and rf:DiagnosticCode
/// individuals exist in repo-facts.ttl, so `crate` and `diagnostic-code`
/// legend entries must be present, after the grammar kinds.
#[test]
fn legend_is_derived_from_real_ontology() {
    let ttl = read_repo_file(REPO_FACTS);
    let facts = parse_ontology_facts(&ttl, None);
    assert!(
        !facts.crate_packages.is_empty(),
        "repo-facts.ttl must declare rf:Crate individuals"
    );
    assert!(
        !facts.diagnostic_codes.is_empty(),
        "repo-facts.ttl must declare rf:DiagnosticCode individuals"
    );
    assert!(facts.crate_packages.contains("ggen-engine"));
    assert!(facts.diagnostic_codes.contains("E0010"));
    assert!(facts.diagnostic_codes.contains("GGEN-TPL-001"));

    let legend = engine().legend().clone();
    assert_eq!(
        legend.token_types,
        vec![
            "crate".to_owned(),
            "module".to_owned(),
            "struct".to_owned(),
            "fn".to_owned(),
            "const".to_owned(),
            "diagnostic-code".to_owned(),
        ]
    );
    assert_eq!(
        legend.modifiers,
        vec!["declaration", "definition", "generated"]
    );
}

/// Ontology absence flips legend entries: no rf:Crate/rf:DiagnosticCode
/// facts -> those token types vanish (legend is driven by the ontology, not
/// hardcoded).
#[test]
fn legend_entries_vanish_when_ontology_lacks_referents() {
    let legend = legend_from_facts(false, false);
    assert!(!legend.token_types.contains(&"crate".to_owned()));
    assert!(!legend.token_types.contains(&"diagnostic-code".to_owned()));
    let with = legend_from_facts(true, true);
    assert_eq!(with.index_of("crate"), Some(0));
    assert_eq!(with.index_of("diagnostic-code"), Some(5));
}

/// Full tokenization of the real generated fixture: the const/fn names in
/// `generated_commands.rs` (itself produced by ggen.toml rule
/// `cli-commands-reference`) come back with the right bit-packed legend
/// indices, valid relative encoding, and the `generated` modifier set.
#[test]
fn full_tokens_on_real_generated_fixture() {
    let text = read_repo_file(FIXTURE);
    let mut eng = engine();
    let legend = eng.legend().clone();
    let uri = "file:///Users/sac/ggen/crates/ggen-cli/src/generated_commands.rs";
    let tokens = eng.full(uri, &text);

    assert!(tokens.result_id.is_some());
    assert_eq!(tokens.data.len() % 5, 0, "bit-packed 5-tuples");
    assert!(!tokens.data.is_empty(), "fixture must yield tokens");

    let abs = decode(&tokens.data);
    // Relative encoding validity: delta_line monotone, delta_start >= 0 on
    // same line; token_type always in legend; modifier bits within legend.
    let mut prev_line = 0u32;
    for (i, t) in abs.iter().enumerate() {
        assert!(t.line >= prev_line, "token {i}: line went backwards");
        prev_line = t.line;
        assert!(
            (t.token_type as usize) < legend.token_types.len(),
            "token {i}: token_type {} out of legend",
            t.token_type
        );
        let max_mods = 1u32 << legend.modifiers.len();
        assert!(
            t.modifiers < max_mods,
            "token {i}: modifier bitset overflow"
        );
    }

    // Named symbols from the generated surface, by text.
    let text_at = |t: &AbsToken| -> String {
        // decode() leaves byte ranges 0; re-derive text via line/start/length
        // over the source using UTF-16 coordinates.
        let lines: Vec<&str> = text.lines().collect();
        let line = lines[t.line as usize];
        let utf16: Vec<u16> = line.encode_utf16().collect();
        let s = t.start as usize;
        let e = s + t.length as usize;
        String::from_utf16(&utf16[s..e]).expect("utf16 slice")
    };
    let names: Vec<String> = abs.iter().map(&text_at).collect();
    assert!(
        names.contains(&"COMMANDS_REFERENCE".to_owned()),
        "const name token: {names:?}"
    );
    assert!(names.contains(&"DEFAULT_VERBS".to_owned()));
    assert!(
        names.contains(&"describe_command".to_owned()),
        "fn name token"
    );

    // Legend classification of those names.
    let tt_const = legend.index_of("const").unwrap();
    let tt_fn = legend.index_of("fn").unwrap();
    let tt_struct = legend.index_of("struct").unwrap();
    for t in &abs {
        match text_at(t).as_str() {
            "COMMANDS_REFERENCE" | "DEFAULT_VERBS" => {
                assert_eq!(t.token_type, tt_const, "const token type");
            }
            "describe_command" => assert_eq!(t.token_type, tt_fn, "fn token type"),
            _ => {}
        }
    }
    let _ = tt_struct;

    // `generated` modifier set (uri is a ggen.toml rule output_file).
    let gen_bit = 1u32 << legend.modifier_bit("generated").unwrap();
    assert!(
        abs.iter().all(|t| t.modifiers & gen_bit != 0),
        "all tokens in a generated surface carry the generated bit"
    );

    // Encode(decode(data)) == data (relative encoding is a bijection on
    // sorted absolute tokens).
    let re_encoded = encode(&abs);
    assert_eq!(re_encoded, tokens.data, "encode/decode round-trip");
}

/// tree-sitter-rust classification on a hand-written Rust sample covering
/// all grammar kinds plus crate references and diagnostic-code strings.
#[test]
fn classify_rust_grammar_kinds_and_ontology_kinds() {
    let ttl = read_repo_file(REPO_FACTS);
    let facts = parse_ontology_facts(&ttl, None);
    let legend = legend_from_facts(true, true);

    let source = r#"
mod inner {
    pub struct Widget { pub name: String }
    pub fn build() -> Widget { Widget { name: "E0010 in doc".to_owned() } }
    const MAX: usize = 3;
}
use ggen_engine::sync;
"#;
    let abs = classify_rust(source, &legend, &facts, false);
    assert!(!abs.is_empty());

    let lines: Vec<&str> = source.lines().collect();
    let text_at = |t: &AbsToken| -> String {
        let line = lines[t.line as usize];
        let utf16: Vec<u16> = line.encode_utf16().collect();
        let s = t.start as usize;
        String::from_utf16(&utf16[s..s + t.length as usize]).expect("utf16 slice")
    };
    let tt = |n: &str| legend.index_of(n).unwrap();
    let mut found = std::collections::BTreeMap::new();
    for t in &abs {
        found.entry(text_at(t).clone()).or_insert(t.token_type);
    }
    assert_eq!(found.get("inner"), Some(&tt("module")), "mod name");
    assert_eq!(found.get("Widget"), Some(&tt("struct")), "struct name");
    assert_eq!(found.get("build"), Some(&tt("fn")), "fn name");
    assert_eq!(found.get("MAX"), Some(&tt("const")), "const name");
    assert_eq!(
        found.get("ggen_engine"),
        Some(&tt("crate")),
        "rf:Crate package reference"
    );
    // Diagnostic code inside a string literal.
    let diag = abs
        .iter()
        .find(|t| t.token_type == tt("diagnostic-code"))
        .expect("E0010 inside a string must classify as diagnostic-code");
    assert!(text_at(diag).contains("E0010"));
}

/// Delta round-trip on the real fixture: full -> edit the text (insert a new
/// fn) -> delta -> apply edits client-side -> decode == decode of a fresh
/// full computation. Also: unchanged text yields zero edits; a stale result
/// id yields a full-replacement edit that still round-trips.
#[test]
fn delta_round_trip_on_real_fixture() {
    let text = read_repo_file(FIXTURE);
    let uri = "file:///Users/sac/ggen/crates/ggen-cli/src/generated_commands.rs";
    let mut eng = engine();

    let first = eng.full(uri, &text);
    let first_id = first.result_id.clone().expect("resultId");

    // Unchanged text: empty edits.
    let same = eng.delta(uri, &first_id, &text);
    assert!(
        same.edits.is_empty(),
        "identical text must produce zero edits, got {:?}",
        same.edits
    );

    // Edit: insert a new fn near the top.
    let edited = format!("fn newly_added() {{}}\n{text}");
    let delta = eng.delta(uri, &first_id, &edited);
    assert_eq!(delta.edits.len(), 1, "prefix/suffix diff = one edit");
    assert!(delta.result_id.is_some());
    assert_ne!(delta.result_id, Some(first_id.clone()));

    // Client-side apply, then compare against a fresh full computation.
    let applied = apply_edits(&first.data, &delta.edits).expect("edits must apply");
    let fresh = eng.full(uri, &edited);
    assert_eq!(
        decode(&applied),
        decode(&fresh.data),
        "applied delta must decode to the fresh full tokens"
    );

    // Stale result id: full-replacement edit against the server's latest
    // stored generation, still round-trips.
    let latest = eng.full(uri, &edited);
    let stale = eng.delta(uri, "tok-stale-from-the-past", &edited);
    assert_eq!(stale.edits.len(), 1);
    let applied_stale = apply_edits(&latest.data, &stale.edits).expect("stale edits apply");
    let fresh2 = eng.full(uri, &edited);
    assert_eq!(decode(&applied_stale), decode(&fresh2.data));
}

/// Edits that reach past the array end must fail loudly, not silently
/// truncate (a decode of garbage is worse than an error).
#[test]
fn apply_edits_rejects_out_of_bounds_edit() {
    let bad = lsp_max_protocol::lsp_3_18::SemanticTokensEdit {
        start: 999,
        delete_count: 10,
        data: Some(vec![0]),
    };
    assert!(apply_edits(&[1, 2, 3, 4, 5], &[bad]).is_none());
}
