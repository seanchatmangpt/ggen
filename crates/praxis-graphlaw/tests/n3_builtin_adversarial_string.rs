//! Adversarial edge-case testing for string builtins (empty string,
//! unicode/multi-byte, matches/notMatches with pathological/empty regex,
//! *IgnoringCase case-sensitivity boundaries).

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples
        .iter()
        .map(praxis_graphlaw::TripleStore::decode_triple)
        .collect()
}

fn materialize(data: &str) -> Vec<String> {
    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    decode_all(&inferred)
}

// ===========================================================================
// string: empty string, unicode/multi-byte, matches/notMatches, case
// boundaries.
// ===========================================================================

#[test]
fn string_length_of_empty_string_is_zero() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :s :v \"\" .\n\
                { ?s :v ?v . ?v string:length ?l } => { ?s :result ?l }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("\"0\"")),
        "empty string must have length 0, got: {:?}",
        decoded
    );
}

// string:length must count Unicode scalar values (chars), not UTF-8 bytes --
// the independent oracle is Rust's own `.chars().count()`, same primitive
// the implementation uses, applied to strings chosen to differ sharply
// between byte-length and char-length (accented letters, emoji).
proptest! {
    #[test]
    fn prop_string_length_counts_unicode_chars_not_bytes(
        s in prop::sample::select(vec![
            "café", "naïve", "日本語", "👍👍👍", "Zürich", "a\u{0301}bc", "🇺🇸flag", "",
        ]),
    ) {
        let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
             \n\
             :s :v \"{escaped}\" .\n\
             {{ ?s :v ?v . ?v string:length ?l }} => {{ ?s :result ?l }}.\n"
        );
        let decoded = materialize(&data);
        let expected = s.chars().count();
        let expected_lex = format!("\"{expected}\"");
        prop_assert!(
            decoded.iter().any(|d| d.contains("/result") && d.contains(&expected_lex)),
            "string {:?}: expected char-count length {}, got: {:?}", s, expected, decoded
        );
    }
}

// string:concat with unicode operands: the oracle is straightforward Rust
// string concatenation.
#[test]
fn string_concat_preserves_unicode() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :s :a \"caf\u{e9}\" .\n\
                :s :b \"\u{1f600}\" .\n\
                { ?s :a ?a . ?s :b ?b . ( ?a ?b ) string:concat ?c } => { ?s :result ?c }.\n";
    let decoded = materialize(data);
    let expected = "caf\u{e9}\u{1f600}".to_string();
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains(&expected)),
        "expected concatenation {:?}, got: {:?}",
        expected,
        decoded
    );
}

// string:matches / notMatches: pathological/empty regex patterns must not
// panic (Regex::new(...).ok() gracefully fails closed for invalid
// patterns) and an empty pattern "" must match every string (regex "").
proptest! {
    #[test]
    fn prop_string_matches_empty_pattern_always_matches(
        s in "[a-zA-Z0-9 ]{0,20}",
    ) {
        let escaped = s.replace('\\', "\\\\").replace('"', "\\\"");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
             \n\
             :s :v \"{escaped}\" .\n\
             {{ ?s :v ?v . ?v string:matches \"\" }} => {{ ?s a :Matched }}.\n"
        );
        let decoded = materialize(&data);
        // An empty regex pattern matches any string (including the empty
        // string itself), per standard regex semantics.
        prop_assert!(
            decoded.iter().any(|d| d.contains("/Matched")),
            "empty regex pattern must match {:?}, got: {:?}", s, decoded
        );
    }
}

#[test]
fn string_matches_invalid_regex_fails_closed_no_panic() {
    // "(" is an unterminated group -- an invalid regex. Regex::new(...) is
    // Err, and eval_string_matches's `.map(...).unwrap_or(false)` makes
    // the constraint simply not hold -- no panic, no derivation.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :s :v \"anything\" .\n\
                { ?s :v ?v . ?v string:matches \"(\" } => { ?s a :Matched }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Matched")),
        "an invalid regex pattern must fail closed (no derivation), got: {:?}",
        decoded
    );
}

#[test]
fn string_not_matches_invalid_regex_fails_closed_no_panic() {
    // eval_string_not_matches: `!Regex::new(...).map(...).unwrap_or(false)`
    // -- for an Err regex, `.unwrap_or(false)` is false, so `!false` is
    // true: notMatches treats an invalid pattern as vacuously "not
    // matching". Document and pin that exact behavior.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :s :v \"anything\" .\n\
                { ?s :v ?v . ?v string:notMatches \"(\" } => { ?s a :NotMatched }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/NotMatched")),
        "an invalid regex pattern must make notMatches vacuously true (documented current behavior), got: {:?}",
        decoded
    );
}

// *IgnoringCase case-sensitivity boundary: strings differing only by case
// must be equalIgnoringCase but NOT (plain) string:lessThan/equal, and the
// non-ignoring-case variants must distinguish them.
proptest! {
    #[test]
    fn prop_string_equal_ignoring_case_boundary(
        base in "[a-zA-Z]{1,10}",
    ) {
        let upper = base.to_uppercase();
        let lower = base.to_lowercase();
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
             \n\
             :s :a \"{upper}\" .\n\
             :s :b \"{lower}\" .\n\
             {{ ?s :a ?a . ?s :b ?b . ?a string:equalIgnoringCase ?b }} => {{ ?s a :IgnoreCaseEqual }}.\n\
             {{ ?s :a ?a . ?s :b ?b . ?a string:notEqualIgnoringCase ?b }} => {{ ?s a :IgnoreCaseNotEqual }}.\n"
        );
        let decoded = materialize(&data);
        let should_be_equal_ignoring_case = upper.to_lowercase() == lower.to_lowercase();
        prop_assert_eq!(
            decoded.iter().any(|d| d.contains("/IgnoreCaseEqual")),
            should_be_equal_ignoring_case,
            "upper={:?} lower={:?}: equalIgnoringCase mismatch, got: {:?}", upper, lower, decoded
        );
        prop_assert_eq!(
            decoded.iter().any(|d| d.contains("/IgnoreCaseNotEqual")),
            !should_be_equal_ignoring_case,
            "upper={:?} lower={:?}: notEqualIgnoringCase mismatch, got: {:?}", upper, lower, decoded
        );
    }
}

#[test]
fn string_contains_ignoring_case_boundary() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :s :a \"Hello World\" .\n\
                :s :b \"WORLD\" .\n\
                { ?s :a ?a . ?s :b ?b . ?a string:containsIgnoringCase ?b } => { ?s a :Found }.\n\
                { ?s :a ?a . ?s :b ?b . ?a string:contains ?b } => { ?s a :FoundCaseSensitive }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/Found")) && !decoded.iter().any(|d| d.contains("/FoundCaseSensitive")),
        "\"Hello World\" containsIgnoringCase \"WORLD\" must hold but plain contains must not, got: {:?}",
        decoded
    );
}
