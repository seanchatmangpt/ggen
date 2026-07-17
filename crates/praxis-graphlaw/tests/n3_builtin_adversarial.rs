//! Adversarial edge-case testing for builtin CORRECTNESS (not dispatch
//! coverage -- `n3_builtin_coverage.rs` already asserts every builtin IRI is
//! `classify`d; this file asserts the actual *computed values* are right
//! under boundary/edge inputs). Each case independently (re-)computes the
//! expected outcome in the test itself and compares it against the real
//! `materialize()` output -- never "doesn't panic" as the bar.
//!
//! Covers, per builtin family (see lib/src/builtins/{math,string,list,log}.rs
//! for the real current implementations this suite is pinned against):
//! - math: division/remainder by zero, logarithm of zero/negative, integer
//!   overflow boundaries, non-numeric literal rejection.
//! - string: empty string, unicode/multi-byte, matches/notMatches with a
//!   pathological/empty regex, *IgnoringCase case-sensitivity boundaries.
//! - list: empty list, single-element list, deeply nested lists,
//!   out-of-range memberAt.
//! - log: type-mismatched equalTo/notEqualTo (literal vs IRI vs bnode),
//!   bound on a genuinely unbound variable.

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
// math: division / remainder by zero -- must NOT derive (no crash, no
// bogus fact), per `eval_quotient`/`eval_remainder` explicitly returning
// `None` when the divisor is 0.0.
// ===========================================================================

proptest! {
    #[test]
    fn prop_math_quotient_by_zero_never_derives(numerator in -1000i64..1000) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             \n\
             :s :n {numerator} .\n\
             :s :z 0 .\n\
             {{ ?s :n ?n . ?s :z ?z . ( ?n ?z ) math:quotient ?q }} => {{ ?s :result ?q }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "division by zero must not derive any :result fact, got: {:?}", decoded
        );
    }

    #[test]
    fn prop_math_remainder_by_zero_never_derives(numerator in -1000i64..1000) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             \n\
             :s :n {numerator} .\n\
             :s :z 0 .\n\
             {{ ?s :n ?n . ?s :z ?z . ( ?n ?z ) math:remainder ?r }} => {{ ?s :result ?r }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "remainder by zero must not derive any :result fact, got: {:?}", decoded
        );
    }

    // Non-zero divisor: exact-value oracle, computed independently in f64.
    #[test]
    fn prop_math_quotient_exact_value(numerator in -1000i64..1000, denominator in prop::sample::select(vec![-7i64,-3,-1,1,2,3,5,11,100])) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             \n\
             :s :n {numerator} .\n\
             :s :d {denominator} .\n\
             {{ ?s :n ?n . ?s :d ?d . ( ?n ?d ) math:quotient ?q }} => {{ ?s :result ?q }}.\n"
        );
        let decoded = materialize(&data);
        let expected = numerator as f64 / denominator as f64;
        // intern_number formats whole values as integers, else as f64's Display.
        let expected_lex = if expected.fract() == 0.0 && expected.abs() < 1e15 {
            format!("{}", expected as i64)
        } else {
            format!("{}", expected)
        };
        prop_assert!(
            decoded.iter().any(|d| d.contains("/result") && d.contains(&expected_lex)),
            "expected :result containing {:?}, got: {:?}", expected_lex, decoded
        );
    }
}

// math: logarithm of zero and negative numbers must not crash. Rust's
// f64::log(base) for value<=0 yields NaN/-inf, which the engine currently
// still derives as a literal (documented current behavior, not a panic) --
// assert that behavior explicitly and unambiguously here, rather than
// leave it as an untested "hopefully doesn't blow up" corner.
#[test]
fn math_logarithm_of_zero_yields_neg_infinity_literal_not_a_crash() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :s :v 0 .\n\
                :s :b 10 .\n\
                { ?s :v ?v . ?s :b ?b . ( ?v ?b ) math:logarithm ?l } => { ?s :result ?l }.\n";
    let decoded = materialize(data);
    // log(0) base 10 == -inf in IEEE754 f64; Rust's Display for f64 prints
    // "-inf". The engine must derive *some* deterministic literal (no
    // panic, no silent non-derivation), matching what f64::log produces.
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("-inf")),
        "log(0) must deterministically derive -inf (matching Rust's f64::log semantics), got: {:?}",
        decoded
    );
}

#[test]
fn math_logarithm_of_negative_yields_nan_literal_not_a_crash() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :s :v -5 .\n\
                :s :b 10 .\n\
                { ?s :v ?v . ?s :b ?b . ( ?v ?b ) math:logarithm ?l } => { ?s :result ?l }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("NaN")),
        "log(-5) must deterministically derive NaN (matching Rust's f64::log semantics), got: {:?}",
        decoded
    );
}

// math: integer overflow boundaries -- i64::MAX-scale sums must still
// compute the mathematically exact f64 result (or at least not panic /
// silently truncate to something wrong); oracle is plain f64 addition,
// same as the engine's own `numeric_value`/`intern_number` machinery.
proptest! {
    #[test]
    fn prop_math_sum_near_i64_boundary_exact_value(
        a in (i64::MAX - 1000)..i64::MAX,
        b in 0i64..1000,
    ) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
             \n\
             :s :a {a} .\n\
             :s :b {b} .\n\
             {{ ?s :a ?x . ?s :b ?y . ( ?x ?y ) math:sum ?t }} => {{ ?s :result ?t }}.\n"
        );
        let decoded = materialize(&data);
        let expected = a as f64 + b as f64;
        let expected_lex = if expected.fract() == 0.0 && expected.abs() < 1e15 {
            format!("{}", expected as i64)
        } else {
            format!("{}", expected)
        };
        prop_assert!(
            decoded.iter().any(|d| d.contains("/result") && d.contains(&expected_lex)),
            "sum near i64::MAX: expected {:?}, got: {:?}", expected_lex, decoded
        );
    }
}

// math: non-numeric literal input must simply not fire the rule (no
// derivation), never panic.
#[test]
fn math_greater_than_rejects_non_numeric_literal() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :s :v \"not-a-number\" .\n\
                { ?s :v ?v . ?v math:greaterThan 0 } => { ?s a :Positive }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Positive")),
        "non-numeric literal must not satisfy math:greaterThan, got: {:?}",
        decoded
    );
}

#[test]
fn math_sum_rejects_non_numeric_list_member() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :s :list ( 1 \"abc\" 3 ) .\n\
                { ?s :list ?l . ?l math:sum ?t } => { ?s :result ?t }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/result")),
        "a list containing a non-numeric member must not derive a math:sum result, got: {:?}",
        decoded
    );
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

// ===========================================================================
// list: empty list, single-element, deeply nested, out-of-range memberAt.
// ===========================================================================

#[test]
fn list_length_of_empty_list_is_zero() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( ) .\n\
                { ?s :l ?l . ?l list:length ?n } => { ?s :result ?n }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("\"0\"")),
        "empty list must have length 0, got: {:?}",
        decoded
    );
}

#[test]
fn list_first_and_last_of_empty_list_do_not_derive() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( ) .\n\
                { ?s :l ?l . ?l list:first ?x } => { ?s :firstResult ?x }.\n\
                { ?s :l ?l . ?l list:last ?x } => { ?s :lastResult ?x }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/firstResult") || d.contains("/lastResult")),
        "list:first/list:last on an empty list must not derive anything, got: {:?}",
        decoded
    );
}

#[test]
fn list_first_and_last_of_single_element_list_are_the_same_element() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( 42 ) .\n\
                { ?s :l ?l . ?l list:first ?x } => { ?s :firstResult ?x }.\n\
                { ?s :l ?l . ?l list:last ?x } => { ?s :lastResult ?x }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/firstResult") && d.contains("42")),
        "single-element list: list:first must be 42, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/lastResult") && d.contains("42")),
        "single-element list: list:last must be 42, got: {:?}",
        decoded
    );
}

#[test]
fn list_rest_of_single_element_list_is_empty() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :s :l ( 42 ) .\n\
                { ?s :l ?l . ?l list:rest ?r . ?r list:length ?n } => { ?s :result ?n }.\n";
    let decoded = materialize(data);
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/result") && d.contains("\"0\"")),
        "list:rest of a single-element list must be an empty list (length 0), got: {:?}",
        decoded
    );
}

// Deeply nested lists (3+ levels): oracle is the true structural member
// count at each level, cross-checked with list:length and list:first
// unwrapping one level at a time.
proptest! {
    #[test]
    fn prop_list_nested_three_levels_length_and_first(
        a in 0i64..50, b in 0i64..50, c in 0i64..50,
    ) {
        // :l = ( ( ( a b ) c ) ) -- 3 levels of nesting, 1 outer member.
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( ( ( {a} {b} ) {c} ) ) .\n\
             {{ ?s :l ?l . ?l list:length ?n }} => {{ ?s :outerLen ?n }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:length ?n2 }} => {{ ?s :midLen ?n2 }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:first ?f2 . ?f2 list:length ?n3 }} => {{ ?s :innerLen ?n3 }}.\n\
             {{ ?s :l ?l . ?l list:first ?f . ?f list:first ?f2 . ?f2 list:first ?ff }} => {{ ?s :innermost ?ff }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(decoded.iter().any(|d| d.contains("/outerLen") && d.contains("\"1\"")),
            "outer list has 1 member, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/midLen") && d.contains("\"2\"")),
            "middle list ((a b) c) has 2 members, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/innerLen") && d.contains("\"2\"")),
            "innermost list (a b) has 2 members, got: {:?}", decoded);
        prop_assert!(decoded.iter().any(|d| d.contains("/innermost") && d.contains(&format!("\"{a}\""))),
            "deepest list:first must unwrap all the way to {}, got: {:?}", a, decoded);
    }
}

// list:memberAt out-of-range index must not derive (no crash, no wraparound,
// no silent zero).
proptest! {
    #[test]
    fn prop_list_member_at_out_of_range_never_derives(
        len in 0usize..6,
        idx in 6i64..20,
    ) {
        let members: Vec<i64> = (0..len as i64).collect();
        let list_str = members.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( {list_str} ) .\n\
             :s :i {idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "memberAt index {} is out of range for a list of length {}, must not derive, got: {:?}",
            idx, len, decoded
        );
    }

    #[test]
    fn prop_list_member_at_negative_index_never_derives(
        neg_idx in -20i64..0,
    ) {
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( 1 2 3 ) .\n\
             :s :i {neg_idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        prop_assert!(
            !decoded.iter().any(|d| d.contains("/result")),
            "negative memberAt index {} must not derive, got: {:?}", neg_idx, decoded
        );
    }

    #[test]
    fn prop_list_member_at_in_range_exact_value(
        members in prop::collection::vec(0i64..100, 1..8),
    ) {
        let idx = members.len() / 2; // always in range
        let list_str = members.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" ");
        let data = format!(
            "@prefix : <http://example.org/> .\n\
             @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
             \n\
             :s :l ( {list_str} ) .\n\
             :s :i {idx} .\n\
             {{ ?s :l ?l . ?s :i ?i . ( ?l ?i ) list:memberAt ?x }} => {{ ?s :result ?x }}.\n"
        );
        let decoded = materialize(&data);
        let expected = members[idx];
        prop_assert!(
            decoded.iter().any(|d| d.contains("/result") && d.contains(&format!("\"{expected}\""))),
            "memberAt {} of {:?} must be {}, got: {:?}", idx, members, expected, decoded
        );
    }
}

// ===========================================================================
// log: type-mismatched equalTo/notEqualTo (literal vs IRI vs blank node),
// and bound on a genuinely unbound variable.
// ===========================================================================

#[test]
fn log_equal_to_literal_vs_iri_are_not_equal() {
    // The literal "42" must NOT log:equalTo the numerically-unrelated IRI
    // :fortyTwo -- they are different RDF term kinds entirely; equalTo must
    // not coerce an IRI into a number or otherwise conflate the two.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a \"42\" .\n\
                :s :b :fortyTwo .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a string literal and an IRI must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_iri_vs_blank_node_are_not_equal() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a :namedThing .\n\
                :s :b [ ] .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a named IRI and a fresh blank node must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_literal_vs_blank_node_are_not_equal() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a \"something\" .\n\
                :s :b [ ] .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/Equal"))
            && decoded.iter().any(|d| d.contains("/NotEqual")),
        "a literal and a fresh blank node must never be log:equalTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_same_iri_is_equal() {
    // Sanity/control case: the SAME IRI referenced twice via two different
    // predicates must be log:equalTo (and never notEqualTo) -- confirms
    // the mismatched-type tests above are actually exercising a type
    // difference, not a general "equalTo never fires" bug.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a :sameThing .\n\
                :s :b :sameThing .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n\
                { ?s :a ?a . ?s :b ?b . ?a log:notEqualTo ?b } => { ?s a :NotEqual }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/Equal"))
            && !decoded.iter().any(|d| d.contains("/NotEqual")),
        "the same IRI referenced twice must be log:equalTo and never notEqualTo, got: {:?}",
        decoded
    );
}

#[test]
fn log_equal_to_numeric_literals_different_lexical_same_value_are_equal() {
    // 042 and 42 differ lexically but are the same number -- log:equalTo's
    // numeric fallback (`numeric_value(s) == numeric_value(o)`) must treat
    // them as equal, unlike a plain string/term comparison.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s :a 42.0 .\n\
                :s :b 42 .\n\
                { ?s :a ?a . ?s :b ?b . ?a log:equalTo ?b } => { ?s a :Equal }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/Equal")),
        "42.0 and 42 are the same numeric value and must be log:equalTo, got: {:?}",
        decoded
    );
}

// log:bound on a genuinely unbound variable: a variable that never appears
// in any ground fact must fail to satisfy log:bound, so the rule using it
// must never fire.
#[test]
fn log_bound_on_genuinely_unbound_variable_never_fires() {
    // ?neverBound is not connected to any ground fact via the rule body
    // pattern (no triple in the data ties it to anything), so it should
    // remain unbound and log:bound must reject it.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s a :Thing .\n\
                { ?s a :Thing . ?neverBound log:bound ?neverBound } => { ?s a :ShouldNotFire }.\n";
    let decoded = materialize(data);
    assert!(
        !decoded.iter().any(|d| d.contains("/ShouldNotFire")),
        "log:bound on a variable with no binding anywhere in the rule must never fire, got: {:?}",
        decoded
    );
}

#[test]
fn log_bound_on_a_bound_variable_fires() {
    // Control case: the same shape, but ?s IS bound by the preceding
    // pattern -- log:bound must let the rule fire.
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :s a :Thing .\n\
                { ?s a :Thing . ?s log:bound ?s } => { ?s a :ShouldFire }.\n";
    let decoded = materialize(data);
    assert!(
        decoded.iter().any(|d| d.contains("/ShouldFire")),
        "log:bound on an already-bound variable must let the rule fire, got: {:?}",
        decoded
    );
}
