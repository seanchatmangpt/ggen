//! Adversarial edge-case testing for math builtins (division/remainder by zero,
//! logarithm of zero/negative, integer overflow boundaries, non-numeric literal
//! rejection).

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
