//! Machine-checked "no holes" guarantee for N3 builtin coverage.
//!
//! Enumerates the full target list of builtin IRIs this plan commits to
//! supporting (see `docs/jira` / the finish-implementing-shacl-shex plan)
//! and asserts every single one is recognized by
//! `praxis_graphlaw::builtins::classify` -- i.e. resolves to a known
//! (non-`Unknown`... really non-`None`) `BuiltinKind`, whether it's handled
//! procedurally in `builtins::evaluate` or at the reasoner/fixpoint level
//! (`log:implies`, `log:collectAllIn`, etc., which `classify` still
//! registers as `BuiltinKind::ReasonerLevel`).
//!
//! If any IRI is missing, this test FAILS with a clear message listing
//! exactly which ones are unrecognized. The target list must never be
//! shrunk to make this pass -- gaps must be closed by implementing the
//! builtin for real.

use praxis_graphlaw::builtins::classify;
use praxis_graphlaw::term::VarOrTerm;

fn iri(x: &str) -> VarOrTerm {
    VarOrTerm::convert(x.to_string())
}

/// Full target list: (namespace-qualified name, full IRI).
fn target_builtins() -> Vec<(&'static str, &'static str)> {
    let math = "http://www.w3.org/2000/10/swap/math#";
    let string = "http://www.w3.org/2000/10/swap/string#";
    let list = "http://www.w3.org/2000/10/swap/list#";
    let log = "http://www.w3.org/2000/10/swap/log#";
    let time = "http://www.w3.org/2000/10/swap/time#";
    let crypto = "http://www.w3.org/2000/10/swap/crypto#";
    let func = "http://www.w3.org/2007/rif-builtin-function#";

    let mut v: Vec<(&'static str, String)> = Vec::new();

    // math:
    for n in [
        "greaterThan",
        "lessThan",
        "notLessThan",
        "notGreaterThan",
        "equalTo",
        "sum",
        "difference",
        "product",
        "quotient",
        "remainder",
        "absoluteValue",
        "negation",
        "rounded",
        "ceiling",
        "floor",
        "exponentiation",
        "integerQuotient",
        "min",
        "max",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "atan2",
        "logarithm",
        "memberCount",
    ] {
        v.push(("math", format!("<{}{}>", math, n)));
    }

    // string:
    for n in [
        "length",
        "concat",
        "lessThan",
        "contains",
        "containsIgnoringCase",
        "startsWith",
        "endsWith",
        "matches",
        "notMatches",
        "replace",
        "substring",
        "toUpperCase",
        "toLowerCase",
        "split",
        "equalIgnoringCase",
        "notEqualIgnoringCase",
        "greaterThan",
        "format",
        "scrape",
    ] {
        v.push(("string", format!("<{}{}>", string, n)));
    }

    // list:
    for n in [
        "length", "in", "append", "first", "rest", "last", "member", "memberAt", "remove", "sort",
        "unique", "reverse", "iterate",
    ] {
        v.push(("list", format!("<{}{}>", list, n)));
    }

    // log:
    for n in [
        "equalTo",
        "notEqualTo",
        "implies",
        "collectAllIn",
        "notIncludes",
        "includes",
        "conjunction",
        "dtlit",
        "rawType",
        "uri",
        "localName",
        "bound",
        "forAllIn",
        "ifThenElseIn",
        "n3String",
        "parsedAsN3",
    ] {
        v.push(("log", format!("<{}{}>", log, n)));
    }

    // time:
    for n in [
        "localTime",
        "year",
        "month",
        "day",
        "hour",
        "minute",
        "second",
        "dayOfWeek",
        "timeZone",
        "inSeconds",
    ] {
        v.push(("time", format!("<{}{}>", time, n)));
    }

    // crypto:
    for n in ["sha", "sha256", "sha512", "md5"] {
        v.push(("crypto", format!("<{}{}>", crypto, n)));
    }

    // func: (RIF)
    v.push(("func", format!("<{}{}>", func, "lang-from-PlainLiteral")));

    v.into_iter()
        .map(|(ns, iri)| {
            (
                Box::leak(format!("{ns}:{iri}").into_boxed_str()) as &'static str,
                Box::leak(iri.into_boxed_str()) as &'static str,
            )
        })
        .collect()
}

#[test]
fn all_target_builtins_are_classified() {
    let targets = target_builtins();
    assert!(!targets.is_empty(), "target builtin list must not be empty");

    let mut unrecognized: Vec<String> = Vec::new();
    for (label, iri_str) in &targets {
        let term = iri(iri_str);
        if classify(&term).is_none() {
            unrecognized.push(format!("{label} ({iri_str})"));
        }
    }

    assert!(
        unrecognized.is_empty(),
        "The following {} builtin IRI(s) are NOT recognized by classify() \
         (i.e. resolve to None / are otherwise unsupported by the engine). \
         Every builtin on the target list must be implemented -- do not \
         shrink this list to make the test pass:\n  - {}",
        unrecognized.len(),
        unrecognized.join("\n  - ")
    );
}

/// Sanity check: a handful of reasoner-level builtins (handled in
/// `reasoner/`, not `builtins::evaluate`) must still be classified so
/// "is this predicate a builtin the engine supports somewhere" stays a
/// single centralized check.
#[test]
fn reasoner_level_builtins_are_classified() {
    let log = "http://www.w3.org/2000/10/swap/log#";
    let reasoner_level = [
        "implies",
        "collectAllIn",
        "notIncludes",
        "includes",
        "forAllIn",
        "ifThenElseIn",
    ];

    let mut unrecognized = Vec::new();
    for n in reasoner_level {
        let iri_str = format!("<{log}{n}>");
        let term = iri(&iri_str);
        if classify(&term).is_none() {
            unrecognized.push(iri_str);
        }
    }

    assert!(
        unrecognized.is_empty(),
        "Reasoner-level log: builtins not classified: {:?}",
        unrecognized
    );
}
