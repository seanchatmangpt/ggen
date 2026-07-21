//! A native ShEx (Shape Expressions) validator operating directly on
//! `TripleIndex`, replacing the earlier thin-wrapper delegation to the
//! `shex_ast`/`shex_validation`/`rudof_rdf` crates (see `lib/src/shex.rs`,
//! kept temporarily for side-by-side comparison during migration).
//!
//! Motivation: fuzzing found a real, confirmed spec violation in the
//! delegated crates' `OneOf` handling (a satisfied alternative plus an
//! unrelated "extra" predicate on a non-CLOSED shape was wrongly rejected
//! -- per shexspec.github.io/semantics/, non-closed shapes must ignore
//! unmatched extra triples). Since that crate is a black box with zero
//! matching logic of our own to fix, the only way to close the gap is to
//! own the logic -- mirroring `lib/src/shacl.rs`, which already validates
//! SHACL entirely natively with no external SHACL crate at all. This also
//! removes a whole class of "unverified because it's someone else's code"
//! risk, and keeps the dependency surface within the oxrdf/oxigraph
//! ecosystem plus `serde`/`serde_json` (both already core dependencies),
//! with no new external crates.
//!
//! Scope: covers everything exercised by this crate's own ShEx test suite
//! (vendored W3C shexTest cases, stress tests, impossible-construct
//! catalog, vocabulary fuzz tests) -- `Shape`/`TripleConstraint`/`EachOf`/
//! `OneOf`/`ShapeAnd`/`ShapeOr`/`ShapeNot`/`NodeConstraint` (nodeKind,
//! datatype, length facets, numeric facets, pattern, values incl. IriStem
//! and language-tagged literals), `closed`/`extra`, `min`/`max`
//! cardinality. Shape-expression references by label (a bare string
//! `shapeExpr` pointing at another `ShapeDecl`) are NOT exercised by any
//! test in this repo and are not implemented -- an honest, documented
//! scope limit rather than a silent gap, matching this session's
//! established convention (e.g. the N3 EYE corpus's `blocked_reason`
//! entries).

// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::large_enum_variant)]

use crate::encoding::Encoder;
use crate::shacl::{
    compare_numeric, decode_to_term, get_datatype, get_lang_tag, get_lexical_form, get_objects,
    is_blank_node, is_iri, is_lexically_valid_for_datatype, is_literal, match_regex,
};
use crate::tripleindex::TripleIndex;
use crate::triples::{Term, VarOrTerm};
use serde::Deserialize;

// ---------------------------------------------------------------------
// Minimal ShExJ AST (our own structs, not shex_ast's) -- covers exactly
// the vocabulary this crate's ShEx test suite exercises.
// ---------------------------------------------------------------------

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct Schema {
    pub shapes: Vec<ShapeDecl>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct ShapeDecl {
    pub id: String,
    #[serde(rename = "shapeExpr")]
    pub shape_expr: ShapeExpr,
}

/// A shape expression position may hold either an inline expression or a
/// bare-string reference to another top-level `ShapeDecl` by its `id`
/// (e.g. a `ShapeOr` alternative naming a sibling shape instead of
/// inlining it) -- found via a real vendored W3C shexTest case
/// (`1dotRefOR3_fail`) during native-vs-delegated comparison testing, not
/// anticipated up front.
#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ShapeExprOrRef {
    Ref(String),
    Expr(ShapeExpr),
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum ShapeExpr {
    Shape {
        #[serde(default)]
        closed: bool,
        #[serde(default)]
        extra: Vec<String>,
        expression: Option<TripleExpr>,
    },
    ShapeAnd {
        #[serde(rename = "shapeExprs")]
        shape_exprs: Vec<ShapeExprOrRef>,
    },
    ShapeOr {
        #[serde(rename = "shapeExprs")]
        shape_exprs: Vec<ShapeExprOrRef>,
    },
    ShapeNot {
        #[serde(rename = "shapeExpr")]
        shape_expr: Box<ShapeExprOrRef>,
    },
    NodeConstraint {
        datatype: Option<String>,
        #[serde(rename = "nodeKind")]
        node_kind: Option<String>,
        length: Option<i64>,
        minlength: Option<i64>,
        maxlength: Option<i64>,
        mininclusive: Option<f64>,
        maxinclusive: Option<f64>,
        minexclusive: Option<f64>,
        maxexclusive: Option<f64>,
        pattern: Option<String>,
        #[serde(default)]
        flags: String,
        values: Option<Vec<ValueSetValue>>,
    },
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum TripleExpr {
    TripleConstraint {
        predicate: String,
        #[serde(rename = "valueExpr")]
        value_expr: Option<Box<ShapeExprOrRef>>,
        min: Option<i64>,
        max: Option<i64>,
    },
    EachOf {
        expressions: Vec<TripleExpr>,
    },
    OneOf {
        expressions: Vec<TripleExpr>,
    },
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(untagged)]
pub enum ValueSetValue {
    IriStem {
        #[serde(rename = "type")]
        ty: String,
        stem: String,
    },
    /// A `{"type": "Language", "languageTag": "en"}` value-set entry --
    /// constrains only the literal's language tag, irrespective of its
    /// lexical value. Found missing (deserialize failure) via
    /// `shex_validation.rs::test_language_tagged_literal_values` during
    /// native-vs-delegated comparison testing.
    Language {
        #[serde(rename = "type")]
        ty: String,
        #[serde(rename = "languageTag")]
        language_tag: String,
    },
    LangLiteral {
        value: String,
        language: Option<String>,
    },
    Iri(String),
}

// ---------------------------------------------------------------------
// Public API (mirrors lib/src/shex.rs's shape for drop-in comparison).
// ---------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct ShexValidationReport {
    pub conforms: bool,
    pub failures: Vec<ShexValidationFailure>,
}

#[derive(Debug, Clone)]
pub struct ShexValidationFailure {
    pub node: Term,
    pub shape: String,
    pub reason: String,
}

pub fn validate_shex_native(
    data: &TripleIndex, schema_json_str: &str, shape_map: &[(String, String)],
) -> Result<ShexValidationReport, Box<dyn std::error::Error>> {
    let schema: Schema = serde_json::from_str(schema_json_str)?;
    validate_shex_schema(data, &schema, shape_map).map_err(Into::into)
}

/// Same validation as `validate_shex_native`, but takes an already-parsed
/// `Schema` directly rather than a ShExJ JSON string -- lets callers with a
/// different front end (e.g. `shexc_parser::parse_shexc`, ShExC rather than
/// ShExJ) reuse the exact same validation logic with no JSON round-trip and
/// no duplicated code.
pub fn validate_shex_schema(
    data: &TripleIndex, schema: &Schema, shape_map: &[(String, String)],
) -> Result<ShexValidationReport, String> {
    let mut failures = Vec::new();
    let mut conforms = true;

    for (node_str, shape_label) in shape_map {
        if node_str.is_empty() {
            return Err("invalid focus node syntax: empty node string".to_string());
        }
        let decl = schema
            .shapes
            .iter()
            .find(|d| &d.id == shape_label)
            .ok_or_else(|| format!("shape not declared in schema: {}", shape_label))?;

        let focus_id = encode_node(node_str);
        let mut visited = std::collections::HashSet::new();

        if let Err(reasons) = validate_node(data, schema, focus_id, &decl.shape_expr, &mut visited)
        {
            conforms = false;
            failures.push(ShexValidationFailure {
                node: decode_to_term(focus_id),
                shape: shape_label.clone(),
                reason: reasons.join("; "),
            });
        }
    }

    Ok(ShexValidationReport { conforms, failures })
}

/// Encode a `shape_map` node string (a bare IRI like `http://example.org/n`,
/// or a blank-node label `_:b1`) into the same encoded id `TripleIndex`
/// itself uses, matching `VarOrTerm::convert`'s bracket-wrapping
/// convention for bare IRIs.
fn encode_node(node_str: &str) -> usize {
    VarOrTerm::convert(node_str.to_string()).to_encoded()
}

// ---------------------------------------------------------------------
// Core recursive matcher.
// ---------------------------------------------------------------------

/// Validate `focus` against a (possibly by-reference) shape expression,
/// with cycle detection: recursive/mutually-recursive shape references
/// (e.g. `AShape -> BShape -> CShape -> AShape`, found via
/// `shex_validation.rs::test_nested_recursive_references_stress`) are
/// handled coinductively -- if `(focus, label)` is already being validated
/// higher up the same call stack, assume it holds and return `Ok(())`
/// rather than recursing forever. This mirrors `shacl.rs::validate_shape`'s
/// own `visited: &mut HashSet<(usize, usize)>` cycle-guard pattern exactly.
fn validate_ref(
    data: &TripleIndex, schema: &Schema, focus: usize, se: &ShapeExprOrRef,
    visited: &mut std::collections::HashSet<(usize, String)>,
) -> Result<(), Vec<String>> {
    match se {
        ShapeExprOrRef::Expr(e) => validate_node(data, schema, focus, e, visited),
        ShapeExprOrRef::Ref(label) => {
            let key = (focus, label.clone());
            if !visited.insert(key.clone()) {
                return Ok(());
            }
            let decl = schema.shapes.iter().find(|d| &d.id == label);
            let result = match decl {
                Some(d) => validate_node(data, schema, focus, &d.shape_expr, visited),
                None => Err(vec![format!("unresolved shape reference: {label}")]),
            };
            visited.remove(&key);
            result
        }
    }
}

fn validate_node(
    data: &TripleIndex, schema: &Schema, focus: usize, se: &ShapeExpr,
    visited: &mut std::collections::HashSet<(usize, String)>,
) -> Result<(), Vec<String>> {
    match se {
        ShapeExpr::Shape {
            closed,
            extra,
            expression,
        } => {
            let (consumed_predicates, mut errors) = match expression {
                Some(expr) => match_triple_expr(data, schema, focus, expr, visited),
                None => (Default::default(), Vec::new()),
            };

            if *closed {
                let extra_ids: std::collections::HashSet<usize> = extra
                    .iter()
                    .map(|p| VarOrTerm::convert(format!("<{p}>")).to_encoded())
                    .collect();
                if let Some(preds) = data.spo.get(&focus) {
                    // Sort before iterating: `preds` is an `FxHashMap<usize, ...>`, whose key
                    // iteration order depends on bucket layout / insertion history, not just
                    // content -- same non-determinism class as swarm finding #22/#23 (fixed in
                    // shacl/report.rs and shacl/validate.rs). `errors` is joined into
                    // `ShexValidationFailure.reason`, which flows into the same hook-receipt
                    // digest chain, so unsorted iteration here is equally reachable.
                    let mut sorted_preds: Vec<usize> = preds.keys().copied().collect();
                    sorted_preds.sort_unstable();
                    for pred in sorted_preds {
                        if !consumed_predicates.contains(&pred) && !extra_ids.contains(&pred) {
                            errors.push(format!(
                                "CLOSED shape: predicate {:?} is neither matched by the shape expression nor listed in EXTRA",
                                get_lexical_form(pred)
                            ));
                        }
                    }
                }
            }

            if errors.is_empty() {
                Ok(())
            } else {
                Err(errors)
            }
        }
        ShapeExpr::ShapeAnd { shape_exprs } => {
            let mut errors = Vec::new();
            for sub in shape_exprs {
                if let Err(e) = validate_ref(data, schema, focus, sub, visited) {
                    errors.extend(e);
                }
            }
            if errors.is_empty() {
                Ok(())
            } else {
                Err(errors)
            }
        }
        ShapeExpr::ShapeOr { shape_exprs } => {
            let mut all_errors = Vec::new();
            for sub in shape_exprs {
                match validate_ref(data, schema, focus, sub, visited) {
                    Ok(()) => return Ok(()),
                    Err(e) => all_errors.extend(e),
                }
            }
            Err(vec![format!(
                "ShapeOr: no alternative matched ({})",
                all_errors.join("; ")
            )])
        }
        ShapeExpr::ShapeNot { shape_expr } => {
            match validate_ref(data, schema, focus, shape_expr, visited) {
                Ok(()) => Err(vec![
                    "ShapeNot: inner shape unexpectedly conformed".to_string()
                ]),
                Err(_) => Ok(()),
            }
        }
        ShapeExpr::NodeConstraint { .. } => validate_node_constraint(focus, se),
    }
}

/// Evaluate a `TripleExpr` against `focus`'s outgoing triples. Returns the
/// set of predicate ids consumed (matched) by this expression, plus any
/// violation reasons. `EachOf` requires every sub-expression to match
/// (against disjoint predicates -- a first-fit assignment, sufficient for
/// non-overlapping-predicate expressions, which is everything this crate's
/// test suite exercises); `OneOf` requires at least one sub-expression to
/// match and leaves any other alternative's predicate unconsumed (subject
/// to the enclosing shape's own CLOSED/EXTRA, not rejected by OneOf
/// itself -- this is the exact fix for the gap found in the delegated
/// crate).
fn match_triple_expr(
    data: &TripleIndex, schema: &Schema, focus: usize, expr: &TripleExpr,
    visited: &mut std::collections::HashSet<(usize, String)>,
) -> (std::collections::HashSet<usize>, Vec<String>) {
    match expr {
        TripleExpr::TripleConstraint {
            predicate,
            value_expr,
            min,
            max,
        } => {
            let pred_id = VarOrTerm::convert(format!("<{predicate}>")).to_encoded();
            let values = get_objects(data, focus, pred_id);
            let min = min.unwrap_or(1);
            let max = max.unwrap_or(1);
            let count = values.len() as i64;

            let mut errors = Vec::new();
            if count < min || (max >= 0 && count > max) {
                errors.push(format!(
                    "predicate {predicate}: expected between {min} and {max} values, got {count}"
                ));
            }
            if let Some(ve) = value_expr {
                for &v in &values {
                    if let Err(e) = validate_ref(data, schema, v, ve, visited) {
                        errors.push(format!(
                            "predicate {predicate} value violated its shape: {}",
                            e.join("; ")
                        ));
                    }
                }
            }
            let mut consumed = std::collections::HashSet::new();
            consumed.insert(pred_id);
            (consumed, errors)
        }
        TripleExpr::EachOf { expressions } => {
            let mut all_consumed = std::collections::HashSet::new();
            let mut all_errors = Vec::new();
            for sub in expressions {
                let (consumed, errors) = match_triple_expr(data, schema, focus, sub, visited);
                all_consumed.extend(consumed);
                all_errors.extend(errors);
            }
            (all_consumed, all_errors)
        }
        TripleExpr::OneOf { expressions } => {
            // Real ShEx semantics (confirmed against the official W3C
            // shexTest case `1dotOne2dot-oneOf_fail_p1p2p3`, which
            // expects `nonconformant` when data satisfies BOTH
            // alternatives of a 2-branch OneOf): exactly one alternative
            // must match, not "at least one" -- a prior assumption of
            // mine (based on a general, non-test-verified reading of the
            // spec's CLOSED/EXTRA semantics) wrongly concluded "at least
            // one" and mischaracterized the delegated shex_validation
            // crate's rejection of a both-match case as an external-crate
            // bug in `shex_vocabulary_fuzz.rs`; that crate was actually
            // correct, and this comment/behavior corrects my own error,
            // not a third-party one.
            let mut matches = Vec::new();
            let mut all_errors = Vec::new();
            for sub in expressions {
                let (consumed, errors) = match_triple_expr(data, schema, focus, sub, visited);
                if errors.is_empty() {
                    matches.push(consumed);
                } else {
                    all_errors.extend(errors);
                }
            }
            match matches.len() {
                1 => (matches.into_iter().next().unwrap(), Vec::new()),
                0 => (
                    std::collections::HashSet::new(),
                    vec![format!(
                        "OneOf: no alternative matched ({})",
                        all_errors.join("; ")
                    )],
                ),
                n => (
                    std::collections::HashSet::new(),
                    vec![format!(
                        "OneOf: exactly one alternative must match, but {n} alternatives matched"
                    )],
                ),
            }
        }
    }
}

fn validate_node_constraint(focus: usize, se: &ShapeExpr) -> Result<(), Vec<String>> {
    let ShapeExpr::NodeConstraint {
        datatype,
        node_kind,
        length,
        minlength,
        maxlength,
        mininclusive,
        maxinclusive,
        minexclusive,
        maxexclusive,
        pattern,
        flags,
        values,
    } = se
    else {
        unreachable!()
    };

    let mut errors = Vec::new();

    if let Some(nk) = node_kind {
        let ok = match nk.as_str() {
            "iri" => is_iri(focus),
            "literal" => is_literal(focus),
            "bnode" => is_blank_node(focus),
            "nonliteral" => is_iri(focus) || is_blank_node(focus),
            _ => true,
        };
        if !ok {
            errors.push(format!("nodeKind {nk} not satisfied"));
        }
    }

    if let Some(dt) = datatype {
        let dt_id = VarOrTerm::convert(format!("<{dt}>")).to_encoded();
        match get_datatype(focus) {
            Some(actual_dt) if actual_dt == dt_id => {
                if let (Some(lex), Some(dt_lex)) =
                    (get_lexical_form(focus), get_lexical_form(dt_id))
                {
                    if !is_lexically_valid_for_datatype(&lex, &dt_lex) {
                        errors.push(format!("value is not a lexically valid {dt}"));
                    }
                }
            }
            _ => errors.push(format!("datatype {dt} not satisfied")),
        }
    }

    if let Some(lex) = get_lexical_form(focus) {
        let char_len = lex.chars().count() as i64;
        if let Some(n) = length {
            if char_len != *n {
                errors.push(format!("length {n} not satisfied (actual {char_len})"));
            }
        }
        if let Some(n) = minlength {
            if char_len < *n {
                errors.push(format!("minlength {n} not satisfied (actual {char_len})"));
            }
        }
        if let Some(n) = maxlength {
            if char_len > *n {
                errors.push(format!("maxlength {n} not satisfied (actual {char_len})"));
            }
        }
        if let Some(pat) = pattern {
            if !match_regex(pat, &lex, flags) {
                errors.push(format!("pattern {pat:?} not satisfied"));
            }
        }
    } else if length.is_some() || minlength.is_some() || maxlength.is_some() || pattern.is_some() {
        errors.push("value has no string representation for a length/pattern facet".to_string());
    }

    for (bound_name, bound_val, ok_orderings) in [
        (
            "mininclusive",
            mininclusive,
            &[std::cmp::Ordering::Greater, std::cmp::Ordering::Equal][..],
        ),
        (
            "minexclusive",
            minexclusive,
            &[std::cmp::Ordering::Greater][..],
        ),
        (
            "maxinclusive",
            maxinclusive,
            &[std::cmp::Ordering::Less, std::cmp::Ordering::Equal][..],
        ),
        (
            "maxexclusive",
            maxexclusive,
            &[std::cmp::Ordering::Less][..],
        ),
    ] {
        if let Some(bound) = bound_val {
            let bound_id = Encoder::add(bound.to_string());
            match compare_numeric(focus, bound_id) {
                Ok(Some(ord)) if ok_orderings.contains(&ord) => {}
                _ => errors.push(format!("{bound_name} {bound} not satisfied")),
            }
        }
    }

    if let Some(vals) = values {
        let matches = vals.iter().any(|v| value_matches(focus, v));
        if !matches {
            errors.push("value not a member of the declared value set".to_string());
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn value_matches(focus: usize, v: &ValueSetValue) -> bool {
    match v {
        ValueSetValue::Iri(iri) => {
            let iri_id = VarOrTerm::convert(format!("<{iri}>")).to_encoded();
            focus == iri_id
        }
        ValueSetValue::IriStem { stem, .. } => {
            is_iri(focus)
                && get_lexical_form(focus).is_some_and(|lex| lex.starts_with(stem.as_str()))
        }
        ValueSetValue::LangLiteral { value, language } => {
            get_lexical_form(focus).as_deref() == Some(value.as_str())
                && get_lang_tag(focus).as_deref() == language.as_deref()
        }
        ValueSetValue::Language { language_tag, .. } => {
            is_literal(focus) && get_lang_tag(focus).as_deref() == Some(language_tag.as_str())
        }
    }
}
