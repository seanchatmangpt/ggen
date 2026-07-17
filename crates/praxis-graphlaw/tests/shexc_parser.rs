//! Verifies the ShExC (compact syntax) parser (`praxis_graphlaw::shexc_parser`)
//! against every vendored W3C shexTest case's real original `source_shexc`
//! (see `lib/tests/shex_conformance/w3c_suite/*/meta.json`), diffing the
//! resulting `Schema` against the known-correct ShExJ (`schema.json`) sitting
//! right next to it for that same case. This is real, free conformance
//! evidence -- one independent round-trip check per vendored case, with zero
//! new corpus vendoring -- for "does this parser produce the exact same AST
//! as the already-verified-correct hand-derived ShExJ."
//!
//! Shape declaration order is not required to match (compared as a
//! by-id-sorted set) since the original ShExC source and its hand-derived
//! ShExJ sibling were not guaranteed to preserve declaration order during
//! translation; only the *set* of shape declarations and their content is a
//! meaningful correctness signal here.

use praxis_graphlaw::shex_native::{Schema, ShapeDecl};
use praxis_graphlaw::shexc_parser::parse_shexc;
use std::fs;
use std::path::Path;

#[derive(serde::Deserialize)]
struct CaseMeta {
    source_shexc: String,
}

fn sorted_shapes(schema: Schema) -> Vec<ShapeDecl> {
    let mut shapes = schema.shapes;
    shapes.sort_by(|a, b| a.id.cmp(&b.id));
    shapes
}

#[test]
fn shexc_parser_matches_vendored_shexj_for_every_case_with_source() {
    let cases_dir =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/shex_conformance/w3c_suite/cases");
    let mut case_dirs: Vec<_> = fs::read_dir(&cases_dir)
        .expect("failed to read w3c_suite/cases")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_dir())
        .collect();
    case_dirs.sort();
    assert!(!case_dirs.is_empty(), "no vendored ShEx cases found");

    let mut checked = 0usize;
    let mut mismatches = Vec::new();

    for case_dir in &case_dirs {
        let name = case_dir.file_name().unwrap().to_string_lossy().to_string();
        let meta_content = fs::read_to_string(case_dir.join("meta.json"))
            .unwrap_or_else(|e| panic!("case '{name}': failed to read meta.json: {e}"));
        let meta: CaseMeta = match serde_json::from_str(&meta_content) {
            Ok(m) => m,
            Err(e) => panic!("case '{name}': failed to parse meta.json: {e}"),
        };

        // `source_shexc` sometimes carries a human explanatory prefix rather
        // than pure ShExC (a few cases' fields are free-text descriptions,
        // not parseable source) -- only exercise cases where it looks like
        // real ShExC (starts with a shape label or PREFIX/BASE directive).
        let trimmed = meta.source_shexc.trim();
        let looks_parseable = trimmed.starts_with('<')
            || trimmed.starts_with("PREFIX")
            || trimmed.starts_with("BASE");
        if !looks_parseable {
            continue;
        }

        let schema_json = fs::read_to_string(case_dir.join("schema.json"))
            .unwrap_or_else(|e| panic!("case '{name}': failed to read schema.json: {e}"));
        let expected: Schema = serde_json::from_str(&schema_json)
            .unwrap_or_else(|e| panic!("case '{name}': failed to parse schema.json: {e}"));

        let actual = match parse_shexc(&meta.source_shexc) {
            Ok(s) => s,
            Err(e) => {
                mismatches.push(format!("{name}: ShExC parse error: {e}"));
                continue;
            }
        };

        checked += 1;
        let expected_sorted = sorted_shapes(expected);
        let actual_sorted = sorted_shapes(actual);
        if expected_sorted != actual_sorted {
            mismatches.push(format!(
                "{name}: AST mismatch\n  expected: {expected_sorted:#?}\n  actual:   {actual_sorted:#?}"
            ));
        }
    }

    assert!(
        checked > 0,
        "no vendored case had a parseable source_shexc field"
    );
    assert!(
        mismatches.is_empty(),
        "{} of {} ShExC round-trip cases mismatched:\n{}",
        mismatches.len(),
        checked,
        mismatches.join("\n\n")
    );
}

// ---------------------------------------------------------------------
// Hand-authored scope-boundary tests.
// ---------------------------------------------------------------------

use praxis_graphlaw::shex_native::{ShapeExpr, ShapeExprOrRef, TripleExpr};

fn one_shape(schema: &Schema) -> &ShapeDecl {
    assert_eq!(
        schema.shapes.len(),
        1,
        "expected exactly one shape decl, got {:?}",
        schema.shapes
    );
    &schema.shapes[0]
}

#[test]
fn prefix_and_base_resolution() {
    let src = "PREFIX ex: <http://example.org/>\n<http://example.org/S> { ex:p . }";
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    assert_eq!(decl.id, "http://example.org/S");
    let ShapeExpr::Shape {
        expression: Some(TripleExpr::TripleConstraint { predicate, .. }),
        ..
    } = &decl.shape_expr
    else {
        panic!(
            "expected a Shape with one TripleConstraint, got {:?}",
            decl.shape_expr
        );
    };
    assert_eq!(predicate, "http://example.org/p");
}

#[test]
fn base_relative_iri_resolution() {
    let src = "BASE <http://example.org/schema#>\n<#S> { <#p> . }";
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    assert_eq!(decl.id, "http://example.org/schema#S");
}

fn cardinality_of(src: &str) -> (Option<i64>, Option<i64>) {
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    let ShapeExpr::Shape {
        expression: Some(TripleExpr::TripleConstraint { min, max, .. }),
        ..
    } = &decl.shape_expr
    else {
        panic!(
            "expected a Shape with one TripleConstraint, got {:?}",
            decl.shape_expr
        );
    };
    (*min, *max)
}

#[test]
fn cardinality_shorthands() {
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . }"),
        (None, None)
    );
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . * }"),
        (Some(0), Some(-1))
    );
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . + }"),
        (Some(1), Some(-1))
    );
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . ? }"),
        (Some(0), Some(1))
    );
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . {2,5} }"),
        (Some(2), Some(5))
    );
    assert_eq!(
        cardinality_of("<http://a.example/S> { <http://a.example/p> . {3,} }"),
        (Some(3), Some(-1))
    );
}

#[test]
fn nested_and_or_not() {
    let src = "<http://a.example/S> { <http://a.example/p> (IRI OR BNODE) AND NOT LITERAL }";
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    let ShapeExpr::Shape {
        expression:
            Some(TripleExpr::TripleConstraint {
                value_expr: Some(ve),
                ..
            }),
        ..
    } = &decl.shape_expr
    else {
        panic!(
            "expected a TripleConstraint with a value expression, got {:?}",
            decl.shape_expr
        );
    };
    let ShapeExprOrRef::Expr(ShapeExpr::ShapeAnd { shape_exprs }) = ve.as_ref() else {
        panic!("expected a top-level ShapeAnd, got {:?}", ve);
    };
    assert_eq!(
        shape_exprs.len(),
        2,
        "expected (IRI OR BNODE) AND (NOT LITERAL), got {:?}",
        shape_exprs
    );
    let ShapeExprOrRef::Expr(ShapeExpr::ShapeOr {
        shape_exprs: or_alts,
    }) = &shape_exprs[0]
    else {
        panic!(
            "expected the first AND operand to be a ShapeOr, got {:?}",
            shape_exprs[0]
        );
    };
    assert_eq!(or_alts.len(), 2);
    let ShapeExprOrRef::Expr(ShapeExpr::ShapeNot { .. }) = &shape_exprs[1] else {
        panic!(
            "expected the second AND operand to be a ShapeNot, got {:?}",
            shape_exprs[1]
        );
    };
}

#[test]
fn extra_and_closed() {
    let src = "<http://a.example/S> CLOSED EXTRA <http://a.example/p2> { <http://a.example/p1> . }";
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    let ShapeExpr::Shape { closed, extra, .. } = &decl.shape_expr else {
        panic!("expected a Shape, got {:?}", decl.shape_expr);
    };
    assert!(*closed);
    assert_eq!(extra, &vec!["http://a.example/p2".to_string()]);
}

#[test]
fn value_set_with_stem_and_language_tag() {
    let src = "<http://a.example/S> { <http://a.example/p> [<http://a.example/x>~ \"hello\"@en] }";
    let schema = parse_shexc(src).unwrap();
    let decl = one_shape(&schema);
    let ShapeExpr::Shape {
        expression:
            Some(TripleExpr::TripleConstraint {
                value_expr: Some(ve),
                ..
            }),
        ..
    } = &decl.shape_expr
    else {
        panic!(
            "expected a TripleConstraint with a value expression, got {:?}",
            decl.shape_expr
        );
    };
    let ShapeExprOrRef::Expr(ShapeExpr::NodeConstraint {
        values: Some(values),
        ..
    }) = ve.as_ref()
    else {
        panic!("expected a NodeConstraint with a value set, got {:?}", ve);
    };
    assert_eq!(
        values.len(),
        2,
        "expected an IRI stem and a language-tagged literal, got {:?}",
        values
    );
}

#[test]
fn end_to_end_validation_against_real_data() {
    use praxis_graphlaw::parser::{Parser as RoxiParser, Syntax};
    use praxis_graphlaw::TripleStore;

    let src = "PREFIX ex: <http://a.example/>\nex:S { ex:p1 . }";
    let data_ttl = "<http://a.example/n1> <http://a.example/p1> \"v\" .";
    let triples = RoxiParser::parse_triples(data_ttl, Syntax::Turtle).unwrap();
    let mut store = TripleStore::new();
    for t in triples {
        store.triple_index.add(t);
    }
    let report = store
        .validate_shex_c(
            src,
            &[(
                "http://a.example/n1".to_string(),
                "http://a.example/S".to_string(),
            )],
        )
        .expect("validate_shex_c should succeed");
    assert!(report.conforms, "expected conformance, got {:?}", report);

    let bad_report = store
        .validate_shex_c(
            src,
            &[(
                "http://a.example/nonexistent".to_string(),
                "http://a.example/S".to_string(),
            )],
        )
        .expect("validate_shex_c should succeed");
    assert!(
        !bad_report.conforms,
        "a node missing the required predicate must not conform"
    );
}

#[test]
fn out_of_scope_semantic_action_returns_clear_error_not_panic() {
    // Semantic actions (`%...%`) are explicitly out of scope (see
    // shexc_parser.rs's module doc) -- this must return a `Result::Err`,
    // never panic and never silently ignore the construct.
    let src = "<http://a.example/S> { <http://a.example/p> . %javascript{ doSomething(); }% }";
    let result = parse_shexc(src);
    assert!(
        result.is_err(),
        "semantic actions are out of scope and must produce Err, got {:?}",
        result
    );
}

#[test]
fn out_of_scope_triple_expr_label_returns_clear_error_not_panic() {
    // `$label` triple-expression labels are explicitly out of scope.
    let src = "<http://a.example/S> { $<http://a.example/te1> <http://a.example/p> . }";
    let result = parse_shexc(src);
    assert!(
        result.is_err(),
        "triple-expression labels are out of scope and must produce Err, got {:?}",
        result
    );
}
