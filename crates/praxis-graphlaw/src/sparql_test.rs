#![cfg(test)]
// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(deprecated)]

use super::*;
use crate::{Parser, Syntax};
fn prepare_test() -> TripleIndex {
    //load triples
    let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
    <http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Student> <http://example.com/somethingelse> .
    <http://example.com/foo> <http://test/hasVal> \"1\"^^<http://www.w3.org/2001/XMLSchema#integer> <http://example.com/somethingelse> .
    <http://example.com/foo2> <http://test/hasVal> \"10\"^^<http://www.w3.org/2001/XMLSchema#integer> <http://example.com/somethingelse> .";

    let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
    let mut index = TripleIndex::new();
    triples.into_iter().for_each(|t| index.add(t));
    index
}
#[test]
fn test_filter_greater_than() {
    let index = prepare_test();
    let query_str = "Select * WHERE {  ?s <http://test/hasVal> ?o2  . Filter(?o2 > 1). }";
    let query = Query::parse(query_str, None).unwrap();
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan(&plan, &index);
    assert_eq!(1, iterator.collect::<Vec<Vec<EncodedBinding>>>().len());
}
#[test]
fn test_filter_greater_than_or_equal() {
    let index = prepare_test();
    let query_str = "Select * WHERE {  ?s <http://test/hasVal> ?o2  . Filter(?o2 >= 1). }";
    let query = Query::parse(query_str, None).unwrap();
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan(&plan, &index);
    assert_eq!(2, iterator.collect::<Vec<Vec<EncodedBinding>>>().len());
}
#[test]
fn test_filter_less_than() {
    let index = prepare_test();
    let query_str = "Select * WHERE {  ?s <http://test/hasVal> ?o2  . Filter(?o2 <= 1). }";
    let query = Query::parse(query_str, None).unwrap();
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan(&plan, &index);
    assert_eq!(1, iterator.collect::<Vec<Vec<EncodedBinding>>>().len());
}
#[test]
fn test_group_by_count_aggregation() {
    let index = prepare_test();
    let query_str = "Select (COUNT(?s) AS ?count) WHERE {  ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?o  .  } GroupBy ?s ";
    let query = Query::parse(query_str, None).unwrap();
    println!("{:?}", query);
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan_and_debug(&plan, &index);
    // Expected value updated: CountAccumulator::get() previously interned its result via the
    // generic Encoder::add(), which mis-classifies a bare numeric string as an IRI (no leading
    // ?/_:/", falls through to add_iri) rather than a typed literal -- invisible here since this
    // test only ever decoded the raw string back out, but it silently broke any expression-level
    // use of an aggregate's output (HAVING, ORDER BY) because the mis-typed value couldn't be
    // numerically compared. Fixed via Encoder::add_literal(value, Some(xsd:integer), None); this
    // assertion now reflects the corrected, properly-typed value, matching how every other typed
    // literal in this codebase decodes (see Utils::remove_literal_tags, which exists specifically
    // to strip this same `"value"^^<datatype>` shape for callers that want the bare number).
    let results = vec![vec![Binding {
        var: "count".to_string(),
        val: "\"2\"^^<http://www.w3.org/2001/XMLSchema#integer>".to_string(),
    }]];

    assert_eq!(results, iterator.collect::<Vec<Vec<Binding>>>());
}
#[test]
fn test_group_by_count_aggregation_multiple_group() {
    let index = prepare_test();
    let query_str =
        "Select (SUM(?val) AS ?count) ?s WHERE {  ?s <http://test/hasVal> ?val  .  } GroupBy ?s";
    let query = Query::parse(query_str, None).unwrap();
    println!("{:?}", query);
    let plan = eval_query(&query, &index);
    let mut iterator = evaluate_plan_and_debug(&plan, &index);

    let row1 = iterator.next().unwrap();
    assert_eq!(2, row1.len());
    let row2 = iterator.next().unwrap();
    assert_eq!(2, row2.len());
    assert!(iterator.next().is_none());
}
#[test]
fn test_group_by_sum_aggregation() {
    println!("Encoder s: {:?}", Encoder::get("s"));
    println!("Encoder ?s: {:?}", Encoder::get("?s"));
    println!("Encoder val: {:?}", Encoder::get("val"));
    println!("Encoder ?val: {:?}", Encoder::get("?val"));
    let index = prepare_test();
    let query_str = "Select (SUM(?val) AS ?sum) ?s WHERE {  ?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?val.  } GroupBy ?s ";
    let query = Query::parse(query_str, None).unwrap();
    println!("{:?}", query);
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan_and_debug(&plan, &index);
    // Expected value updated: SumAccumulator::get() now uses
    // Encoder::add_literal(..., Some(xsd:decimal), None) instead of the generic Encoder::add(),
    // which mis-classified the bare "0" as an IRI -- this is the same root cause and fix as
    // test_group_by_count_aggregation above. Unlike COUNT's xsd:integer result, though, this
    // query aliases the aggregate ("AS ?sum"), which routes the value through an Extend/rename
    // step in sparql/eval.rs's PlanExpression::Variable evaluator; that evaluator only
    // special-cases xsd:integer/xsd:boolean for typed re-encoding, so an xsd:decimal literal
    // falls through to its generic StringLiteral branch, which strips the datatype tag via
    // Utils::remove_literal_tags on the way out. The value is untagged here ("0", not
    // "0"^^<...decimal>) for that reason -- see the matching disclosure comments in
    // sparql/accumulators.rs's SumAccumulator::get() and sparql/eval.rs, and the equivalent
    // regression test lib_test.rs::sum_over_untagged_literals_computes_the_real_value.
    let results = vec![vec![
        Binding {
            var: "s".to_string(),
            val: "<http://example.com/foo>".to_string(),
        },
        Binding {
            var: "sum".to_string(),
            val: "\"0\"".to_string(),
        },
    ]];

    assert_eq!(results, iterator.collect::<Vec<Vec<Binding>>>());
}

#[test]
fn test_syntactic_sugar_rdf_type() {
    let index = prepare_test();
    let query_str = "Select * WHERE {  ?s a ?val.  }";
    let query = Query::parse(query_str, None).unwrap();
    println!("{:?}", query);
    let plan = eval_query(&query, &index);
    let iterator = evaluate_plan_and_debug(&plan, &index);

    assert_eq!(2, iterator.collect::<Vec<Vec<Binding>>>().len());
}
