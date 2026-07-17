#![cfg(test)]

use super::*;

#[test]
fn parse_tp() {
    let rules = parse(
        "@prefix log: <http://www.w3.org/2000/10/swap/log#>.\n\
         {?VaRr0 <http://test.be/pieter> ?lastVar. ?VaRr0 log:type ?lastVar.}=>{?VaRr0 <ssn:HasValue> ?lastVar.}",
    )
    .unwrap();
    assert_eq!(rules.first().unwrap().body.len(), 2);
}

#[test]
fn parse_multiple_prefixes() {
    let rules = parse(
        "@prefix log: <http://www.w3.org/2000/10/swap/log#>.\n\
         @prefix log2: <http://www.w3.org/2000/10/swap/log2#>.\n\
         {?VaRr0 <http://test.be/pieter> ?lastVar. ?VaRr0 log:type ?lastVar.}=>{?VaRr0 <ssn:HasValue> ?lastVar.}",
    )
    .unwrap();
    assert_eq!(rules.first().unwrap().body.len(), 2);
}

#[test]
fn parse_multiple_rules() {
    let rules = parse(
        "@prefix log: <http://www.w3.org/2000/10/swap/log#>.\n\
         @prefix log2: <http://www.w3.org/2000/10/swap/log2#>.\n\
         {?VaRr0 <http://test.be/pieter> ?lastVar. ?VaRr0 log:type ?lastVar.}=>{?VaRr0 <ssn:HasValue> ?lastVar.}\n\
         {?s <http://test.be/pieter> ?o.}=>{?s <ssn:HasValue> ?o.}",
    )
    .unwrap();
    assert_eq!(rules.len(), 2);
}

#[test]
fn parse_multiple_rules_ending_with_dot() {
    let rules = parse(
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix : <http://eulersharp.sourceforge.net/2009/12dtb/test#>.\n\
         {?V0 rdf:type :N0} => {?V0 rdf:type :N1}.\n\
         {?V0 rdf:type :N1} => {?V0 rdf:type :N2}.",
    )
    .unwrap();
    assert_eq!(rules.len(), 2);
}

#[test]
fn parse_empty_rule() {
    let rules = parse("").unwrap();
    assert_eq!(0, rules.len());
}

#[test]
fn parse_rule_with_multiple_spaces() {
    let rules = parse(
        "{  ?VaRr0   <http://test.be/pieter>   ?lastVar.\n\
          ?VaRr0 <http://www.w3.org/2000/10/swap/log#type> ?lastVar.\n\
         }=>{ ?VaRr0  <ssn:HasValue>  ?lastVar .\n}.\n",
    )
    .unwrap();
    assert!(!rules.is_empty());
    assert_eq!(rules[0].body.len(), 2);
}

#[test]
fn parse_rule_with_a_syntactic_sugar() {
    // "a" in property position should expand to rdf:type
    let rules = parse(
        "{?VaRr0 <http://test.be/pieter> ?lastVar. ?VaRr0 a ?lastVar.}=>{?VaRr0 <ssn:HasValue> ?lastVar.}.",
    )
    .unwrap();
    let rule = rules.first().unwrap();
    // Second body literal should have rdf:type as predicate
    let second_body = &rule.body[1];
    let decoded_p =
        crate::encoding::Encoder::decode(&second_body.pattern.p.to_encoded()).unwrap_or_default();
    assert!(
        decoded_p.contains("type"),
        "Expected rdf:type expansion, got: {}",
        decoded_p
    );
}

#[test]
fn parse_negated_body_literal() {
    let rules = parse(
        "{?x <http://example/p> ?y. not {?x <http://example/q> ?y.}}=>{?x <http://example/r> ?y.}",
    )
    .unwrap();
    let rule = rules.first().unwrap();
    assert_eq!(rule.body.len(), 2);
    assert!(!rule.body[0].negated);
    assert!(rule.body[1].negated);
}

#[test]
fn parse_rule_with_comment() {
    let input = "# This is a comment\n\
                 {?s <http://example/p> ?o.} => {?s <http://example/q> ?o.} # inline comment\n";
    let rules = parse(input).unwrap();
    assert_eq!(rules.len(), 1);
}

#[test]
fn parse_multi_head_rule() {
    // A rule with two TPs in the head should produce two separate rules
    let rules = parse(
        "{?s <http://example/p> ?o.} => {?s <http://example/q> ?o. ?s <http://example/r> ?o.}",
    )
    .unwrap();
    assert_eq!(
        rules.len(),
        2,
        "Multi-head rule should desugar into 2 rules"
    );
    assert_eq!(rules[0].body.len(), 1);
    assert_eq!(rules[1].body.len(), 1);
}

#[test]
fn parse_string_literal_in_object() {
    let rules =
        parse(r#"{?s <http://example/hasName> "Alice".} => {?s <http://example/known> "yes".}"#)
            .unwrap();
    assert_eq!(rules.len(), 1);
}

#[test]
fn parse_blank_node_in_body() {
    let rules = parse("{_:x <http://example/p> ?o.} => {_:x <http://example/q> ?o.}").unwrap();
    assert_eq!(rules.len(), 1);
}
