#![cfg(test)]

use crate::csprite::CSpriteReasoner;
use crate::reasoner::Reasoner;
use crate::{Binding, Parser, Triple, VarOrTerm};

#[test]
#[ignore]
fn test_reconstruct_from_bindings() {
    let data = "{?a in ?c}=>{?a in ?c}";
    let (_content, rules) = Parser::parse(data.to_string());
    println!("encoded {:?}", rules);

    assert_eq!(1, rules.len());
    let rule = &rules[0];

    // Derive the variable/term IDs from the parsed rule instead of hardcoding,
    // so this test is robust against global Encoder state from other tests.
    let body_triple = &rule.body[0].pattern;
    let var_a_id = body_triple.s.as_var().name;
    let in_term_id = body_triple.p.as_term().id();
    let var_c_id = body_triple.o.as_var().name;

    // Bind ?a → 10, ?c → 11 (arbitrary placeholder term IDs for the test)
    let mut result_bindings: Binding = Binding::new();
    result_bindings.add(&var_a_id, 10);
    result_bindings.add(&var_c_id, 11);

    let expected = vec![vec![Triple {
        s: VarOrTerm::new_encoded_term(10),
        p: VarOrTerm::new_encoded_term(in_term_id),
        o: VarOrTerm::new_encoded_term(11),
        g: None,
    }]];

    let triples = CSpriteReasoner::reconstruct_triples_from_bindings(&mut result_bindings, rule);
    assert_eq!(expected, triples);
}

#[test]
fn test_rule_substitution() {
    let data = ":a in :b.\n\
                {?a in ?b.?b in ?c}=>{?a in ?c}\n\
                {:a in :b.:b in ?c}=>{:a in ?c}\n\
                {?a in :a.:a in :b}=>{?a in :b}";
    let (content, rules) = Parser::parse(data.to_string());
    let matching_triple = content.first().unwrap();
    let matching_rule = rules.first().unwrap();
    let results = Reasoner::substitute_rule(matching_triple, matching_rule);
    assert_eq!(&rules[1..], results);
}
