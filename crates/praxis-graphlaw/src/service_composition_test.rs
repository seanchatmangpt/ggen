#![cfg(test)]
// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity)]

use crate::bindings::Binding;
use crate::reasoner::Reasoner;
use crate::{
    BackwardChainer, Encoder, Rule, RuleIndex, Triple, TripleIndex, TripleStore, VarOrTerm,
};
use log::debug;
use std::collections::HashMap;
use std::sync::Arc; // Use log crate when building application

pub fn eval_backward_with_history(
    triple_index: &TripleIndex,
    rule_index: &RuleIndex,
    rule_head: &Triple,
    history: &mut Vec<Triple>,
) -> Binding {
    let sub_rules: Vec<(Arc<Rule>, Vec<(usize, usize)>)> =
        BackwardChainer::find_subrules(rule_index, rule_head);
    let mut all_bindings = Binding::new();
    for (sub_rule, var_subs) in sub_rules.into_iter() {
        debug!(
            "Backchainging rule: {:?}",
            TripleStore::decode_rule(&sub_rule)
        );
        let mut rule_bindings = Binding::new();
        for body_lit in &sub_rule.body {
            let rule_atom = &body_lit.pattern;
            debug!("Matching body: {:?}", TripleStore::decode_triple(rule_atom));

            if let Some(result_bindings) = triple_index.query(rule_atom, None) {
                debug!(
                    "   Found matching body: {:?}",
                    TripleStore::decode_bindings(&result_bindings)
                );

                let visited_triples =
                    Reasoner::substitute_triple_with_bindings(rule_atom, &result_bindings);
                //check if visited triples are already contained in history
                if visited_triples.iter().all(|item| history.contains(item)) {
                    break;
                }
                history.extend(visited_triples);
                rule_bindings = rule_bindings.join(&result_bindings);
            }
            //recursive call
            let recursive_bindings =
                eval_backward_with_history(triple_index, rule_index, rule_atom, history);
            rule_bindings.combine(recursive_bindings);
        }

        //rename variables
        let renamed = rule_bindings.rename(var_subs);
        all_bindings.combine(renamed);
    }
    all_bindings
}

#[test]
fn test_eval_backward_rule() {
    let data="@prefix test: <http://test.org/> \n\
        <http://example2.com/mlModel1> a test:MLModel.\n\
                <http://example2.com/mlModel1> a test:Action.\n\
                <http://example2.com/mlModel1> test:hasInput <http://example2.com/relational_normalied>.\n\
                <http://example2.com/mlModel1> test:requirement <http://example2.com/normalisation>.\n\
                <http://example2.com/normalizer> a test:MLModel.\n\
                <http://example2.com/normalizer> test:hasInput <http://example2.com/relational_nonnormalized>.\n\
                <http://example2.com/normalizer> test:hasOutput <http://example2.com/relational_normalied>.\n\
                <http://example2.com/normalizer> test:hasFunction <http://example2.com/normalisation>.\n\
                <http://example2.com/temperatureSensor> a test:Source.\n\
                <http://example2.com/temperatureSensor> test:hasOutput <http://example2.com/relational_nonnormalized>.\n\
                {?source a test:Source. ?source test:hasOutput ?output. }=>{?source a test:Test. }.\n\
             {?x test:hasInput ?input. ?x test:requirement ?req. ?y test:hasOutput ?input. ?y test:hasFunction ?req.}=>{?y test:hasInput ?input.}\n\
             {?source a test:Source. ?source test:hasOutput ?output. ?y test:hasInput ?output }=>{?source a test:NeededInput. }.";

    let store = TripleStore::from(data);
    let backward_head = Triple {
        s: VarOrTerm::new_var("?newVar".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("test:NeededInput".to_string()),
        g: None,
    };
    let var_encoded = Encoder::add("?newVar".to_string());
    let result_encoded = Encoder::add("<http://example2.com/temperatureSensor>".to_string());
    let mut history = Vec::new();
    let bindings = eval_backward_with_history(
        &store.triple_index,
        &store.rules_index,
        &backward_head,
        &mut history,
    );
    println!("History: {}", TripleStore::decode_triples(&history));
    println!("Bindings {}", TripleStore::decode_bindings(&bindings));
    let result_bindings = HashMap::from([(var_encoded, Vec::from([result_encoded]))]);
    assert_eq!(result_bindings.get(&12), bindings.get(&12));
}
#[test]
fn test_eval_forward_rule() {
    let data="@prefix test: <http://test.org/> \n\
        <http://example2.com/mlModel1> a test:MLModel.\n\
                <http://example2.com/mlModel1> a test:Action.\n\
                <http://example2.com/mlModel1> test:hasInput <http://example2.com/relational_normalied>.\n\
                <http://example2.com/mlModel1> test:requirement <http://example2.com/normalisation>.\n\
                <http://example2.com/normalizer> a test:MLModel.\n\
                <http://example2.com/normalizer> test:hasInput <http://example2.com/relational_nonnormalized>.\n\
                <http://example2.com/normalizer> test:hasOutput <http://example2.com/relational_normalied>.\n\
                <http://example2.com/normalizer> test:hasFunction <http://example2.com/normalisation>.\n\
                <http://example2.com/temperatureSensor> a test:Source.\n\
                <http://example2.com/temperatureSensor> test:hasOutput <http://example2.com/relational_nonnormalized>.\n\
                {?source a test:Source. ?source test:hasOutput ?output. }=>{?source a test:Test. }.\n\
             {?x test:hasInput ?input. ?x test:requirement ?req. ?y test:hasOutput ?input. ?y test:hasFunction ?req.}=>{?y test:hasInput ?input.}\n\
             {?source a test:Source. ?source test:hasOutput ?output. ?y test:hasInput ?output }=>{?source <http://test.org/defines> <http://test.org/NeededInput>. }.";
    let mut store = TripleStore::from(data);
    println!("store size {:?}", store.len());
    store.materialize().unwrap();
    println!("store size {:?}", store.len());
    match store.query("Select * WHERE{?s <http://test.org/defines> ?o}") {
        Ok(result) => assert_ne!(0, result.len()),
        Err(_err) => assert_eq!(0, 1),
    }
}
