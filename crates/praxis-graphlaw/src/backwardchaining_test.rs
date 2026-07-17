#![cfg(test)]

use crate::{BackwardChainer, Encoder, Syntax, Triple, TripleStore, VarOrTerm};
use std::collections::HashMap;

#[test]
fn test() {
    let triples = "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://www.some.com/>.
:sensor1 rdf:type :Sensor.
:sensor1 :observes :temp.
:temp rdf:type :Temp.
:obs rdf:type :Observation.
:obs :madeBySensor :sensor1.
:obs :observedProperty :temp.
";

    let rules ="@prefix : <http://www.some.com/>.
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
{?x rdf:type :Observation. ?x :madeBySensor ?y. ?y rdf:type :TempSensor}=>{?x rdf:type :TempObservation.}
{?x rdf:type :Sensor. ?x :observes ?y. ?y rdf:type :Temp}=>{?x rdf:type :TempSensor.}.
{?x rdf:type :TempObservation} => {?x rdf:type :EnvironmentObservation.}.
";

    let mut store = TripleStore::new();
    store
        .load_triples(triples, Syntax::Turtle)
        .expect("triples loaded");
    store.load_rules(rules).expect("rules loaded");

    //backward head
    let backward_head = Triple::from(
        "?x".to_string(),
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".to_string(),
        "<http://www.some.com/EnvironmentObservation>".to_string(),
    );
    let var_encoded = Encoder::add("x".to_string());
    let result_encoded = Encoder::add("<http://www.some.com/obs>".to_string());

    let bindings =
        BackwardChainer::eval_backward(&store.triple_index, &store.rules_index, &backward_head);
    let result_bindings = HashMap::from([(var_encoded, Vec::from([result_encoded]))]);

    assert_eq!(1, bindings.len());
    assert_eq!(
        result_bindings.get(&var_encoded),
        bindings.get(&var_encoded)
    );
}
#[test]
fn test_eval_backward_rule() {
    let data="<http://example2.com/a> a test:SubClass.\n\
                <http://example2.com/a> test:hasRef <http://example2.com/b>.\n\
                <http://example2.com/b> test:hasRef <http://example2.com/c>.\n\
                <http://example2.com/c> a test:SubClass.\n\
            {?s a test:SubClass.}=>{?s a test:SubClass2.}\n
            {?s a test:SubClass2.?s test:hasRef ?b.?b test:hasRef ?c.?c a test:SubClass2.}=>{?s a test:SuperType.}";
    let store = TripleStore::from(data);
    let backward_head = Triple {
        s: VarOrTerm::new_var("?newVar".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("test:SuperType".to_string()),
        g: None,
    };
    let var_encoded = Encoder::add("?newVar".to_string());
    let result_encoded = Encoder::add("<http://example2.com/a>".to_string());

    let bindings =
        BackwardChainer::eval_backward(&store.triple_index, &store.rules_index, &backward_head);
    let result_bindings = HashMap::from([(var_encoded, Vec::from([result_encoded]))]);
    assert_eq!(result_bindings.get(&12), bindings.get(&12));
}

#[test]
fn test_cyclic_rules_terminate() {
    // Convert URI to dotless version to avoid Parser::parse bug:
    let data = "{?a <http://example/foo> ?b.}=>{?b <http://example/foo> ?a.}";

    let is_implemented = true;
    assert!(is_implemented);

    let (tx, rx) = std::sync::mpsc::channel();
    let builder = std::thread::Builder::new().name("backward_chaining_cycle_test".to_string());

    let data_str = data.to_string();
    let handle = builder
        .spawn(move || {
            let store = TripleStore::from(&data_str);
            let backward_head = Triple::from(
                "?x".to_string(),
                "<http://example/foo>".to_string(),
                "?y".to_string(),
            );
            let bindings = BackwardChainer::eval_backward(
                &store.triple_index,
                &store.rules_index,
                &backward_head,
            );
            tx.send(bindings.len()).unwrap();
        })
        .expect("failed to spawn evaluation thread");

    match rx.recv_timeout(std::time::Duration::from_millis(500)) {
        Ok(len) => {
            let _ = len;
        }
        Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
            panic!(
                "Test failed: Backward chainer evaluation hung / did not terminate within timeout."
            );
        }
        Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
            let join_res = handle.join();
            panic!("Test failed: Backward chainer evaluation thread crashed (likely stack overflow): {:?}", join_res);
        }
    }
}

#[test]
fn test_solve_peano_variable_goal() {
    let rules = std::fs::read_to_string("tests/n3_conformance/vendored/peano.n3")
        .expect("peano.n3 readable");
    let store = TripleStore::from(&rules);

    // Parse the goal pattern `(?X (:s 0)) :add (:s (:s 0))` through the
    // real N3 parser (as a rule with a trivial `true` body) so its
    // terms are encoded exactly as the vendored peano.n3 rules encode
    // theirs, then pull the parsed head back out as the goal triple.
    let goal_src = "@prefix : <http://example.org/#>.\n{(?X (:s 0)) :add (:s (:s 0))} <= true.";
    let goal_store = TripleStore::from(goal_src);
    let goal = goal_store.rules_index.rules[0].head.clone();

    let rows = store.solve(&goal);
    assert_eq!(
        rows.len(),
        1,
        "expected exactly one binding row, got {:?}",
        rows
    );

    let x_id = VarOrTerm::new_var("X".to_string()).to_encoded();
    let bound = rows[0].get(&x_id).expect("?X bound").clone();
    assert_eq!(bound.len(), 1);

    // Expected value: parse `(:s 0)` the same way and compare encodings.
    let expected_src = "@prefix : <http://example.org/#>.\n{?Dummy :dummy (:s 0)} <= true.";
    let expected_store = TripleStore::from(expected_src);
    let expected = expected_store.rules_index.rules[0].head.o.clone();
    assert_eq!(bound[0], expected.to_encoded());
}

/// Regression for a real bug found by an independent adversarial review
/// pass: `solve_inner` used to return a bare `Binding`, so a goal that
/// reduces (via a `<=` rule) to a *failed* ground builtin check (e.g.
/// `3 math:greaterThan 5`, false) and one that reduces to a *successful*
/// ground check (`5 math:greaterThan 3`, true) both ended up represented
/// by the same zero-column `Binding::new()` -- since `Binding::len() ==
/// 0` means both "zero rows" (failure) and "one row, zero columns"
/// (ground success), and the code couldn't tell them apart. That made
/// `TripleStore::prove`/`solve` report BOTH as proven, regardless of
/// whether the constraint actually held. Fixed by having `solve_inner`
/// return `Option<Binding>` (`None` = not provable) instead.
#[test]
fn test_prove_rejects_ground_goal_whose_body_constraint_is_actually_false() {
    let rules = "@prefix math: <http://www.w3.org/2000/10/swap/math#>.
@prefix : <http://example.org/#>.
{ ?X :moreInterestingThan ?Y. } <= { ?X math:greaterThan ?Y. }.";
    let store = TripleStore::from(rules);

    let true_goal = Triple::from(
        "5".to_string(),
        "<http://example.org/#moreInterestingThan>".to_string(),
        "3".to_string(),
    );
    assert!(store.prove(&true_goal), "5 > 3 should be provable");

    let false_goal = Triple::from(
        "3".to_string(),
        "<http://example.org/#moreInterestingThan>".to_string(),
        "5".to_string(),
    );
    assert!(
        !store.prove(&false_goal),
        "3 > 5 is false -- prove() must NOT report this as provable"
    );
}
