#![cfg(test)]

use crate::csprite::{CSprite, CSpriteReasoner};
use crate::{
    Binding, BodyLiteral, Parser, QueryEngine, Rule, SimpleQueryEngine, Triple, VarOrTerm,
};
use std::rc::Rc;
use std::sync::Arc;

#[test]
fn test_sprite_compute() {
    let data="<http://example2.com/a> a test:SubClass.\n\
                <http://example2.com/a> test:hasRef <http://example2.com/b>.\n\
                <http://example2.com/b> test:hasRef <http://example2.com/c>.\n\
                <http://example2.com/c> a test:SubClassH1.\n\
            {?s a test:SubClass.}=>{?s a test:SubClass2.}\n\
            {?s a test:SubClass2.}=>{?s a test:SubClass.}\n\
            {?s a test:SubClass0.}=>{?s a test:SubClass2.}\n\
            {?s a test:SubClass01.}=>{?s a test:SubClass0.}\n\
            {?s a test:SubClassH1.}=>{?s a test:SubClassH.}\n\
            {?s a test:SubClassH2.}=>{?s a test:SubClassH1.}\n\
            {?s a test:SubClassH22.}=>{?s a test:SubClassH1.}\n\
            {?s a test:SubClass2.?s test:hasRef ?b.?b test:hasRef ?c.?c a test:SubClassH.}=>{?s a test:SuperType.}\n\
            {?super a test:SuperType.}=>{?super a test:SuperType3.}";
    let mut store = CSprite::from(data);

    let backward_head = Triple {
        s: VarOrTerm::new_var("?newVar".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("test:SuperType".to_string()),
        g: None,
    };

    //assert_eq!(4,store.len());
    let validation_triple = Triple {
        s: VarOrTerm::new_term("<http://example2.com/a>".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("test:SuperType".to_string()),
        g: None,
    };

    store.compute_sprite(&backward_head);
    store.materialize().unwrap();
    assert!(store.triple_index.contains(&validation_triple));
    assert_eq!(7, store.len());
}
//todo move to benchmark
#[test]
fn test_sprite_compute_hierarchy() {
    let timer_load = ::std::time::Instant::now();

    let size = 10;
    let mut data = String::new();
    for i in 0..size {
        data += &format!("<http://example2.com/a{}> a test:SubClass0.\n", i);
        data += &format!(
            "{{?s a test:SubClass{}.}}=>{{?s a test:SubClass{}.}}\n",
            i,
            (i + 1)
        );
    }
    let mut store = CSprite::from(data.as_str());

    let backward_head = Triple {
        s: VarOrTerm::new_var("?newVar".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term(format!("test:SubClass{}", size)),
        g: None,
    };

    let load_time = timer_load.elapsed();
    println!("Load Time: {:.2?}", load_time);
    assert_eq!(size, store.len());
    let timer_load = ::std::time::Instant::now();
    store.compute_sprite(&backward_head);
    let csprite_time = timer_load.elapsed();
    println!("CSprite Time: {:.2?}", csprite_time);
    let timer_load = ::std::time::Instant::now();
    store.materialize().unwrap();
    assert_eq!(2 * size, store.len());
    let load_time = timer_load.elapsed();
    println!("Materialization Time: {:.2?}", load_time);
}

#[test]
fn test_rewrite_hierarchy_csprite() {
    let data = "<http://example2.com/a> a test:SubClass.\n\
            {?s a test:SubClassH1.}=>{?s a test:SubClassH.}\n\
            {?s a test:SubClassH2.}=>{?s a test:SubClassH1.}\n\
            {?s a test:SubClassH3.}=>{?s a test:SubClassH2.}";
    let (_content, rules) = Parser::parse(data.to_string());
    println!("{:?}", rules);

    let rc_rules: Vec<_> = rules.into_iter().map(Arc::new).collect();
    let original_len = rc_rules.len();
    let rewritten_rules = CSprite::rewrite_hierarchy(&rc_rules);
    // The 3-rule SubClass chain must survive rewriting as a non-empty rule
    // set no larger than the input (hierarchy rewriting merges, never invents).
    assert!(
        !rewritten_rules.is_empty(),
        "hierarchy rewrite dropped all rules"
    );
    assert!(
        rewritten_rules.len() <= original_len,
        "hierarchy rewrite grew the rule set: {} -> {}",
        original_len,
        rewritten_rules.len()
    );
}

#[test]
fn test_csprite_cycles_terminate() {
    // TICKET-003: Csprite Cycle Guards. Exercises both helpers directly on a
    // cyclic class hierarchy (ClassA->ClassB->ClassC->ClassA) with NO
    // wall-clock timing involved: a real cycle-guard regression means these
    // calls simply never return, and that is caught by the pre-existing
    // outer `timeout 30s`/`600s cargo test` wrapper in `just test`/
    // `test-lib` (justfile), not by a per-test race against the clock. A
    // prior version of this test used a background thread + recv_timeout as
    // its own liveness proxy; that made the test's pass/fail depend on
    // ambient system load rather than on CSprite's actual behavior (it
    // false-failed under heavy parallel `cargo test --workspace`
    // contention even though the helpers always terminated correctly), and
    // widening the timeout only made a genuine hang more expensive without
    // fixing the flakiness. Calling directly removes both problems: no
    // timing dependency, no thread/channel overhead, deterministic result.
    let data = "{?s a <http://example/ClassA>.}=>{?s a <http://example/ClassB>.}\n\
{?s a <http://example/ClassB>.}=>{?s a <http://example/ClassC>.}\n\
{?s a <http://example/ClassC>.}=>{?s a <http://example/ClassA>.}";

    let query = Triple {
        s: VarOrTerm::new_var("?s".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("<http://example/ClassA>".to_string()),
        g: None,
    };

    let store = CSprite::from(data);
    let (matched_rules, hierarchies) = store.eval_backward_csprite(&query);
    assert!(
        !matched_rules.is_empty(),
        "recursive helper found no matching rules for a cyclic hierarchy"
    );
    let _ = hierarchies;

    let (matched_rules, hierarchies) = store.eval_backward_csprite_helper_with_stack(&query);
    assert!(
        !matched_rules.is_empty(),
        "stack-based helper found no matching rules for a cyclic hierarchy"
    );
    let _ = hierarchies;
}

#[test]
fn test_reconstruct_triples_from_bindings_skips_negated_literal_with_unbound_variable() {
    // swarm audit wnl2yhbgm finding #3: SimpleQueryEngine::query only ever binds
    // variables from a rule's *positive* body literals (queryengine/mod.rs:95-97
    // separates non_negated_lits/negated_lits and only the former populates
    // Binding). A variable that appears solely inside a negated literal is
    // therefore never a key in the returned Binding. This exercises the real
    // `reconstruct_triples_from_bindings` directly with exactly that shape: `?x`
    // is bound (it also appears in the positive literal), `?reason` is not.
    let x_var = VarOrTerm::new_var("?x".to_string()).as_var().name;
    let mut bindings = Binding::new();
    bindings.add(
        &x_var,
        VarOrTerm::new_term("test:foo".to_string()).to_encoded(),
    );

    let rule = Rule {
        head: Triple {
            s: VarOrTerm::new_var("?x".to_string()),
            p: VarOrTerm::new_term("test:included".to_string()),
            o: VarOrTerm::new_term("test:yes".to_string()),
            g: None,
        },
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple {
                    s: VarOrTerm::new_var("?x".to_string()),
                    p: VarOrTerm::new_term("a".to_string()),
                    o: VarOrTerm::new_term("test:Thing".to_string()),
                    g: None,
                },
            },
            BodyLiteral {
                negated: true,
                pattern: Triple {
                    s: VarOrTerm::new_var("?x".to_string()),
                    p: VarOrTerm::new_term("test:excluded".to_string()),
                    o: VarOrTerm::new_var("?reason".to_string()),
                    g: None,
                },
            },
        ],
    };

    // Must not panic.
    let reconstructed = CSpriteReasoner::reconstruct_triples_from_bindings(&mut bindings, &rule);

    assert_eq!(reconstructed.len(), 1);
    // Only the positive literal is reconstructed; the negated literal (whose
    // ?reason variable has no binding) contributes no triple.
    assert_eq!(reconstructed[0].len(), 1);
    assert_eq!(
        reconstructed[0][0],
        Triple {
            s: VarOrTerm::new_term("test:foo".to_string()),
            p: VarOrTerm::new_term("a".to_string()),
            o: VarOrTerm::new_term("test:Thing".to_string()),
            g: None,
        }
    );
}

#[test]
fn test_materialize_window_does_not_panic_on_negated_body_literal_with_unbound_variable() {
    // End-to-end reachability check for the same bug via the real, public,
    // production-wired entry point (CSprite::materialize_window, called from
    // both pipeline.rs and imars_reasoner.rs). A rule with a negated body
    // literal whose variable (?reason) appears nowhere else previously panicked
    // CSpriteReasoner::materialize as soon as the rule's positive literal
    // matched a stored fact.
    let data = "test:foo a test:Thing.\n\
{?x a test:Thing . not {?x test:excluded ?reason}}=>{?x test:included test:yes}";
    let mut store = CSprite::from(data);

    let trigger = Rc::new(Triple {
        s: VarOrTerm::new_term("test:foo".to_string()),
        p: VarOrTerm::new_term("a".to_string()),
        o: VarOrTerm::new_term("test:Thing".to_string()),
        g: None,
    });

    // Must not panic. Current (pinned) behavior: the rule whose negated body
    // literal carries an otherwise-unbound variable is skipped rather than
    // fired, so the window yields no inference — the regression this guards
    // is the panic, and this assertion pins the skip semantics so any future
    // change to fire-with-unbound-negation shows up loudly.
    let inferred = store.materialize_window(vec![(0, trigger)]);
    assert!(
        inferred.is_empty(),
        "unbound-negated-variable rule is expected to be skipped (no inference), got: {inferred:?}"
    );
}

// #[test]
// fn test_transitive(){
//     let rules ="{?a in ?b.?b in ?c}=>{?a in ?c}";
//     let data =":1 in :0.\n\
//         :2 in :1.\n\
//         :3 in :2.\n\
//         :4 in :3.\n\
//         :5 in :4.\n\
//         :6 in :5";
//     let csprite = CSprite::from_with_window(rules, 4, 2);
//     let (mut content, mut rules) = Parser::parse(data.to_string(), &mut csprite.borrow_mut().encoder);
//
//
//
//
//
//     content.into_iter().enumerate().for_each(|(i, t)| csprite.borrow_mut().window.add(t, i as i32));
//
//     //contains 4 triples and 1 inferred triple
//     assert_eq!(19, csprite.borrow_mut().window.len());
// }
