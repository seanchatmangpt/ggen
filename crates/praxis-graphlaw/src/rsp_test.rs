#![cfg(test)]

use std::fs::{File, OpenOptions};
use std::io;
use std::io::{BufRead, Write};

use super::*;
use std::time::Duration;

#[test]
#[ignore]
fn rsp_integration() {
    let ntriples_file = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/bar> <http://schema.org/name> \"Bar\" .";
    let rules = "@prefix test: <http://www.w3.org/test/>.\n{?x <http://test.be/hasVal> ?y. ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.}=>{?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> test:SuperType.}";
    let function = Box::new(|r| println!("Bindings: {:?}", r));
    let result_consumer = ResultConsumer {
        function: Arc::new(function),
    };
    let r2r = Box::new(SimpleR2R {
        item: TripleStore::new(),
    });
    let mut engine = RSPBuilder::new(10, 2)
        .add_tick(Tick::TimeDriven)
        .add_report_strategy(ReportStrategy::OnWindowClose)
        .add_triples(ntriples_file)
        .add_syntax(Syntax::NTriples)
        .add_rules(rules)
        .add_query("Select * WHERE{ ?s a <http://www.w3.org/test/SuperType>}")
        .add_consumer(result_consumer)
        .add_r2r(r2r)
        .add_r2s(StreamOperator::RSTREAM)
        .build()
        .unwrap();
    for i in 0..10 {
        let triple = WindowTriple {
            s: format!("s{}", i),
            p: "<http://test.be/hasVal>".to_string(),
            o: "<http://example.com/foo>".to_string(),
        };

        engine.add(triple, i);
    }
    engine.stop();
    thread::sleep(Duration::from_secs(2));
}
#[test]
#[ignore]
fn test_load_from_file() {
    let ntriples_file = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/bar> <http://schema.org/name> \"Bar\" .";
    let rules = "@prefix test: <http://www.w3.org/test/>.\n{?x <http://test.be/hasVal> ?y. ?y <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person>.}=>{?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> test:SuperType.}";

    //write to file
    let function = Box::new(|r| {
        let mut output = OpenOptions::new()
            .append(true)
            .open("/tmp/rox_rsp.out")
            .unwrap();
        writeln!(output, "Bindings: {:?}", r).unwrap();
    });
    let result_consumer = ResultConsumer {
        function: Arc::new(function),
    };
    let r2r = Box::new(SimpleR2R {
        item: TripleStore::new(),
    });
    let mut engine = RSPBuilder::new(10, 2)
        .add_tick(Tick::TimeDriven)
        .add_report_strategy(ReportStrategy::OnWindowClose)
        .add_triples(ntriples_file)
        .add_syntax(Syntax::NTriples)
        .add_rules(rules)
        .add_query("Select * WHERE{ ?s <http://test/hasLocation> ?loc}")
        .add_consumer(result_consumer)
        .add_r2r(r2r)
        .add_r2s(StreamOperator::RSTREAM)
        .build()
        .unwrap();

    //read from file:
    let file =
        File::open("/Users/psbonte/Documents/Github/RoXi/examples/rsp/location_update_stream.nt")
            .unwrap();

    for (i, lines) in io::BufReader::new(file).lines().enumerate() {
        let lines = lines.unwrap();
        let mut line = lines.split(" ");
        let triple = WindowTriple {
            s: line.next().unwrap().to_string(),
            p: line.next().unwrap().to_string(),
            o: line.next().unwrap().to_string(),
        };

        engine.add(triple, i);
    }
    engine.stop();
    thread::sleep(Duration::from_secs(2));
}
#[test]
#[ignore]
fn rsp_transitive_testp() {
    let ntriples_file = "";
    let rules = "@prefix test: <http://test/>.
 @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
 {?x test:isIn ?y. ?y test:isIn ?z. }=>{?x test:isIn ?z.}";
    let function = Box::new(|r| println!("Bindings: {:?}", r));
    let result_consumer = ResultConsumer {
        function: Arc::new(function),
    };
    let r2r = Box::new(SimpleR2R {
        item: TripleStore::new(),
    });
    let mut engine = RSPBuilder::new(10, 2)
        .add_tick(Tick::TimeDriven)
        .add_report_strategy(ReportStrategy::OnWindowClose)
        .add_triples(ntriples_file)
        .add_syntax(Syntax::NTriples)
        .add_rules(rules)
        .add_query("Select * WHERE{ ?x <http://test/isIn> ?y}")
        .add_consumer(result_consumer)
        .add_r2r(r2r)
        .add_r2s(StreamOperator::RSTREAM)
        .set_operation_mode(OperationMode::SingleThread)
        .build()
        .unwrap();
    for i in 0..20 {
        let triple = WindowTriple {
            s: format!("<http://test/{}>", i),
            p: "<http://test/isIn>".to_string(),
            o: format!("<http://test/{}>", i + 1),
        };

        engine.add(triple, i);
    }
    engine.stop();
}

#[test]
#[ignore]
fn test_static_abox() {
    let ntriples_file = "<http://test/sensor1> <http://test/hasLocation> <http://test/location1>.";
    let rules = "@prefix test: <http://test/>.
 @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.
 {?x test:madeBy ?y. ?y test:hasLocation ?z. }=>{?x test:madeIn ?z.}";
    let function = Box::new(|r| println!("Bindings: {:?}", r));
    let result_consumer = ResultConsumer {
        function: Arc::new(function),
    };
    let r2r = Box::new(SimpleR2R {
        item: TripleStore::new(),
    });
    let mut engine = RSPBuilder::new(10, 2)
        .add_tick(Tick::TimeDriven)
        .add_report_strategy(ReportStrategy::OnWindowClose)
        .add_triples(ntriples_file)
        .add_syntax(Syntax::NTriples)
        .add_rules(rules)
        .add_query("Select * WHERE{ ?x <http://test/madeIn> ?y}")
        .add_consumer(result_consumer)
        .add_r2r(r2r)
        .add_r2s(StreamOperator::RSTREAM)
        .set_operation_mode(OperationMode::SingleThread)
        .build()
        .unwrap();
    for i in 0..20 {
        let triple = WindowTriple {
            s: format!("<http://test/{}>", i),
            p: "<http://test/madeBy>".to_string(),
            o: "<http://test/sensor1>".to_string(),
        };

        engine.add(triple, i);
    }
    engine.stop();
}

// Regression for the `consumer_temp`/`r2s_operator` typed-refusal fix in
// `evaluate_r2r_and_call_r2s` (that function's own doc comment has the full mechanism/nuance
// disclosure -- an adversarial audit found blind poison-recovery, encoding.rs's pattern, was the
// WRONG fix here, since `TripleStore::materialize`'s checkpoint rollback only runs on `Err`, not
// on a panic unwind, so a recovered-but-possibly-mid-fixpoint store could be silently wrong, not
// merely delayed). Calls `evaluate_r2r_and_call_r2s` DIRECTLY (not a hand-rolled stand-in for its
// lock pattern -- `ContentContainer::new`/`add` were widened to `pub(crate)` specifically so this
// test can build a real `ContentContainer` and exercise the actual fixed function), with a
// pre-poisoned `consumer_temp`, and asserts two things: the call returns normally (no panic --
// the poisoned-lock branch was reached and skipped this tick), and the `r2s_consumer` callback
// was never invoked (proving the tick was genuinely skipped end-to-end, not silently continued on
// a possibly-bad store).
#[test]
// Same complex-generic-type shape `rsp.rs`'s own file-level `#![allow(clippy::too_many_arguments,
// deprecated)]` already tolerates for this API; the concrete type annotation is needed here
// (unlike rsp.rs's own generic `I`/`O`-parameterized fields) to construct a real trait object.
#[allow(clippy::type_complexity)]
fn evaluate_r2r_and_call_r2s_skips_the_tick_on_a_poisoned_consumer_lock() {
    let consumer_temp: Arc<Mutex<Box<dyn R2ROperator<WindowTriple, Vec<Binding>>>>> =
        Arc::new(Mutex::new(Box::new(SimpleR2R {
            item: TripleStore::new(),
        })));

    let poison_handle = {
        let consumer_temp = consumer_temp.clone();
        thread::spawn(move || {
            let _guard = consumer_temp.lock().unwrap();
            panic!("intentional test poisoning of consumer_temp");
        })
    };
    assert!(
        poison_handle.join().is_err(),
        "the spawned thread must have actually panicked, or this test proves nothing"
    );

    let r2s_called = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let r2s_consumer: Arc<dyn Fn(Vec<Binding>) + Send + Sync> = {
        let r2s_called = r2s_called.clone();
        Arc::new(move |_: Vec<Binding>| {
            r2s_called.store(true, std::sync::atomic::Ordering::SeqCst);
        })
    };
    let r2s_operator = Arc::new(Mutex::new(Relation2StreamOperator::new(
        StreamOperator::RSTREAM,
        0,
    )));
    let query = Query::parse("Select * WHERE{?s ?p ?o}", None).expect("static query must parse");
    let mut content: ContentContainer<WindowTriple> = ContentContainer::new();
    content.add(
        WindowTriple {
            s: "<http://example.com/foo>".to_string(),
            p: "<http://test.be/hasVal>".to_string(),
            o: "<http://example.com/bar>".to_string(),
        },
        0,
    );

    // Must not panic: the poisoned `consumer_temp` lock is reached and this tick is skipped.
    RSPEngine::<WindowTriple, Vec<Binding>>::evaluate_r2r_and_call_r2s(
        &query,
        consumer_temp,
        r2s_consumer,
        r2s_operator,
        content,
    );

    assert!(
        !r2s_called.load(std::sync::atomic::Ordering::SeqCst),
        "r2s_consumer must never be invoked when the tick was skipped due to a poisoned lock"
    );
}

#[test]
#[allow(clippy::type_complexity)]
fn evaluate_r2r_and_call_r2s_skips_the_tick_on_a_poisoned_r2s_operator_lock() {
    // Mirrors the test above but poisons the *second* lock instead of the first:
    // `consumer_temp` stays healthy (the R2R stage -- add/materialize/execute_query --
    // genuinely runs), and only `r2s_operator` is poisoned. This exercises rsp.rs's
    // second typed-refusal match arm (evaluate_r2r_and_call_r2s's `r2s_operator.lock()`
    // match), which the first test above never reaches because it returns early on the
    // first lock. Without this test, that second match arm had zero coverage.
    let consumer_temp: Arc<Mutex<Box<dyn R2ROperator<WindowTriple, Vec<Binding>>>>> =
        Arc::new(Mutex::new(Box::new(SimpleR2R {
            item: TripleStore::new(),
        })));

    let r2s_operator = Arc::new(Mutex::new(Relation2StreamOperator::new(
        StreamOperator::RSTREAM,
        0,
    )));

    let poison_handle = {
        let r2s_operator = r2s_operator.clone();
        thread::spawn(move || {
            let _guard = r2s_operator.lock().unwrap();
            panic!("intentional test poisoning of r2s_operator");
        })
    };
    assert!(
        poison_handle.join().is_err(),
        "the spawned thread must have actually panicked, or this test proves nothing"
    );

    let r2s_called = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let r2s_consumer: Arc<dyn Fn(Vec<Binding>) + Send + Sync> = {
        let r2s_called = r2s_called.clone();
        Arc::new(move |_: Vec<Binding>| {
            r2s_called.store(true, std::sync::atomic::Ordering::SeqCst);
        })
    };
    let query = Query::parse("Select * WHERE{?s ?p ?o}", None).expect("static query must parse");
    let mut content: ContentContainer<WindowTriple> = ContentContainer::new();
    content.add(
        WindowTriple {
            s: "<http://example.com/foo>".to_string(),
            p: "<http://test.be/hasVal>".to_string(),
            o: "<http://example.com/bar>".to_string(),
        },
        0,
    );

    // Must not panic: consumer_temp's stage runs to completion, then the poisoned
    // r2s_operator lock is reached and the tick's R2S evaluation is skipped.
    RSPEngine::<WindowTriple, Vec<Binding>>::evaluate_r2r_and_call_r2s(
        &query,
        consumer_temp,
        r2s_consumer,
        r2s_operator,
        content,
    );

    assert!(
        !r2s_called.load(std::sync::atomic::Ordering::SeqCst),
        "r2s_consumer must never be invoked when the R2S stage was skipped due to a poisoned \
         r2s_operator lock"
    );
}

// Regression for swarm finding `panics-unwraps-core` #5: `RSPBuilder::build()` used to
// `.expect(...)` its two required fields (`query`, `r2r`), crashing the process instead of
// surfacing a typed error when a caller omits one. Calls the REAL `RSPBuilder::build()` (not a
// hand-rolled reimplementation of the missing-field check) in both omission orders so each of the
// two `ok_or(...)` arms gets independent coverage.
#[test]
fn build_refuses_with_typed_error_when_query_is_missing() {
    let r2r = Box::new(SimpleR2R {
        item: TripleStore::new(),
    });
    let result = RSPBuilder::<WindowTriple, Vec<Binding>>::new(10, 2)
        .add_r2r(r2r)
        .build();
    assert_eq!(result.err(), Some("Please provide R2R query"));
}

#[test]
fn build_refuses_with_typed_error_when_r2r_is_missing() {
    let result = RSPBuilder::<WindowTriple, Vec<Binding>>::new(10, 2)
        .add_query("Select * WHERE{?s ?p ?o}")
        .build();
    assert_eq!(result.err(), Some("Please provide R2R operator!"));
}
