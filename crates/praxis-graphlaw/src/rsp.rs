// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::too_many_arguments, deprecated)]

use crate::rsp::r2r::R2ROperator;
use crate::rsp::r2s::{Relation2StreamOperator, StreamOperator};
use crate::rsp::s2r::{
    CSPARQLWindow, ContentContainer, Report, ReportStrategy, Tick, WindowTriple,
};
use crate::sparql::{evaluate_plan_and_debug, Binding};
use crate::{Encoder, Syntax, Triple, TripleStore};
use log::{debug, error}; // Use log crate when building application
use spargebra::Query;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::mpsc::Receiver;
use std::sync::{Arc, Mutex};
use std::thread;

pub mod r2r;
pub mod r2s;
pub mod s2r;

pub enum OperationMode {
    SingleThread,
    MultiThread,
}
pub struct RSPBuilder<'a, I, O> {
    width: usize,
    slide: usize,
    tick: Option<Tick>,
    report_strategy: Option<ReportStrategy>,
    triples: Option<&'a str>,
    syntax: Option<Syntax>,
    rules: Option<&'a str>,
    query_str: Option<&'a str>,
    result_consumer: Option<ResultConsumer<O>>,
    r2s: Option<StreamOperator>,
    r2r: Option<Box<dyn R2ROperator<I, O>>>,
    operation_mode: OperationMode,
}
impl<'a, I, O> RSPBuilder<'a, I, O>
where
    // `Ord` added alongside the swarm-finding-#22-class fix in `r2s.rs`'s
    // `Relation2StreamOperator::eval` (DSTREAM branch) -- see that function's own doc comment.
    O: Clone + Hash + Eq + Ord + Send + Debug + 'static,
    I: Eq + PartialEq + Clone + Debug + Hash + Send + 'static,
{
    pub fn new(width: usize, slide: usize) -> RSPBuilder<'a, I, O> {
        RSPBuilder {
            width,
            slide,
            tick: None,
            report_strategy: None,
            triples: None,
            syntax: None,
            rules: None,
            query_str: None,
            result_consumer: None,
            r2s: None,
            r2r: None,
            operation_mode: OperationMode::MultiThread,
        }
    }
    pub fn add_tick(mut self, tick: Tick) -> RSPBuilder<'a, I, O> {
        self.tick = Some(tick);
        self
    }
    pub fn add_report_strategy(mut self, strategy: ReportStrategy) -> RSPBuilder<'a, I, O> {
        self.report_strategy = Some(strategy);
        self
    }
    pub fn add_triples(mut self, triples: &'a str) -> RSPBuilder<'a, I, O> {
        self.triples = Some(triples);
        self
    }
    pub fn add_rules(mut self, rules: &'a str) -> RSPBuilder<'a, I, O> {
        self.rules = Some(rules);
        self
    }
    pub fn add_query(mut self, query: &'a str) -> RSPBuilder<'a, I, O> {
        self.query_str = Some(query);
        self
    }
    pub fn add_consumer(mut self, consumer: ResultConsumer<O>) -> RSPBuilder<'a, I, O> {
        self.result_consumer = Some(consumer);
        self
    }
    pub fn add_r2s(mut self, r2s: StreamOperator) -> RSPBuilder<'a, I, O> {
        self.r2s = Some(r2s);
        self
    }
    pub fn add_r2r(mut self, r2r: Box<dyn R2ROperator<I, O>>) -> RSPBuilder<'a, I, O> {
        self.r2r = Some(r2r);
        self
    }
    pub fn add_syntax(mut self, syntax: Syntax) -> RSPBuilder<'a, I, O> {
        self.syntax = Some(syntax);
        self
    }
    pub fn set_operation_mode(mut self, operation_mode: OperationMode) -> RSPBuilder<'a, I, O> {
        self.operation_mode = operation_mode;
        self
    }
    /// Builds the engine, or refuses with a typed error when a required
    /// field (`query`, `r2r`) was never supplied via `add_query`/`add_r2r`.
    ///
    /// Reachability note (checked, not assumed): `RSPBuilder` is constructed
    /// nowhere in this workspace outside `rsp_test.rs`'s own test functions
    /// (`grep -rn "RSPBuilder::new" --include="*.rs" crates/ | grep -v
    /// _test.rs` returns only the struct/impl definitions themselves) --
    /// this is test-only code today, not a live production path. The two
    /// required fields previously panicked via `.expect(...)` rather than
    /// surfacing a typed error to a caller that omits them (swarm finding
    /// `panics-unwraps-core` #5); fixed here as precautionary hardening of
    /// this `pub fn`'s contract, matching the same-file convention already
    /// used by `load_rules` (`Result<(), &'static str>`).
    pub fn build(self) -> Result<RSPEngine<I, O>, &'static str> {
        let query_str = self.query_str.ok_or("Please provide R2R query")?;
        let r2r = self.r2r.ok_or("Please provide R2R operator!")?;
        Ok(RSPEngine::new(
            self.width,
            self.slide,
            self.tick.unwrap_or_default(),
            self.report_strategy.unwrap_or_default(),
            self.triples.unwrap_or(""),
            self.syntax.unwrap_or_default(),
            self.rules.unwrap_or(""),
            query_str,
            self.result_consumer.unwrap_or(ResultConsumer {
                function: Arc::new(Box::new(|r| println!("Bindings: {:?}", r))),
            }),
            self.r2s.unwrap_or_default(),
            r2r,
            self.operation_mode,
        ))
    }
}
pub struct RSPEngine<I, O>
where
    I: Eq + PartialEq + Clone + Debug + Hash + Send,
{
    s2r: CSPARQLWindow<I>,
    r2r: Arc<Mutex<Box<dyn R2ROperator<I, O>>>>,
    r2s_consumer: ResultConsumer<O>,
    r2s_operator: Arc<Mutex<Relation2StreamOperator<O>>>,
}
pub struct ResultConsumer<I> {
    pub function: Arc<dyn Fn(I) + Send + Sync>,
}

impl<I, O> RSPEngine<I, O>
where
    // `Ord` added alongside the swarm-finding-#22-class fix in `r2s.rs`'s
    // `Relation2StreamOperator::eval` (DSTREAM branch) -- see that function's own doc comment.
    O: Clone + Hash + Eq + Ord + Send + 'static,
    I: Eq + PartialEq + Clone + Debug + Hash + Send + 'static,
{
    pub fn new(
        width: usize, slide: usize, tick: Tick, report_strategy: ReportStrategy, triples: &str,
        syntax: Syntax, rules: &str, query_str: &str, result_consumer: ResultConsumer<O>,
        r2s: StreamOperator, r2r: Box<dyn R2ROperator<I, O>>, operation_mode: OperationMode,
    ) -> RSPEngine<I, O> {
        let mut report = Report::new();
        report.add(report_strategy);
        let window = CSPARQLWindow::new(width, slide, report, tick);
        let mut store = r2r;

        if let Err(parsing_error) = store.load_triples(triples, syntax) {
            error!("Unable to load ABox: {:?}", parsing_error.to_string())
        }
        if let Err(rule_error) = store.load_rules(rules) {
            error!("Unable to load rules: {:?}", rule_error.to_string())
        }
        let query = match Query::parse(query_str, None) {
            Ok(parsed_query) => parsed_query,
            Err(err) => {
                error!("Unable to parse query! {:?}", err.to_string());
                error!("Using Select * WHERE{{?s ?p ?o}} instead");
                Query::parse("Select * WHERE{?s ?p ?o}", None).unwrap()
            }
        };
        let mut engine = RSPEngine {
            s2r: window,
            r2r: Arc::new(Mutex::new(store)),
            r2s_consumer: result_consumer,
            r2s_operator: Arc::new(Mutex::new(Relation2StreamOperator::new(r2s, 0))),
        };
        match operation_mode {
            OperationMode::SingleThread => {
                let consumer_temp = engine.r2r.clone();
                let r2s_consumer = engine.r2s_consumer.function.clone();
                let r2s_operator = engine.r2s_operator.clone();
                let call_back: Box<dyn FnMut(ContentContainer<I>)> = Box::new(move |content| {
                    Self::evaluate_r2r_and_call_r2s(
                        &query,
                        consumer_temp.clone(),
                        r2s_consumer.clone(),
                        r2s_operator.clone(),
                        content,
                    );
                });
                engine.s2r.register_callback(call_back);
            }
            OperationMode::MultiThread => {
                let consumer = engine.s2r.register();
                engine.register_r2r(consumer, query);
            }
        }

        engine
    }
    fn register_r2r(&mut self, receiver: Receiver<ContentContainer<I>>, query: Query) {
        let consumer_temp = self.r2r.clone();
        let r2s_consumer = self.r2s_consumer.function.clone();
        let r2s_operator = self.r2s_operator.clone();
        thread::spawn(move || {
            loop {
                match receiver.recv() {
                    Ok(content) => {
                        Self::evaluate_r2r_and_call_r2s(
                            &query,
                            consumer_temp.clone(),
                            r2s_consumer.clone(),
                            r2s_operator.clone(),
                            content,
                        );
                    }
                    Err(_) => {
                        debug!("Shutting down!");
                        break;
                    }
                }
            }
            debug!("Shutdown complete!");
        });
    }

    // Typed-refusal note (swarm finding `panics-unwraps-core`, encoding.rs:236 -- `consumer_temp`/
    // `r2s_operator` shared the same `.lock().unwrap()` anti-pattern `Encoder`'s `recover()` fix
    // addressed, but a fresh adversarial audit of that fix (this session) found blind
    // poison-recovery is the WRONG fix here, for a reason more specific than "TripleStore is
    // complex": `TripleStore::materialize` (lib.rs) clones a checkpoint and restores it ONLY on
    // its `Result::Err` arm -- a genuine panic unwinds past that `match` entirely, bypassing the
    // checkpoint rollback that exists specifically to undo a bad materialization. So a panic
    // inside `Reasoner::materialize`'s multi-stratum fixpoint loop can leave `triple_index` frozen
    // mid-fixpoint with NO rollback ever having run, and blindly recovering the lock afterward
    // would hand the next tick that exact frozen-mid-fixpoint store as if it were valid --
    // silently wrong, not just delayed, unlike `GLOBAL_ENCODER`'s provably-always-consistent
    // interning. The honest fix instead mirrors `SimpleR2R::execute_query`'s own convention two
    // lines below (added 022aa0e9, same day as the original `.lock().unwrap()` bug): on a
    // poisoned lock, log and skip this tick entirely rather than either panicking forever or
    // silently continuing on a store a prior panic may have left inconsistent. Reachability note
    // (checked, not assumed): `RSPEngine`/`RSPBuilder` are constructed nowhere in this workspace
    // outside `rsp_test.rs`'s own test functions -- this is test-only code today, not a live
    // production path, so this fix is precautionary hardening, not an active-incident repair.
    fn evaluate_r2r_and_call_r2s(
        query: &Query, consumer_temp: Arc<Mutex<Box<dyn R2ROperator<I, O>>>>,
        r2s_consumer: Arc<dyn Fn(O) + Send + Sync>,
        r2s_operator: Arc<Mutex<Relation2StreamOperator<O>>>, content: ContentContainer<I>,
    ) {
        debug!("R2R operator retrieved graph {:?}", content);
        let time_stamp = content.get_last_timestamp_changed();
        let mut store = match consumer_temp.lock() {
            Ok(guard) => guard,
            Err(_) => {
                error!(
                    "R2R: consumer_temp lock is poisoned (a prior tick panicked while holding \
                     it, possibly bypassing TripleStore::materialize's own checkpoint rollback); \
                     skipping this tick rather than risking a silently stale/mid-fixpoint store"
                );
                return;
            }
        };
        content.clone().into_iter().for_each(|t| {
            store.add(t);
        });
        let inferred = store.materialize();
        let r2r_result = store.execute_query(query);
        let mut r2s_guard = match r2s_operator.lock() {
            Ok(guard) => guard,
            Err(_) => {
                error!(
                    "R2R: r2s_operator lock is poisoned (a prior tick panicked while holding \
                     it); skipping this tick's R2S evaluation rather than risking stale RSTREAM/ \
                     ISTREAM/DSTREAM old_result/new_result state"
                );
                return;
            }
        };
        let r2s_result = r2s_guard.eval(r2r_result, time_stamp);
        drop(r2s_guard);
        // R2S runs synchronously in the same thread as R2R; async dispatch is left for future work
        for result in r2s_result {
            (r2s_consumer)(result);
        }
        //remove data from stream
        content.iter().for_each(|t| {
            store.remove(t);
        });
        inferred.iter().for_each(|t| {
            store.remove(t);
        });
    }

    pub fn add(&mut self, event_item: I, ts: usize) {
        self.s2r.add_to_window(event_item, ts);
    }
    pub fn stop(&mut self) {
        self.s2r.stop();
    }
}

pub struct SimpleR2R {
    pub item: TripleStore,
}
impl R2ROperator<WindowTriple, Vec<Binding>> for SimpleR2R {
    fn load_triples(&mut self, data: &str, syntax: Syntax) -> Result<(), String> {
        let reseult = self.item.load_triples(data, syntax);
        println!(
            "Store size after loading: {:?}",
            self.item.triple_index.len()
        );
        reseult
    }

    fn load_rules(&mut self, data: &str) -> Result<(), &'static str> {
        self.item
            .load_rules(data)
            .map_err(|_| "Failed to load rules")
    }

    fn add(&mut self, data: WindowTriple) {
        println!("Store size: {:?}", self.item.triple_index.len());

        let encoded_triple = Triple::from(data.s, data.p, data.o);
        self.item.add(encoded_triple);
    }

    fn remove(&mut self, data: &WindowTriple) {
        let encoded_triple = Triple::from(data.s.clone(), data.p.clone(), data.o.clone());

        self.item.remove_ref(&encoded_triple);
    }

    fn materialize(&mut self) -> Vec<WindowTriple> {
        println!("Store size: {:?}", self.item.triple_index.len());
        let inferred = self.item.materialize().unwrap_or_default();
        // Each `t` here is a triple `self.item.materialize()` itself just derived and inserted,
        // so `t.s`/`t.p`/`t.o` are symbol ids the crate's own (global, append-only, never-purging)
        // Encoder assigned moments ago -- `decode` returning `None` for one of them should be
        // unreachable under this crate's own interning invariants. Trait signature
        // (`R2ROperator::materialize`, part of the vendored API, see this file's top-of-file
        // scope note) is infallible, so a decode miss can't be surfaced as a typed error; the
        // honest response is to skip and log that one triple rather than either panicking
        // (finding #4's original bug: this ran inside a `Mutex::lock()` critical section, so a
        // panic here poisoned the lock for every future call) or silently defaulting the missing
        // component to an empty string (which would fabricate a wrong-but-valid-looking
        // WindowTriple instead of honestly omitting the malformed one) -- matching the "log and
        // emit no rows for this tick" convention `execute_query` already uses immediately below.
        inferred
            .into_iter()
            .filter_map(|t| {
                let s = Encoder::decode(&t.s.to_encoded());
                let p = Encoder::decode(&t.p.to_encoded());
                let o = Encoder::decode(&t.o.to_encoded());
                match (s, p, o) {
                    (Some(s), Some(p), Some(o)) => Some(WindowTriple { s, p, o }),
                    _ => {
                        error!(
                            "R2R materialize: skipping a just-inferred triple whose \
                             subject/predicate/object failed to decode (encoder invariant \
                             violated -- an id materialize() just produced was not found in the \
                             global Encoder)"
                        );
                        None
                    }
                }
            })
            .collect()
    }

    fn execute_query(&self, query: &Query) -> Vec<Vec<Binding>> {
        // Trait signature is part of the vendored `R2ROperator` API (see the
        // vendored-scope note atop this file) and stays infallible. On a
        // planning refusal (e.g. GROUP_CONCAT/SAMPLE aggregate), log and
        // emit no rows for this tick -- the same convention `RSPEngine::new`
        // already uses two call sites up for `load_triples`/`load_rules`
        // failures in a long-running stream engine that must not crash a
        // window callback over one malformed query.
        match crate::plan_query_or_refuse(query, &self.item.triple_index) {
            Ok(plan) => evaluate_plan_and_debug(&plan, &self.item.triple_index).collect(),
            Err(refusal) => {
                error!("SPARQL query planning refused: {refusal}");
                Vec::new()
            }
        }
    }
}

#[cfg(test)]
#[path = "rsp_test.rs"]
mod rsp_test;
