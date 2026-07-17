#![cfg(test)]

use crate::reasoner::Reasoner;
use crate::{
    BodyLiteral, QueryEngine, Rule, RuleIndex, SimpleQueryEngine, Triple, TripleIndex, TripleStore,
    VarOrTerm,
};
use std::collections::HashMap;

// Regression for a real, swarm-identified crash: `sparql/plan.rs`'s
// `build_for_aggregate` returns `Err("Failed")` for GROUP_CONCAT/SAMPLE (its
// catch-all for aggregates it doesn't implement), and the caller in
// `extract_query_plan`'s `Group` arm unwraps that `Result`, panicking. Before
// `plan_query_or_refuse`, this crashed the calling thread outright for any
// caller of `TripleStore::query`. It must now surface as a typed `Err`.
#[test]
fn group_concat_aggregate_refuses_instead_of_panicking() {
    let data = "<http://example.org/a> <http://example.org/p> \"1\".\n\
                <http://example.org/a> <http://example.org/p> \"2\".";
    let store = TripleStore::from(data);
    let result = store.query(
        "SELECT (GROUP_CONCAT(?o) AS ?g) WHERE { ?s <http://example.org/p> ?o } GROUP BY ?s",
    );
    match result {
        Err(msg) => assert!(
            msg.contains("planning refused"),
            "expected a query-planning refusal message, got: {msg}"
        ),
        Ok(rows) => panic!("expected GROUP_CONCAT to be refused, got rows: {rows:?}"),
    }
}

// Regression for a real, swarm-identified crash: `sparql/mod.rs`'s `PlanNode::Aggregate`
// execution arm called `Encoder::get(&var_str).unwrap()` on a GROUP BY key variable that
// was never interned (never appears in the WHERE clause or SELECT projection), panicking
// on the first row evaluated -- a separate execution-phase panic from the
// group_concat_aggregate test above (planning-phase), so `plan_query_or_refuse`'s
// catch_unwind does not cover it: `evaluate_plan` runs after planning already returned a
// `PlanNode`, outside that boundary. Fixed by the same `unwrap_or_else(|| Encoder::add(...))`
// safe-fallback pattern already used one block above (mod.rs:168) for the aggregate target
// variable. Per SPARQL's own unbound-group-key semantics, grouping by a variable with no
// binding groups every row together under that missing key -- not a crash -- so this
// asserts real (non-panicking), correct output, not just that no crash occurs.
#[test]
fn group_by_unbound_variable_groups_instead_of_panicking() {
    let data = "<http://example.org/a> <http://example.org/p> \"1\".\n\
                <http://example.org/a> <http://example.org/p> \"2\".\n\
                <http://example.org/b> <http://example.org/p> \"3\".";
    let store = TripleStore::from(data);
    let result = store.query(
        "SELECT (COUNT(*) AS ?c) WHERE { ?s <http://example.org/p> ?o } GROUP BY ?neverBound",
    );
    match result {
        Ok(rows) => {
            assert_eq!(
                rows.len(),
                1,
                "grouping by an unbound variable must fold every row into one group, got: {rows:?}"
            );
            let count_binding = rows[0]
                .iter()
                .find(|b| b.var == "c")
                .expect("aggregate binding ?c must be present");
            // Expected value updated after fixing CountAccumulator::get() to intern its result
            // as a properly-typed xsd:integer literal (Encoder::add_literal) instead of letting
            // the generic Encoder::add() mis-classify the bare "3" as an IRI -- see the fix's own
            // commit message for the full root cause (HAVING/ORDER BY on an aggregate silently
            // failed since a mis-typed IRI can't be numerically compared). The bare-string
            // assertion this test originally had was itself asserting the bug's symptom.
            assert_eq!(
                count_binding.val, "\"3\"^^<http://www.w3.org/2001/XMLSchema#integer>",
                "COUNT(*) over all 3 triples must be 3, got: {rows:?}"
            );
        }
        Err(msg) => panic!("expected a real grouped result, got a refusal: {msg}"),
    }
}

// Regression for a real, dogfood-workflow-identified symptom: `SELECT ?s (COUNT(*) AS ?c)
// ... GROUP BY ?s HAVING (COUNT(*) > 1)` silently returned zero rows for data where a real
// group has count > 1. The originating audit's own root-cause claim (a `GraphPattern::Having`
// enum variant falling through `extract_query_plan`'s catch-all) was checked directly against
// spargebra 0.4.6's source and found to be wrong -- no such variant exists; `HAVING` desugars
// at parse time into `Filter(Group(...))`, both already-matched arms. The real, four-layer bug
// chain: (1) `CountAccumulator::get()` handed its result to the generic `Encoder::add()`
// classifier, which mis-tagged the bare numeric string as an IRI (any lexical form with no `?`/
// `_:`/`"` prefix falls into the IRI branch); (2) the aggregate operand-variable lookup in
// `sparql/mod.rs` used `Encoder::get(&var_str).unwrap_or(0)`, silently substituting a wrong-but-
// valid-looking symbol ID 0 instead of interning the variable; (3) `Utils::remove_literal_tags`
// only stripped surrounding quotes when a `^^datatype` suffix was present, so an untagged
// decoded literal like `"3"` passed through the FILTER's numeric-comparison path still quote-
// wrapped, failing `.parse::<f64>()` silently. Fixed (1) via `Encoder::add_literal` with an
// explicit `xsd:integer`/`xsd:decimal` datatype tag, (2) via the same safe
// `unwrap_or_else(|| Encoder::add(var_str))` fallback used elsewhere in this file, (3) by
// unconditionally stripping quotes from the pre-`^^`-split lexical form. A fourth, deeper gap
// remains and is intentionally NOT fixed here: `EncodedTerm` (sparql/eval.rs) has no decimal/
// float variant, so `xsd:decimal`-tagged SUM/MIN/MAX/AVG results still can't be numerically
// compared once routed through `PlanExpression::Variable`'s evaluator -- see that file's and
// `sparql/accumulators.rs`'s cross-referencing disclosure comments. COUNT is unaffected (its
// `xsd:integer` tag IS handled by that evaluator), so this HAVING/COUNT case is now genuinely
// fixed end-to-end; HAVING/ORDER BY on SUM/MIN/MAX/AVG remains future work.
#[test]
fn having_count_filters_correctly() {
    let data = "<http://example.org/a> <http://example.org/p> \"1\".\n\
                <http://example.org/a> <http://example.org/p> \"2\".\n\
                <http://example.org/a> <http://example.org/p> \"3\".\n\
                <http://example.org/b> <http://example.org/p> \"4\".";
    let store = TripleStore::from(data);
    let result = store
        .query(
            "SELECT ?s (COUNT(*) AS ?c) WHERE { ?s <http://example.org/p> ?o } GROUP BY ?s HAVING (COUNT(*) > 1)",
        )
        .expect("query must succeed");
    assert_eq!(result.len(), 1, "only ?s=a has count > 1, got: {result:?}");
}

#[test]
fn having_count_excludes_all_when_threshold_too_high() {
    let data = "<http://example.org/a> <http://example.org/p> \"1\".\n\
                <http://example.org/a> <http://example.org/p> \"2\".\n\
                <http://example.org/a> <http://example.org/p> \"3\".\n\
                <http://example.org/b> <http://example.org/p> \"4\".";
    let store = TripleStore::from(data);
    let result = store
        .query(
            "SELECT ?s (COUNT(*) AS ?c) WHERE { ?s <http://example.org/p> ?o } GROUP BY ?s HAVING (COUNT(*) > 10)",
        )
        .expect("query must succeed");
    assert_eq!(result.len(), 0, "no group has count > 10, got: {result:?}");
}

// SUM's own accumulated VALUE is now correct (the `remove_literal_tags` quote-stripping fix
// plus the accumulator's `Encoder::add_literal` type-tagging fix both apply here too). The
// displayed value is untagged ("6", not "\"6\"^^<...decimal>") because SPARQL requires an
// aggregate to be aliased ("AS ?total"), which routes the result through an Extend/rename step
// -- and that step's expression evaluator has no decimal/float `EncodedTerm` variant either, so
// it strips the datatype tag on the way out (the same disclosed, deferred gap noted above). This
// test asserts what the current architecture actually, honestly guarantees: the numeric VALUE is
// correct, not that HAVING/ORDER BY on it works yet.
#[test]
fn sum_over_untagged_literals_computes_the_real_value() {
    let data = "<http://example.org/a> <http://example.org/p> \"1\".\n\
                <http://example.org/a> <http://example.org/p> \"2\".\n\
                <http://example.org/a> <http://example.org/p> \"3\".\n\
                <http://example.org/b> <http://example.org/p> \"4\".";
    let store = TripleStore::from(data);
    let result = store
        .query("SELECT ?s (SUM(?o) AS ?total) WHERE { ?s <http://example.org/p> ?o } GROUP BY ?s")
        .expect("query must succeed");
    let a_row = result
        .iter()
        .find(|row| row.iter().any(|b| b.var == "s" && b.val.contains("/a")))
        .expect("row for ?s=a must exist");
    let total = a_row
        .iter()
        .find(|b| b.var == "total")
        .expect("total bound");
    assert_eq!(total.val, "\"6\"", "1+2+3=6, got: {result:?}");
}

// Found by an adversarial dogfood sweep for the bug class fixed in commits 89ba964c/f08b4e41:
// `sparql::mod`'s `PlanNode::Aggregate` branch of `evaluate_plan` groups rows into a
// `std::collections::HashMap<Vec<usize>, Vec<AccumulatorImpl>>` (the same `RandomState`-hashed,
// per-process-reseeded map type as finding #22 -- not even the narrower `FxHashMap` case of
// #23/#24), then iterated it directly, so every SPARQL query using `GROUP BY` -- reachable via
// `TripleStore::query`'s public SELECT/CONSTRUCT API -- returned its result rows in raw HashMap
// order, differing across separate process runs of byte-identical input/query. Fixed by
// collecting the map's entries and sorting by group key (`Vec<usize>`, `Ord` lexically -- an
// unambiguous total order since distinct groups always have distinct key vectors).
//
// Written directly with the two-independently-built-stores pattern from the start (not the
// repeated-calls-on-one-store pattern the SHACL/ShEx sibling tests above were found, by an
// independent adversarial audit, to be toothless for -- see
// `test_shacl_validate_order_is_independent_of_triple_insertion_order`'s own doc comment for the
// full account of why that pattern proves nothing).
#[test]
fn test_group_by_row_order_is_independent_of_triple_insertion_order() {
    let subjects = ["a", "b", "c", "d", "e", "f", "g", "h"];
    let triple = |s: &str| {
        format!("<http://example.org/det-groupby-{s}> <http://example.org/det-groupby-p> \"1\".")
    };
    let forward: String = subjects
        .iter()
        .map(|s| triple(s))
        .collect::<Vec<_>>()
        .join("\n");
    let reversed: String = subjects
        .iter()
        .rev()
        .map(|s| triple(s))
        .collect::<Vec<_>>()
        .join("\n");

    let store_forward = TripleStore::from(forward.as_str());
    let store_reversed = TripleStore::from(reversed.as_str());

    let query =
        "SELECT ?s (COUNT(*) AS ?c) WHERE { ?s <http://example.org/det-groupby-p> ?o } GROUP BY ?s";
    let result_forward = store_forward.query(query).expect("query must succeed");
    let result_reversed = store_reversed.query(query).expect("query must succeed");

    assert_eq!(
        result_forward.len(),
        8,
        "all 8 distinct subjects must form their own group, got: {result_forward:?}"
    );
    assert_eq!(
        result_forward, result_reversed,
        "the same logical facts, inserted in forward vs. reversed order into two separate \
         TripleStores, must produce GROUP BY rows in the identical order"
    );
}

#[test]
fn test_parse() {
    let data = ":a a :C0.\n\
            {?a a :C0}=>{?a a :C1}\n\
            {?a a :C1}=>{?a a :C2}\n\
            {?a a :C2}=>{?a a :C3}";

    let mut store = TripleStore::from(data);

    let mat = store.materialize().unwrap();
    println!("Length: {:?}", store.len());
    println!("Length Mat: {:?}", mat.len());
}

#[test]
fn test_store() {
    let timer = ::std::time::Instant::now();
    let mut rules = Vec::new();
    let max_depth = 5;
    for i in 0..max_depth {
        let rule = Rule {
            head: Triple {
                s: VarOrTerm::new_var("s".to_string()),
                p: VarOrTerm::new_term("http://test".to_string()),
                o: VarOrTerm::new_term(format!("U{}", i + 1)),
                g: None,
            },
            body: Vec::from([BodyLiteral {
                negated: false,
                pattern: Triple {
                    s: VarOrTerm::new_var("s".to_string()),
                    p: VarOrTerm::new_term("http://test".to_string()),
                    o: VarOrTerm::new_term(format!("U{}", i)),
                    g: None,
                },
            }]),
        };
        rules.push(rule);
    }

    let content = Vec::from([Triple {
        s: VarOrTerm::new_term("sTerm".to_string()),
        p: VarOrTerm::new_term("http://test".to_string()),
        o: VarOrTerm::new_term("U0".to_string()),
        g: None,
    }]);
    let mut rules_index = RuleIndex::new();
    for rule in rules.iter() {
        rules_index.add_ref(rule);
    }
    let mut triple_index = TripleIndex::new();
    content.into_iter().for_each(|t| triple_index.add(t));
    let query = Triple {
        s: VarOrTerm::new_var("s".to_string()),
        p: VarOrTerm::new_term("http://test".to_string()),
        o: VarOrTerm::new_term(format!("U{}", max_depth)),
        g: None,
    };

    let mut store = TripleStore {
        rules: Vec::new(),
        rules_index,
        triple_index,
        reasoner: Reasoner {},
        aggregates: HashMap::new(),
        strata: Vec::new(),
        hooks: Vec::new(),
        receipts: Vec::new(),
        additions: Vec::new(),
        removals: Vec::new(),
        verdicts: Vec::new(),
    };

    store.materialize().unwrap();
    let elapsed = timer.elapsed();

    let result = SimpleQueryEngine::query(
        &store.triple_index,
        &Vec::from([BodyLiteral {
            negated: false,
            pattern: query,
        }]),
        None,
    );

    println!("Processed in: {:.2?}", elapsed);
    println!("Result: {:?}", result);
}

#[test]
fn test_incomplete_rule_match() {
    let data = ":a in :b.\n\
            {?a in ?b. ?b in ?c}=>{?a in ?c.}";

    let mut store = TripleStore::from(data);
    assert_eq!(1, store.len());
    store.materialize().unwrap();
    assert_eq!(1, store.len());
}
#[test]
fn test_no_var_query() {
    let data = ":a in :b.\n\
            {:a in :b}=>{:a in :c}";

    let mut store = TripleStore::from(data);
    assert_eq!(1, store.len());
    store.materialize().unwrap();
    assert_eq!(2, store.len());
}
#[test]
fn test_single_rule() {
    let data = ":a a :A.\n\
            {?a a :A}=>{?a a :B}";

    let mut store = TripleStore::from(data);
    assert_eq!(1, store.len());
    store.materialize().unwrap();
    assert_eq!(2, store.len());
}
#[test]
fn test_multiple_rule() {
    let data = ":a a :A.\n\
            {?a a :A}=>{?a a :B}\n\
            {?a a :B}=>{?a a :C}";

    let mut store = TripleStore::from(data);
    assert_eq!(1, store.len());
    store.materialize().unwrap();
    assert_eq!(3, store.len());
}
#[test]
fn test_join_rule() {
    let data = ":a a :A.\n\
            :a in :b.\n\
            {?a a :A.?a in ?o}=>{?a a :B}";

    let mut store = TripleStore::from(data);
    assert_eq!(2, store.len());
    store.materialize().unwrap();
    assert_eq!(3, store.len());
}
#[test]
fn test_long_join_rule() {
    let data = ":a a :A.\n\
            :a in :b.\n\
            :b in :c.\n\
            :c a :A.\n\
            {?a a :A.?a in ?o.?o in ?o2.?o2 a :A}=>{?a a :B}";

    let mut store = TripleStore::from(data);
    assert_eq!(4, store.len());
    store.materialize().unwrap();
    assert_eq!(5, store.len());
}
#[test]
fn test_transitive_rule() {
    let mut data = "{?a in ?b.?b in ?c}=>{?a in ?c}\n".to_owned();
    for i in 0..10 {
        data += format!(":{} in :{}.\n", i + 1, i).as_str();
    }
    let mut store = TripleStore::from(data.as_str());
    assert_eq!(10, store.len());
    store.materialize().unwrap();
    assert_eq!(55, store.len());
}
#[test]
fn test_hierarchy() {
    let max_depth = 10;
    let mut data = ":a a :U0\n".to_owned();
    for i in 0..max_depth {
        data += format!("{{?a a :U{}}}=>{{?a a :U{}}}\n", i, i + 1).as_str();
        data += format!("{{?a a :U{}}}=>{{?a a :J{}}}\n", i, i + 1).as_str();
        data += format!("{{?a a :U{}}}=>{{?a a :Q{}}}\n", i, i + 1).as_str();
    }
    println!("{}", data);
    let mut store = TripleStore::from(data.as_str());
    let inferred = store.materialize().unwrap();
    println!("Triples: {:?}", store.len());
    assert_eq!(3 * max_depth, inferred.len());
}
#[test]
fn test_rdf_hierarchy() {
    let max_depth = 10;
    let mut data = ":a a :U0\n\
                        {?a :subClassOf ?b.?b :subClassOf ?c}=>{?a :subClassOf ?c}\n"
        .to_owned();
    for i in 0..max_depth {
        data += format!(":U{} :subClassOf :U{}.\n", i, i + 1).as_str();
        data += format!(":U{} :subClassOf :J{}.\n", i, i + 1).as_str();
        data += format!(":U{} :subClassOf :Q{}.\n", i, i + 1).as_str();
    }
    let mut store = TripleStore::from(data.as_str());
    let inferred = store.materialize().unwrap();
    println!("Inferred: {:?}, Total: {:?}", inferred.len(), store.len());
    // The transitive closure over 3 interleaved subClassOf chains produces 135 inferred triples.
    assert_eq!(135, inferred.len());
}
// #[test]
// fn test_eval_backward_multiple_rules(){
//     let mut store = ReasoningStore::new();
//     store.parse_and_add_rule("@prefix test: <http://www.test.be/test#>.\n @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n \
//     {?s rdf:type test:SubClass.}=>{?s rdf:type test:SuperType.}\n\
//     {?s rdf:type test:SubClass2.}=>{?s rdf:type test:SuperType.}");
//     store.load_abox( b"<http://example2.com/a> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.test.be/test#SubClass> .".as_ref());
//     store.load_abox( b"<http://example2.com/c> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.test.be/test#SubClass2> .".as_ref());
//
//     // diff variable names
//     let backward_head = ReasonerTriple::new("?newVar".to_string(),"http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),"http://www.test.be/test#SuperType".to_string());
//     let  bindings = store.eval_backward( &backward_head);
//     let mut result_bindings: Binding = Binding::new();
//     result_bindings.add("newVar", Term::from(NamedNode::new("http://example2.com/a".to_string()).unwrap()));
//     result_bindings.add("newVar", Term::from(NamedNode::new("http://example2.com/c".to_string()).unwrap()));
//
//     assert_eq!(result_bindings, bindings);
// }
// #[test]
// fn test_eval_backward_nested_rules(){
//     let mut store = ReasoningStore::new();
//     store.parse_and_add_rule("@prefix test: <http://www.test.be/test#>.\n @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n \
//     {?s rdf:type test:SubClass. ?s test:hasRef ?o. ?o rdf:type test:SubClass2.}=>{?s rdf:type test:SuperType.}\n\
//     {?q rdf:type test:SubClassTemp.}=>{?q rdf:type test:SubClass2.}");
//     store.load_abox( b"<http://example2.com/a> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.test.be/test#SubClass> .".as_ref());
//     store.load_abox( b"<http://example2.com/b> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.test.be/test#SubClassTemp> .".as_ref());
//     store.load_abox( b"<http://example2.com/a> <http://www.test.be/test#hasRef> <http://example2.com/b> .".as_ref());
//
//     // diff variable names
//     let backward_head = ReasonerTriple::new("?newVar".to_string(),"http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),"http://www.test.be/test#SuperType".to_string());
//     let  bindings = store.eval_backward( &backward_head);
//     let mut result_bindings: Binding = Binding::new();
//     result_bindings.add("newVar", Term::from(NamedNode::new("http://example2.com/a".to_string()).unwrap()));
//
//     assert_eq!(result_bindings, bindings);
// }

// -----------------------------------------------------------------------
// SHACL integration tests
// -----------------------------------------------------------------------

#[test]
fn test_shacl_min_count_violation() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person> .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .";
    let report = store.validate_shacl(shapes).unwrap();
    assert!(!report.conforms, "Expected a sh:minCount violation");
    assert!(!report.results.is_empty());
}

#[test]
fn test_shacl_conforms() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person> .\n<http://example/bob> <http://example/name> \"Bob\" .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ; sh:maxCount 1 ] .";
    let report = store.validate_shacl(shapes).unwrap();
    assert!(report.conforms, "Expected conformance");
}

// Regression for a real, swarm-identified determinism bug: `shacl::report::Validator::validate`
// (`report.rs`) iterated its `shape_nodes`/`focus_nodes` sets -- both `std::collections::
// HashSet<usize>`, whose default `RandomState` hasher is reseeded from OS entropy once per
// process -- directly, so byte-identical input produced differently-ordered `results` across
// separate process runs (the swarm's own scenario: the same `cng` workday benchmark run twice as
// two OS processes).
//
// SELF-CORRECTION (disclosed, not silently fixed): this test originally called
// `store.validate_shacl(shapes)` repeatedly on the SAME already-built `TripleStore` and asserted
// the results matched every time. An independent adversarial audit (of the sibling `#23`/`#24`
// tests below, which shared this exact structure) proved that pattern provides ZERO regression
// protection: it built the actual pre-fix buggy source in an isolated worktree, ran the
// then-existing tests against it, and both passed anyway -- because `validate_shacl` borrows the
// store's `TripleIndex` (`&self`) rather than rebuilding it, so its `HashSet`'s bucket layout is
// already fixed in memory before the first call and cannot change between repeated calls on the
// same instance, regardless of whether the code sorts before iterating. The audit's own follow-up
// experiment (compiling a standalone program against this workspace's pinned `rustc-hash`
// version) confirmed the actual mechanism instead: a hash-based collection's iteration order can
// depend on insertion sequence (resize/rehash history), not just final key-set content -- so two
// *independently constructed* collections with the same final content can iterate differently.
// Fixed here by testing THAT property directly: two separately-built `TripleStore`s over the same
// logical facts, inserted in reverse order, must still validate to the identical result order.
#[test]
fn test_shacl_validate_order_is_independent_of_triple_insertion_order() {
    // 8 target instances (not 2): more entries raises the chance that two differently-ordered
    // `TripleIndex`es actually land in different `HashSet`/`FxHashMap` bucket layouts, matching
    // the audit's own reproduction (which needed >= 5 keys to reliably observe a difference).
    let names = [
        "alice", "bob", "carol", "dave", "erin", "frank", "grace", "heidi",
    ];
    let triple = |n: &str| {
        format!(
            "<http://example/det-shacl-{n}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/det-shacl-Person> ."
        )
    };
    let forward: String = names
        .iter()
        .map(|n| triple(n))
        .collect::<Vec<_>>()
        .join("\n");
    let reversed: String = names
        .iter()
        .rev()
        .map(|n| triple(n))
        .collect::<Vec<_>>()
        .join("\n");

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:DetShaclPersonShape a sh:NodeShape ;
    sh:targetClass ex:det-shacl-Person ;
    sh:property [ sh:path ex:det-shacl-name ; sh:minCount 1 ] .";

    let mut store_forward = TripleStore::new();
    store_forward
        .load_triples(&forward, crate::parser::Syntax::NTriples)
        .unwrap();
    let mut store_reversed = TripleStore::new();
    store_reversed
        .load_triples(&reversed, crate::parser::Syntax::NTriples)
        .unwrap();

    let report_forward = store_forward.validate_shacl(shapes).unwrap();
    let report_reversed = store_reversed.validate_shacl(shapes).unwrap();

    assert!(
        !report_forward.conforms,
        "all 8 instances lack ex:det-shacl-name; expected minCount violations"
    );
    assert_eq!(
        report_forward.results.len(),
        8,
        "all 8 people must be reported, got: {:?}",
        report_forward.results
    );
    assert_eq!(
        report_forward.results, report_reversed.results,
        "the same logical facts, inserted in forward vs. reversed order into two separate \
         TripleStores, must validate to the identical result order"
    );
}

// Regression for swarm finding #23 (same bug class as #22 above, different file): `shacl::
// validate::validate_shape_closed_and_targets_tail`'s `sh:closed` check iterated
// `data.spo.get(&focus_node)`'s `FxHashMap<usize, ...>` keys directly, so with >= 2 forbidden
// predicates present on one focus node, `results` push order depended on that map's bucket
// layout rather than a canonical order. Fixed the same way as #22: collect and
// `sort_unstable()` the predicate IDs before iterating.
//
// Uses the two-independently-built-stores comparison established above (not repeated calls on
// one store -- see that test's own doc comment for why the repeated-call pattern was proven
// toothless by an independent adversarial audit). 8 extra predicates on one focus node, inserted
// in forward vs. reversed order across two separate stores.
#[test]
fn test_shacl_closed_shape_violation_order_is_deterministic() {
    let extra_preds: Vec<String> = (0..24).map(|i| format!("extra-{i}")).collect();
    let build = |order: &[String]| {
        let mut lines = vec![
            "<http://example/det-closed-alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/det-closed-Person> .".to_string(),
        ];
        for p in order {
            lines.push(format!(
                "<http://example/det-closed-alice> <http://example/det-closed-{p}> \"x\" ."
            ));
        }
        lines.join("\n")
    };
    let forward = build(&extra_preds);
    // A scrambled (not just reversed) permutation: reversal alone can preserve enough relative
    // hash-bucket structure to accidentally re-converge on the same iteration order for a given
    // FxHash instance -- empirically confirmed against this exact fixture before settling on
    // this permutation (see the commit message for the adversarial self-check this test was
    // built under). Interleaving by stride scrambles insertion order more thoroughly than a
    // straight reversal.
    let scrambled: Vec<String> = (0..24)
        .step_by(2)
        .chain((1..24).step_by(2))
        .map(|i| extra_preds[23 - i].clone())
        .collect();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:DetClosedPersonShape a sh:NodeShape ;
    sh:targetClass ex:det-closed-Person ;
    sh:closed true ;
    sh:ignoredProperties (rdf:type) .";

    let mut store_forward = TripleStore::new();
    store_forward
        .load_triples(&forward, crate::parser::Syntax::NTriples)
        .unwrap();
    let mut store_scrambled = TripleStore::new();
    store_scrambled
        .load_triples(&build(&scrambled), crate::parser::Syntax::NTriples)
        .unwrap();

    let report_forward = store_forward.validate_shacl(shapes).unwrap();
    let report_scrambled = store_scrambled.validate_shacl(shapes).unwrap();

    assert!(
        !report_forward.conforms,
        "all 24 extra predicates are outside the closed shape"
    );
    assert_eq!(
        report_forward.results.len(),
        24,
        "all 24 extra predicates must be reported, got: {:?}",
        report_forward.results
    );
    assert_eq!(
        report_forward.results, report_scrambled.results,
        "the same logical extra predicates, inserted in forward vs. scrambled order into two \
         separate TripleStores, must produce sh:closed violations in the identical order"
    );
}

// Regression for swarm finding #24 (same bug class as #22/#23, ShEx's CLOSED shape check
// instead of SHACL's sh:closed): `shex_native::validate_node`'s CLOSED-shape branch iterated
// `data.spo.get(&focus)`'s `FxHashMap<usize, ...>` keys directly, joining one "CLOSED shape:
// predicate ..." message per extra predicate into a single `ShexValidationFailure.reason`
// string via `errors.join("; ")` -- so with >= 2 extra predicates, the joined message order
// depended on FxHashMap bucket layout. Fixed the same way as #22/#23: sort the predicate IDs
// before iterating. Same two-independently-built-stores pattern as the sibling tests above.
#[test]
fn test_shex_closed_shape_violation_order_is_deterministic() {
    // 24 predicates, scrambled (not just reversed) insertion order -- see the sibling
    // `test_shacl_closed_shape_violation_order_is_deterministic`'s own comment: a straight
    // reversal of a small (8-element) set was empirically confirmed, via an adversarial
    // self-check against the actual pre-fix source, to NOT reliably trigger `FxHashMap`'s
    // insertion-order-dependent bucket layout for this crate's symbol-ID value range. This
    // larger, more scrambled fixture was confirmed (same self-check method) to actually fail
    // against the pre-fix code before being trusted as a real regression test.
    let extra_preds: Vec<String> = (0..24).map(|i| format!("extra-{i}")).collect();
    let build = |order: &[String]| {
        order
            .iter()
            .map(|p| {
                format!(
                    "<http://example.org/det-shex-alice> <http://example.org/det-shex-{p}> \"x\" ."
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    };
    let forward = build(&extra_preds);
    let scrambled: Vec<String> = (0..24)
        .step_by(2)
        .chain((1..24).step_by(2))
        .map(|i| extra_preds[23 - i].clone())
        .collect();

    let schema_json = r#"{
          "@context": "http://www.w3.org/ns/shexj.jsonld",
          "type": "Schema",
          "shapes": [
            {
              "type": "ShapeDecl",
              "id": "http://example.org/DetShexClosedShape",
              "shapeExpr": {
                "type": "Shape",
                "closed": true,
                "expression": null
              }
            }
          ]
        }"#;
    let shape_map = vec![(
        "http://example.org/det-shex-alice".to_string(),
        "http://example.org/DetShexClosedShape".to_string(),
    )];

    let mut store_forward = TripleStore::new();
    store_forward
        .load_triples(&forward, crate::parser::Syntax::NTriples)
        .unwrap();
    let mut store_scrambled = TripleStore::new();
    store_scrambled
        .load_triples(&build(&scrambled), crate::parser::Syntax::NTriples)
        .unwrap();

    let report_forward = store_forward
        .validate_shex(schema_json, &shape_map)
        .unwrap();
    let report_scrambled = store_scrambled
        .validate_shex(schema_json, &shape_map)
        .unwrap();

    assert!(
        !report_forward.conforms,
        "all 24 extra predicates violate the closed shape with no expression/EXTRA"
    );
    assert_eq!(
        report_forward.failures.len(),
        1,
        "one failure for the one (node, shape) pair"
    );
    assert_eq!(
        report_forward.failures[0].reason, report_scrambled.failures[0].reason,
        "the same logical extra predicates, inserted in forward vs. scrambled order into two \
         separate TripleStores, must produce the identical joined CLOSED-shape reason string"
    );
}

#[test]
fn test_shacl_report_to_triples() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/e> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/P> .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:PShape a sh:NodeShape ;
    sh:targetClass ex:P ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] .";
    let report = store.validate_shacl(shapes).unwrap();
    assert!(!report.conforms);
    let triples = report.to_triples();
    assert!(
        !triples.is_empty(),
        "Report should serialise to RDF triples"
    );
}

#[test]
fn test_shacl_node_constraint_violation() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person> .\n<http://example/alice> <http://example/address> <http://example/addr1> .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:AddressShape a sh:NodeShape ;
    sh:property [ sh:path ex:city ; sh:minCount 1 ] .
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [ sh:path ex:address ; sh:node ex:AddressShape ] .";
    let report = store.validate_shacl(shapes).unwrap();
    assert!(
        !report.conforms,
        "Expected a sh:node violation because ex:addr1 has no ex:city"
    );
    assert!(report.results.iter().any(|r| r.source_constraint_component
        == crate::triples::Term::Iri(crate::triples::TermImpl {
            iri: crate::encoding::Encoder::get(
                "<http://www.w3.org/ns/shacl#NodeConstraintComponent>"
            )
            .unwrap()
        })));
}

#[test]
fn test_shacl_node_constraint_conforms() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person> .\n<http://example/bob> <http://example/address> <http://example/addr2> .\n<http://example/addr2> <http://example/city> \"Springfield\" .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let shapes = "@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example/> .
ex:AddressShape a sh:NodeShape ;
    sh:property [ sh:path ex:city ; sh:minCount 1 ] .
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [ sh:path ex:address ; sh:node ex:AddressShape ] .";
    let report = store.validate_shacl(shapes).unwrap();
    assert!(
        report.conforms,
        "Expected conformance since ex:addr2 has an ex:city"
    );
}

// -----------------------------------------------------------------------
// ShEx integration tests
// -----------------------------------------------------------------------

#[test]
fn test_triplestore_validate_shex() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example.org/Alice> <http://example.org/age> \"30\"^^<http://www.w3.org/2001/XMLSchema#integer> .\n<http://example.org/Bob> <http://example.org/age> \"thirty\" .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();

    let schema_json = r#"{
          "@context": "http://www.w3.org/ns/shex.jsonld",
          "type": "Schema",
          "shapes": [
            {
              "type": "ShapeDecl",
              "id": "http://example.org/AgeShape",
              "shapeExpr": {
                "type": "Shape",
                "expression": {
                  "type": "TripleConstraint",
                  "predicate": "http://example.org/age",
                  "valueExpr": {
                    "type": "NodeConstraint",
                    "datatype": "http://www.w3.org/2001/XMLSchema#integer"
                  }
                }
              }
            }
          ]
        }"#;

    let shape_map = vec![
        (
            "http://example.org/Alice".to_string(),
            "http://example.org/AgeShape".to_string(),
        ),
        (
            "http://example.org/Bob".to_string(),
            "http://example.org/AgeShape".to_string(),
        ),
    ];

    let report = store.validate_shex(schema_json, &shape_map).unwrap();
    assert!(
        !report.conforms,
        "Bob's non-integer age should fail the AgeShape"
    );
    assert_eq!(report.failures.len(), 1);
    assert_eq!(report.failures[0].shape, "http://example.org/AgeShape");
}

// -----------------------------------------------------------------------
// Datalog stratification tests
// -----------------------------------------------------------------------

#[test]
fn test_datalog_safety_check_rejects_unsafe_rule() {
    use crate::datalog::validate_rules;
    let rules = vec![crate::Rule {
        body: vec![crate::BodyLiteral {
            negated: false,
            pattern: crate::Triple {
                s: crate::VarOrTerm::new_var("?s".to_string()),
                p: crate::VarOrTerm::new_term("<http://example/p>".to_string()),
                o: crate::VarOrTerm::new_var("?o".to_string()),
                g: None,
            },
        }],
        head: crate::Triple {
            s: crate::VarOrTerm::new_var("?s".to_string()),
            p: crate::VarOrTerm::new_term("<http://example/q>".to_string()),
            o: crate::VarOrTerm::new_var("?unbound".to_string()),
            g: None,
        },
    }];
    let result = validate_rules(&rules, &std::collections::HashMap::new());
    assert!(
        result.is_err(),
        "Unsafe rule (unbound head variable) should be rejected"
    );
}

#[test]
fn test_datalog_safe_rule_stratification() {
    use crate::datalog::validate_rules;
    let rules = vec![crate::Rule {
        body: vec![crate::BodyLiteral {
            negated: false,
            pattern: crate::Triple {
                s: crate::VarOrTerm::new_var("?s".to_string()),
                p: crate::VarOrTerm::new_term("<http://example/p>".to_string()),
                o: crate::VarOrTerm::new_var("?o".to_string()),
                g: None,
            },
        }],
        head: crate::Triple {
            s: crate::VarOrTerm::new_var("?s".to_string()),
            p: crate::VarOrTerm::new_term("<http://example/q>".to_string()),
            o: crate::VarOrTerm::new_var("?o".to_string()),
            g: None,
        },
    }];
    let result = validate_rules(&rules, &std::collections::HashMap::new());
    assert!(result.is_ok(), "Safe positive rule should stratify");
    assert_eq!(result.unwrap(), vec![0]);
}

// Regression for a real, reproduced panic found by an adversarial dogfood audit this session:
// `Reasoner::materialize`'s `HookCondition::Window` round-evaluation arm (reasoner/mod.rs)
// computed `usize::from(window) - 1` unconditionally -- for `window == 0` this underflows and
// panics under this workspace's default overflow-checked build profile. `kh:window 0` is now
// refused at hook-PARSE time (hooks/parsing.rs, see `test_b3_window_size_bounds` in
// tests/knowledge_hooks_e2e_tier2.rs for that half), so the normal `load_hook_pack` API can no
// longer construct this scenario -- this test instead pushes a `CompiledHook` with `condition:
// HookCondition::Window { window: 0, .. }` directly onto `TripleStore.hooks` (a public field),
// bypassing the parser entirely, to prove the SEPARATE defense-in-depth fix in reasoner/mod.rs
// itself (raw `- 1` changed to `.saturating_sub(1)`, matching the same pattern already used by
// the sibling `hooks/condition.rs::evaluate_condition` implementation) actually holds for any
// caller that reaches this code path without going through hook parsing at all.
#[test]
fn materialize_does_not_panic_on_a_directly_constructed_zero_window_hook() {
    use crate::hooks::{CmpOp, CompiledHook, EffectKind, EventId, HookCondition, HookId};

    let mut store = TripleStore::new();
    store
        .load_triples(
            "<http://example.org/window-zero-subject> <http://example.org/metric> \"1\".",
            crate::parser::Syntax::NTriples,
        )
        .unwrap();
    store.hooks.push(CompiledHook {
        id: HookId(0),
        iri: "urn:test:window-zero-direct".to_string(),
        name: "window_zero_direct".to_string(),
        event: EventId(0),
        on: "any".to_string(),
        condition: HookCondition::Window {
            var: "http://example.org/metric".to_string(),
            op: CmpOp::Ge,
            k: 0,
            window: 0,
        },
        effect: EffectKind::EmitDelta,
        action: None,
        reason: None,
        priority: 0,
        after: smallvec::SmallVec::new(),
    });

    // Must not panic: this is the exact call sequence the audit reproduced the underflow with.
    let result = store.materialize();
    assert!(
        result.is_ok(),
        "materialize() must not panic (or otherwise fail) on a directly-constructed \
         window=0 hook, got: {result:?}"
    );
}

// Regression for the `TripleStore::materialize` checkpoint-rollback hardening (task tracked
// as "materialize's checkpoint rollback is panic-blind" this session): the pre-existing
// `Err` arm already restored `triple_index` from the pre-call checkpoint on a *typed*
// failure, but a *panic* partway through the same reasoner call used to bypass both match
// arms entirely, leaving `triple_index`/`receipts`/`verdicts`/`removals` in whatever
// partially-mutated state the panic interrupted -- silently corrupting the store for any
// caller further up the stack that catches the panic and keeps using the same `TripleStore`
// (this crate already does exactly that pattern elsewhere -- see `plan_query_or_refuse`'s
// own `catch_unwind`, and `HookCondition::Datalog`'s condition evaluator, which recursively
// calls `materialize()` on a `temp_store` while itself running *inside* an outer
// `materialize()` call). `materialize()` now wraps the reasoner call in
// `std::panic::catch_unwind` and runs the identical checkpoint-restore the `Err` arm always
// did on a caught panic, before converting it to the same `Result<_, String>` convention.
//
// This test proves the *unchanged* half of that contract -- an ordinary typed failure
// (an unparseable Datalog hook condition, constructed the same directly-onto-`store.hooks`
// way the sibling window=0 test above does, bypassing hook-pack parsing) still rolls the
// checkpoint back and returns Err exactly as before the refactor. An extensive search this
// cycle across the reasoner/hooks/datalog-translation call graph materialize() reaches
// (reasoner/mod.rs, hooks/condition.rs, hooks/datalog.rs, hooks/quads.rs,
// reasoner/substitution.rs) found every raw-indexing site already explicitly length-guarded
// (`match len() { .. _ => Err(..) }`, `.get().unwrap_or(..)`, `.saturating_sub(1)`) -- no
// currently-live, easily-reproduced panic trigger was found to exercise the *new* catch_unwind
// arm itself against a real caller this cycle; that arm's coverage is therefore proactive
// hardening, disclosed as such rather than claimed to close a reproduced crash. One deeper,
// not-yet-confirmed-reachable concern was found and disclosed separately (not fixed here):
// `Binding::len()` (`bindings.rs`) returns `self.bindings.values().next()`'s column length --
// HashMap-iteration-order-dependent, so if two variables in the same `Binding` ever end up
// with genuinely different-length columns, several `Vec` index sites (this file's own
// aggregate grouping in `reasoner/mod.rs` and `Binding::join`) would index the shorter column
// out of bounds. Whether that state is reachable through the query engine's own binding
// construction (which normally produces uniform-length columns per solution row) was not
// established this cycle.
#[test]
fn materialize_still_rolls_back_the_checkpoint_and_returns_err_on_an_ordinary_typed_failure() {
    use crate::hooks::{CompiledHook, EffectKind, EventId, HookCondition, HookId};

    let mut store = TripleStore::new();
    store
        .load_triples(
            "<http://example.org/pre-existing> <http://example.org/p> \"already-here\".",
            crate::parser::Syntax::NTriples,
        )
        .unwrap();
    let checkpoint_triples = store.triple_index.triples.clone();

    // A Datalog program whose head has no parenthesized argument list --
    // `translate_datalog_program` (reasoner/mod.rs) splits on ":-" first (so a program with
    // no ":-" at all is silently skipped as a no-op, not an error -- confirmed by an earlier,
    // wrong version of this test); given a head/body split, it returns a typed `Err` when
    // `head_part.split('(').nth(1)` is `None` ("Datalog rule head missing argument"). This
    // is the pre-existing `Err` arm's path, not the new panic-catching arm.
    store.hooks.push(CompiledHook {
        id: HookId(0),
        iri: "urn:test:malformed-datalog-direct".to_string(),
        name: "malformed_datalog_direct".to_string(),
        event: EventId(0),
        on: "any".to_string(),
        condition: HookCondition::Datalog {
            program: "foo :- t(?x,?y,?z)".to_string(),
            goal: "irrelevant".to_string(),
        },
        effect: EffectKind::EmitDelta,
        action: None,
        reason: None,
        priority: 0,
        after: smallvec::SmallVec::new(),
    });

    let result = store.materialize();
    assert!(
        result.is_err(),
        "an unparseable Datalog hook condition must be a typed refusal, not a silent success"
    );
    assert_eq!(
        store.triple_index.triples, checkpoint_triples,
        "checkpoint must be fully restored on the ordinary Err path -- unchanged by the \
         catch_unwind refactor"
    );
}

// Regression for a real bug an adversarial dogfood audit found in the `catch_unwind` hardening
// above: both rollback arms (the pre-existing `Err` arm and the new panic arm) restore
// `triple_index`/`receipts`/`additions`/`removals` but never cleared `self.verdicts` -- so a
// hook that fires and records a `HookVerdictRecord` *before* a later hook in the same
// `materialize()` call fails would leave that stale verdict sitting in `store.verdicts` after
// rollback, contradicting the doc comment's "restores every field" claim. Two hooks, processed
// in push order: Hook A (a `Sparql`-condition hook matching the pre-existing triple, no action)
// fires and pushes a real `Fired` verdict (confirmed by reading `reasoner/mod.rs`: a fired hook
// with no additions still gets `verdict_pushed = true` via the empty-receipt branch). Hook B is
// the same malformed-Datalog hook from the test above, which then fails the whole call.
#[test]
fn materialize_clears_verdicts_on_rollback_not_just_triple_index() {
    use crate::hooks::{CompiledHook, EffectKind, EventId, HookCondition, HookId};

    let mut store = TripleStore::new();
    store
        .load_triples(
            "<http://example.org/pre-existing> <http://example.org/p> \"already-here\".",
            crate::parser::Syntax::NTriples,
        )
        .unwrap();

    store.hooks.push(CompiledHook {
        id: HookId(0),
        iri: "urn:test:fires-and-records-a-verdict".to_string(),
        name: "fires_and_records_a_verdict".to_string(),
        event: EventId(0),
        on: "any".to_string(),
        condition: HookCondition::Sparql {
            query: "SELECT ?s WHERE { ?s <http://example.org/p> \"already-here\" }".to_string(),
        },
        effect: EffectKind::EmitDelta,
        action: None,
        reason: None,
        priority: 0,
        after: smallvec::SmallVec::new(),
    });
    store.hooks.push(CompiledHook {
        id: HookId(1),
        iri: "urn:test:malformed-datalog-direct".to_string(),
        name: "malformed_datalog_direct".to_string(),
        event: EventId(0),
        on: "any".to_string(),
        condition: HookCondition::Datalog {
            program: "foo :- t(?x,?y,?z)".to_string(),
            goal: "irrelevant".to_string(),
        },
        effect: EffectKind::EmitDelta,
        action: None,
        reason: None,
        priority: 1,
        after: smallvec::SmallVec::new(),
    });

    let result = store.materialize();
    assert!(
        result.is_err(),
        "the second hook's unparseable Datalog condition must still refuse the whole call"
    );
    assert!(
        store.verdicts.is_empty(),
        "verdicts must be cleared on rollback along with triple_index/receipts -- found {} \
         stale verdict(s) from the first hook's Fired record surviving the Err rollback",
        store.verdicts.len()
    );
}

// Regression for a real, `docs/jira/v26.7.12/REMAINING_WORK.md`-flagged correctness bug: the
// Bellman-Ford stratification loop's `iteration` counter always executes its loop body at
// least once (`changed` starts `true`), so on an empty ruleset (`num_predicates == 0`) it
// still increments `iteration` to 1 over zero edges, then the post-loop cycle check
// (`iteration > num_predicates`, i.e. `1 > 0`) spuriously reports a stratification cycle for
// input that has no rules -- and therefore cannot have a cycle. `crown_local.rs` already works
// around this by requiring a non-empty rule pack (a harmless-non-firing rule instead of a true
// empty one); this test asserts the direct, unworked-around call is now correct.
#[test]
fn test_datalog_empty_ruleset_is_trivially_stratifiable() {
    use crate::datalog::validate_rules;
    let result = validate_rules(&[], &std::collections::HashMap::new());
    assert_eq!(
        result,
        Ok(Vec::new()),
        "an empty ruleset has no predicates and no cycle to detect; it must not be refused"
    );
}

// Same bug, exercised through its actual production caller: `TripleStore::add_rules` was
// reachable by this false refusal any time a caller extended an already-empty ruleset with
// another empty batch (e.g. `TripleStore::new()` followed by `add_rules(vec![])`) --
// `?`-propagated as a real, user-visible `Err`, unlike `TripleStore::from`'s constructor path,
// which happened to survive the bug only because it silently discards `validate_rules`'s
// result via `if let Ok(...)`.
#[test]
fn test_add_rules_with_empty_ruleset_does_not_refuse() {
    let mut store = TripleStore::new();
    let result = store.add_rules(Vec::new());
    assert!(
        result.is_ok(),
        "extending an empty ruleset with zero new rules must not be refused, got: {result:?}"
    );
}

// N3 / forward chaining integration
#[test]
fn test_n3_rules_forward_chaining() {
    let mut store = TripleStore::new();
    store
            .load_triples(
                "<http://example/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person> .",
                crate::parser::Syntax::NTriples,
            )
            .unwrap();
    store
            .load_rules(
                "{?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Person>.} => {?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example/Agent>.}",
            )
            .unwrap();
    let new_triples = store.materialize().unwrap();
    assert!(
        !new_triples.is_empty(),
        "Expected at least one inferred triple"
    );
}

// -----------------------------------------------------------------------
// Knowledge Hook Pack & Registry / Gating Tests (Milestone M1)
// -----------------------------------------------------------------------

fn create_temp_pack_dir(name: &str, toml: &str, ttl: &str) -> std::path::PathBuf {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir().join(format!("praxis_test_pack_{}_{}", name, id));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("pack.toml"), toml).unwrap();
    std::fs::write(dir.join("ontology.ttl"), ttl).unwrap();
    dir
}

#[test]
fn test_load_hook_pack_valid() {
    let toml = r#"[pack]
name = "valid-pack"
version = "1.0.0"
description = "A valid test pack"
required_dialects = ["delta"]
"#;
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:on "assert" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "refused by test" ;
    kh:priority 3 .
"#;
    let pack_dir = create_temp_pack_dir("valid", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(res.is_ok(), "Failed to load valid hook pack: {:?}", res);
    assert_eq!(store.hooks.len(), 1);
    assert_eq!(store.hooks[0].name, "h1");
    assert_eq!(store.hooks[0].priority, 3);

    // Cleanup
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_load_hook_pack_unsupported_dialect() {
    let toml = r#"[pack]
name = "unsupported-dialect"
version = "1.0.0"
description = "Dialect test"
required_dialects = ["unsupported-dialect"]
"#;
    let ttl = "";
    let pack_dir = create_temp_pack_dir("dialect", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(res.is_err());
    assert!(res.unwrap_err().contains("unsupported dialect"));
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_load_hook_pack_forbidden_keyword() {
    let toml = r#"[pack]
name = "forbidden-keyword"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    // Tries to sneak a command/shell execution via reason string
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:on "assert" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "let us run exec or curl shell" .
"#;
    let pack_dir = create_temp_pack_dir("forbidden", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(res.is_err());
    assert!(res.unwrap_err().contains("forbidden keyword"));
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_load_hook_pack_forbidden_predicate() {
    let toml = r#"[pack]
name = "forbidden-predicate"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:forbiddenField "malicious" .
"#;
    let pack_dir = create_temp_pack_dir("forbidden_pred", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(
        res.is_err(),
        "Should have failed due to forbidden predicate or SHACL violation"
    );
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_load_hook_pack_closed_shape_violation() {
    let toml = r#"[pack]
name = "closed-violation"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    // Uses kh:command which is forbidden by the closed shape validation
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:command "malicious_command" .
"#;
    let pack_dir = create_temp_pack_dir("closed_viol", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(
        res.is_err(),
        "Should have failed SHACL validation due to sh:closed"
    );
    assert!(res.unwrap_err().contains("SHACL validation failed"));
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_load_hook_pack_action_closed_shape_violation() {
    let toml = r#"[pack]
name = "action-violation"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    // Action has kh:shell property which is not allowed
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "ground-action" ;
    kh:action ex:act1 .

ex:act1 a kh:Action ;
    kh:handler ex:hnd1 ;
    kh:shell "malicious_shell" .
"#;
    let pack_dir = create_temp_pack_dir("action_viol", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(
        res.is_err(),
        "Should have failed SHACL validation due to closed Action shape"
    );
    assert!(res.unwrap_err().contains("SHACL validation failed"));
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_hook_pack_topological_sorting() {
    let toml = r#"[pack]
name = "sorting"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:priority 5 ;
    kh:after ex:h2 .

ex:h2 a kh:Hook ;
    kh:name "h2" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:priority 2 .
"#;
    let pack_dir = create_temp_pack_dir("sorting", toml, ttl);
    let mut store = TripleStore::new();
    store.load_hook_pack(&pack_dir).unwrap();
    assert_eq!(store.hooks.len(), 2);
    assert_eq!(store.hooks[0].name, "h2");
    assert_eq!(store.hooks[1].name, "h1");
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_hook_pack_cycle_detection() {
    let toml = r#"[pack]
name = "cycle"
version = "1.0.0"
description = "test"
required_dialects = ["delta"]
"#;
    let ttl = r#"
@prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
@prefix ex: <http://example.org/> .

ex:h1 a kh:Hook ;
    kh:name "h1" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:after ex:h2 .

ex:h2 a kh:Hook ;
    kh:name "h2" ;
    kh:kind "delta" ;
    kh:var "v" ;
    kh:effect "refuse" ;
    kh:reason "r" ;
    kh:after ex:h1 .
"#;
    let pack_dir = create_temp_pack_dir("cycle", toml, ttl);
    let mut store = TripleStore::new();
    let res = store.load_hook_pack(&pack_dir);
    assert!(res.is_err());
    assert!(res.unwrap_err().contains("dependency cycle"));
    let _ = std::fs::remove_dir_all(pack_dir);
}

#[test]
fn test_fibo_blank_nodes_typed_literals_collections() {
    let fibo_ttl = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:person1 ex:hasName _:b1 .
_:b1 rdf:type ex:BlankNode ;
    ex:firstName "John"@en .

ex:amount "1000"^^<http://www.w3.org/2001/XMLSchema#integer> .

ex:list1 rdf:value (1 2 3) .
    "#;

    let store = TripleStore::from(fibo_ttl);
    eprintln!("Loaded {} triples from FIBO TTL", store.len());
    for triple in store.triple_index.triples.iter().take(5) {
        eprintln!("{:?}", triple);
    }
    assert!(store.len() > 0, "Should load at least some triples");
}
