//! NK combinatorial-interaction study for SHACL constraint combinations
//! (docs/jira/26.7.4/012-shacl-fmea-matrix.md, docs/jira/26.7.4's breakage
//! ledger). Every prior test in this session verified constraints in
//! isolation or hand-picked pairs. This harness instead:
//!
//! 1. Represents ~11 constraint types generically over a shared domain (a
//!    set of integer-literal values assigned to `ex:val` on a single focus
//!    node), each with (a) a TTL shape fragment generator and (b) an
//!    INDEPENDENT oracle function computing whether that one constraint is
//!    satisfied -- written from the constraint's own definition, not by
//!    trusting the engine.
//! 2. Combines K constraints (K=2 exhaustive over representative parameters;
//!    K=3-4 randomly sampled) into ONE property shape, and asserts the real
//!    `Validator::validate` engine's `report.conforms` matches the
//!    conjunction of all K independent oracles -- i.e. this is genuinely
//!    checking constraint *interaction*, not re-testing each constraint
//!    alone.
//! 3. Falsification-seeking: value generation is biased toward each active
//!    constraint's own boundary (not uniform-random), since off-by-one and
//!    interaction bugs concentrate at boundaries, not in "typical" data.
//!
//! TPS framing: every disagreement between the real engine and the
//! independent oracle is reported individually (andon) with the exact
//! triggering combination -- there is no aggregate pass-rate metric.

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator};
use praxis_graphlaw::tripleindex::TripleIndex;
use proptest::prelude::*;

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

/// One of the ~11 constraint families exercised by this study, each
/// generically defined over "the set of integer values assigned to
/// `ex:val`" so any subset can be combined into a single property shape.
#[derive(Debug, Clone)]
// Variant names mirror the SHACL constraint vocabulary (sh:nodeKind etc.).
#[allow(clippy::enum_variant_names)]
enum Kind {
    MinCount(i64),
    MaxCount(i64),
    MinInclusive(i64),
    MaxInclusive(i64),
    Datatype(&'static str), // "integer" (always true for our domain) or "string" (always false)
    Pattern(&'static str),  // regex on the lexical form of each value
    HasValue(i64),
    In(Vec<i64>),
    MinLength(i64),
    MaxLength(i64),
    NodeKind(&'static str), // "Literal" (always true) or "IRI" (always false)
}

impl Kind {
    fn ttl_fragment(&self) -> String {
        match self {
            Kind::MinCount(n) => format!("sh:minCount {n}"),
            Kind::MaxCount(n) => format!("sh:maxCount {n}"),
            Kind::MinInclusive(v) => format!("sh:minInclusive {v}"),
            Kind::MaxInclusive(v) => format!("sh:maxInclusive {v}"),
            Kind::Datatype(dt) => format!("sh:datatype xsd:{dt}"),
            Kind::Pattern(p) => format!("sh:pattern \"{p}\""),
            Kind::HasValue(v) => format!("sh:hasValue {v}"),
            Kind::In(vs) => format!(
                "sh:in ( {} )",
                vs.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            ),
            Kind::MinLength(n) => format!("sh:minLength {n}"),
            Kind::MaxLength(n) => format!("sh:maxLength {n}"),
            Kind::NodeKind(k) => format!("sh:nodeKind sh:{k}"),
        }
    }

    /// Independent oracle: does the WHOLE set of values satisfy this one
    /// constraint, per its own definition (not the engine's behavior)?
    /// Per-value constraints require every value to individually satisfy;
    /// per-property constraints (count, hasValue) look at the whole set.
    fn satisfies(&self, values: &[i64]) -> bool {
        match self {
            Kind::MinCount(n) => values.len() as i64 >= *n,
            Kind::MaxCount(n) => values.len() as i64 <= *n,
            Kind::MinInclusive(v) => values.iter().all(|x| x >= v),
            Kind::MaxInclusive(v) => values.iter().all(|x| x <= v),
            Kind::Datatype(dt) => *dt == "integer", // our domain always emits xsd:integer literals
            Kind::Pattern(p) => {
                let re = regex::Regex::new(p).unwrap();
                values.iter().all(|x| re.is_match(&x.to_string()))
            }
            Kind::HasValue(v) => values.contains(v),
            Kind::In(vs) => values.iter().all(|x| vs.contains(x)),
            Kind::MinLength(n) => values.iter().all(|x| x.to_string().len() as i64 >= *n),
            Kind::MaxLength(n) => values.iter().all(|x| x.to_string().len() as i64 <= *n),
            Kind::NodeKind(k) => *k == "Literal", // our domain always emits literal values
        }
    }
}

/// A fixed, representative pool of constraint instances (with concrete
/// parameters chosen to be boundary-adjacent to the value ranges this
/// study generates) used for the K=2 exhaustive sweep.
fn representative_pool() -> Vec<Kind> {
    vec![
        Kind::MinCount(2),
        Kind::MaxCount(3),
        Kind::MinInclusive(0),
        Kind::MaxInclusive(10),
        Kind::Datatype("integer"),
        Kind::Pattern("^[0-9]+$"),
        Kind::HasValue(5),
        Kind::In(vec![1, 2, 3, 5, 8]),
        Kind::MinLength(1),
        Kind::MaxLength(2),
        Kind::NodeKind("Literal"),
    ]
}

/// Build a property shape combining all given constraints with AND
/// semantics (SHACL's default: sibling constraints on one property shape
/// are all required), validate a candidate value set, and return
/// `report.conforms`.
fn run_real_engine(kinds: &[Kind], values: &[i64]) -> bool {
    let fragments: Vec<String> = kinds.iter().map(Kind::ttl_fragment).collect();
    let shapes_str = format!(
        r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
        ex:S a sh:NodeShape ; sh:targetClass ex:Item ;
            sh:property [ sh:path ex:val ; {} ] .
        "#,
        fragments.join(" ; ")
    );
    let shapes = ShapesGraph::parse(&shapes_str).unwrap();

    let mut data_str = String::from("@prefix ex: <http://example.org/> .\nex:i a ex:Item .\n");
    for v in values {
        data_str.push_str(&format!("ex:i ex:val {v} .\n"));
    }
    let data = build_data_index(&data_str);
    Validator::validate(&data, &shapes).conforms
}

fn independent_oracle(kinds: &[Kind], values: &[i64]) -> bool {
    kinds.iter().all(|k| k.satisfies(values))
}

/// Falsification-seeking value sets: not uniform-random, but concentrated
/// around the representative pool's own boundaries (0, 1, 2, 3, 5, 8, 10,
/// and their neighbors), since interaction bugs concentrate at boundaries.
fn boundary_adjacent_value_sets() -> Vec<Vec<i64>> {
    vec![
        vec![],
        vec![0],
        vec![1],
        vec![2],
        vec![3],
        vec![5],
        vec![8],
        vec![10],
        vec![-1],
        vec![11],
        vec![1, 2],
        vec![1, 2, 3],
        vec![5, 5],
        vec![1, 2, 3, 5],
        vec![1, 2, 3, 5, 8],
        vec![100],
    ]
}

/// K=2: exhaustive over all C(11,2)=55 pairs from the representative pool,
/// each checked against every boundary-adjacent value set.
#[test]
fn nk_study_k2_exhaustive() {
    let pool = representative_pool();
    let value_sets = boundary_adjacent_value_sets();
    let mut disagreements = Vec::new();
    let mut cases_run = 0usize;

    for i in 0..pool.len() {
        for j in (i + 1)..pool.len() {
            let combo = vec![pool[i].clone(), pool[j].clone()];
            for values in &value_sets {
                cases_run += 1;
                let real = run_real_engine(&combo, values);
                let oracle = independent_oracle(&combo, values);
                if real != oracle {
                    disagreements.push(format!(
                        "K=2 DISAGREEMENT: constraints={:?} values={:?} real_engine_conforms={} independent_oracle={}",
                        combo, values, real, oracle
                    ));
                }
            }
        }
    }

    println!(
        "[andon board] K=2 exhaustive: {} pairs x {} value-sets = {} cases run, {} disagreements",
        pool.len() * (pool.len() - 1) / 2,
        value_sets.len(),
        cases_run,
        disagreements.len()
    );

    assert!(
        disagreements.is_empty(),
        "NK study (K=2, {} cases run) found {} andon-worthy disagreement(s) between the real engine and the independent oracle:\n{}",
        cases_run, disagreements.len(), disagreements.join("\n")
    );
}

// K=3/K=4: randomly sampled (not exhaustive -- C(11,4)=330 combined with
// many value sets is large but doesn't need full coverage to be useful)
// via proptest, each generated combination checked against a
// proptest-generated value vector as well (combinatorial fuzzing of both
// the *structure* and the *data* dimensions simultaneously).
proptest! {
    #[test]
    fn nk_study_k3_k4_sampled(
        indices in prop::collection::hash_set(0usize..11, 3..=4),
        values in prop::collection::vec(-5i64..15, 0..6),
    ) {
        let pool = representative_pool();
        let combo: Vec<Kind> = indices.iter().map(|&i| pool[i].clone()).collect();

        let real = run_real_engine(&combo, &values);
        let oracle = independent_oracle(&combo, &values);

        prop_assert_eq!(
            real, oracle,
            "K={} DISAGREEMENT: constraints={:?} values={:?} real_engine_conforms={} independent_oracle={}",
            combo.len(), combo, values, real, oracle
        );
    }
}
