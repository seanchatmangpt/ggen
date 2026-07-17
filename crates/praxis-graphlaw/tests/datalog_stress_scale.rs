//! Stress tests for large-scale aggregation and composition patterns in the Datalog
//! engine (`praxis_graphlaw::datalog`). Tests rule application and fact derivation
//! at scale (hundreds to thousands of facts), ensuring aggregation and stratified
//! negation over complex derived predicates remain efficient.

use praxis_graphlaw::encoding::Encoder;
use praxis_graphlaw::triples::{
    Aggregate, AggregateFunction, BodyLiteral, Rule, Triple, VarOrTerm,
};
use praxis_graphlaw::TripleStore;
use std::time::{Duration, Instant};

fn decode_all(triples: &[Triple]) -> Vec<String> {
    triples.iter().map(TripleStore::decode_triple).collect()
}

fn pred(name: &str) -> String {
    format!("http://example.org/{}", name)
}

/// Stress: aggregation (COUNT grouped by department) over 1000 facts across
/// 50 groups must produce exactly the right per-group counts and complete
/// quickly. This is ~300x more facts than the conformance suite's
/// hand-picked 3-employee example.
#[test]
fn test_large_scale_grouped_aggregation() {
    const NUM_DEPTS: usize = 50;
    const EMPLOYEES_PER_DEPT: usize = 20; // 1000 facts total

    let mut store = TripleStore::new();
    for d in 0..NUM_DEPTS {
        for e in 0..EMPLOYEES_PER_DEPT {
            store.add(Triple::from(
                format!("http://example.org/dept{}", d),
                pred("hasEmployee"),
                format!("http://example.org/emp{}_{}", d, e),
            ));
        }
    }

    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("employeeCount"),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from("?d".to_string(), pred("hasEmployee"), "?e".to_string()),
        }],
    };
    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let start = Instant::now();
    store
        .add_rule_with_aggregate(rule, agg)
        .expect("valid aggregate rule over 1000 facts must be accepted");
    let derived = store.materialize().unwrap();
    let elapsed = start.elapsed();

    let decoded: Vec<String> = derived.iter().map(TripleStore::decode_triple).collect();
    let count_facts: Vec<&String> = decoded
        .iter()
        .filter(|d| d.contains("employeeCount"))
        .collect();

    assert_eq!(
        count_facts.len(),
        NUM_DEPTS,
        "expected exactly {} employeeCount facts (one per department), got {}: {:?}",
        NUM_DEPTS,
        count_facts.len(),
        count_facts
    );
    for d in 0..NUM_DEPTS {
        assert!(
            count_facts
                .iter()
                .any(|f| f.contains(&format!("/dept{}>", d))
                    && f.contains(&EMPLOYEES_PER_DEPT.to_string())),
            "dept{} should have employeeCount={}, got: {:?}",
            d,
            EMPLOYEES_PER_DEPT,
            count_facts
        );
    }

    assert!(
        elapsed < Duration::from_secs(10),
        "1000-fact grouped aggregation took {:?}, expected well under 10s",
        elapsed
    );
}

/// Composition: a rule negates on a predicate that is itself derived from a
/// value produced by an EARLIER-stratum aggregate (COUNT), rather than
/// negation-depth and aggregation being tested only in isolation as the
/// other stress tests do. Shape:
///   employeeCount(d, c)   :- hasEmployee(d, e)        [COUNT aggregate, stratum 0]
///   HighStaff(d)          :- employeeCount(d, "5")     [ordinary join on the
///                                                        aggregate's own output, stratum 0/1]
///   LowStaffAlert(d)      :- isDept(d), not HighStaff(d) [negates on a
///                                                          rule that consumes
///                                                          the aggregate, stratum 2]
/// dept1 has 3 employees (not HighStaff -> alerted), dept2 has 5 (HighStaff
/// -> not alerted). This is the realistic real-world composition pattern
/// (aggregate -> derived boolean -> negated), which neither
/// `test_large_scale_grouped_aggregation` (no negation) nor the
/// stratification-chain tests in the recursion suite (no aggregation) actually exercise.
#[test]
fn test_stratified_negation_over_aggregate_derived_predicate() {
    let mut store = TripleStore::new();
    for e in 0..3 {
        store.add(Triple::from(
            "http://example.org/dept1".to_string(),
            pred("hasEmployee"),
            format!("http://example.org/emp1_{}", e),
        ));
    }
    for e in 0..5 {
        store.add(Triple::from(
            "http://example.org/dept2".to_string(),
            pred("hasEmployee"),
            format!("http://example.org/emp2_{}", e),
        ));
    }
    store.add(Triple::from(
        "http://example.org/dept1".to_string(),
        pred("isDept"),
        "http://example.org/true".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/dept2".to_string(),
        pred("isDept"),
        "http://example.org/true".to_string(),
    ));

    let count_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("employeeCount"),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from("?d".to_string(), pred("hasEmployee"), "?e".to_string()),
        }],
    };
    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };
    store
        .add_rule_with_aggregate(count_rule, agg)
        .expect("aggregate rule must be accepted");

    // The COUNT aggregate encodes its numeric result via a raw,
    // bracket-less `Encoder::add(count.to_string())` (see aggregation.rs),
    // NOT through `Triple::from`'s normal convention of wrapping bare
    // strings in `<...>` before interning -- so matching it here requires
    // building the object term from the exact same raw-string encoding
    // rather than going through `Triple::from("...", "...", "5")`, which
    // would intern the differently-shaped string "<5>" and never match.
    let five_id = Encoder::add("5".to_string());
    let high_staff_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("HighStaff"),
            "http://example.org/true".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple {
                s: VarOrTerm::convert("?d".to_string()),
                p: VarOrTerm::convert(pred("employeeCount")),
                o: VarOrTerm::new_encoded_term(five_id),
                g: None,
            },
        }],
    };
    let low_staff_alert_rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            pred("LowStaffAlert"),
            "http://example.org/true".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    pred("isDept"),
                    "http://example.org/true".to_string(),
                ),
            },
            BodyLiteral {
                negated: true,
                pattern: Triple::from(
                    "?d".to_string(),
                    pred("HighStaff"),
                    "http://example.org/true".to_string(),
                ),
            },
        ],
    };
    store
        .add_rules(vec![high_staff_rule, low_staff_alert_rule])
        .expect(
            "negation over an aggregate-derived predicate must be accepted as safely stratifiable",
        );

    let derived = store.materialize().unwrap();
    let decoded = decode_all(&derived);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/dept2") && d.contains("HighStaff")),
        "dept2 (5 employees) must be classified HighStaff, got: {:?}",
        decoded
    );
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/dept1") && d.contains("HighStaff")),
        "dept1 (3 employees) must NOT be classified HighStaff, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/dept1") && d.contains("LowStaffAlert")),
        "dept1 (not HighStaff) must be flagged LowStaffAlert, got: {:?}",
        decoded
    );
    assert!(
        !decoded
            .iter()
            .any(|d| d.contains("/dept2") && d.contains("LowStaffAlert")),
        "dept2 (HighStaff) must NOT be flagged LowStaffAlert, got: {:?}",
        decoded
    );
}
