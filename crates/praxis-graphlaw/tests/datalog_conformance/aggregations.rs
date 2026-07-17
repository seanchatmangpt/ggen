use praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

// Proposed/expected Types for Datalog Aggregation support.
// When implementing TICKET-004, these can be defined in praxis_graphlaw::datalog or praxis_graphlaw::triples.

/// CONFORM-007: Aggregations - Count with grouping.
#[test]
fn test_aggregate_count() {
    let mut store = TripleStore::new();

    // Facts: employees in departments
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/d2".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e3".to_string(),
    ));

    // Rule: count employees grouped by department
    // In N3/Datalog syntax: { ?d :hasEmployee ?e } => { ?d :employeeCount count(?e) }
    // For test skeleton, we represent this with a rule-aggregate mapping:
    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/employeeCount".to_string(),
            "?count".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?d".to_string(),
                "http://example.org/hasEmployee".to_string(),
                "?e".to_string(),
            ),
        }],
    };

    let _agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    // Placeholder assert that compiles but fails until feature is implemented
    let res = store.add_rule_with_aggregate(rule, _agg);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();

    let count_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("employeeCount")
        })
        .map(TripleStore::decode_triple)
        .collect();

    assert_eq!(count_triples.len(), 2);
    let d1_correct = count_triples
        .iter()
        .any(|s| s.contains("d1") && s.contains("2"));
    let d2_correct = count_triples
        .iter()
        .any(|s| s.contains("d2") && s.contains("1"));
    assert!(d1_correct, "d1 count should be 2");
    assert!(d2_correct, "d2 count should be 1");
}

/// CONFORM-008: Aggregations - Sum with grouping.
#[test]
fn test_aggregate_sum() {
    let mut store = TripleStore::new();

    // Facts: employees, departments, and salaries
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e1".to_string(),
        "http://example.org/salary".to_string(),
        "1000".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e2".to_string(),
        "http://example.org/salary".to_string(),
        "2000".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d2".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e3".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e3".to_string(),
        "http://example.org/salary".to_string(),
        "1500".to_string(),
    ));

    // Rule: sum salary grouped by department
    // In N3/Datalog: { ?d :hasEmployee ?e . ?e :salary ?s } => { ?d :totalSalary sum(?s) }
    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/totalSalary".to_string(),
            "?total".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?e".to_string(),
                    "http://example.org/salary".to_string(),
                    "?s".to_string(),
                ),
            },
        ],
    };

    let _agg = Aggregate {
        function: AggregateFunction::Sum,
        source_var: "?s".to_string(),
        target_var: "?total".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let res = store.add_rule_with_aggregate(rule, _agg);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();

    let sum_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("totalSalary")
        })
        .map(TripleStore::decode_triple)
        .collect();

    assert_eq!(sum_triples.len(), 2);
    let d1_correct = sum_triples
        .iter()
        .any(|s| s.contains("d1") && s.contains("3000"));
    let d2_correct = sum_triples
        .iter()
        .any(|s| s.contains("d2") && s.contains("1500"));
    assert!(d1_correct, "d1 totalSalary should be 3000");
    assert!(d2_correct, "d2 totalSalary should be 1500");
}

/// CONFORM-009: Aggregations - Min/Max with grouping.
#[test]
fn test_aggregate_min_max() {
    let mut store = TripleStore::new();

    // Facts: employees, departments, and salaries
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e1".to_string(),
        "http://example.org/salary".to_string(),
        "1000".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e2".to_string(),
        "http://example.org/salary".to_string(),
        "2000".to_string(),
    ));

    // Rule for MIN: { ?d :hasEmployee ?e . ?e :salary ?s } => { ?d :minSalary min(?s) }
    // Rule for MAX: { ?d :hasEmployee ?e . ?e :salary ?s } => { ?d :maxSalary max(?s) }
    let rule_min = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/minSalary".to_string(),
            "?min".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?e".to_string(),
                    "http://example.org/salary".to_string(),
                    "?s".to_string(),
                ),
            },
        ],
    };

    let _agg_min = Aggregate {
        function: AggregateFunction::Min,
        source_var: "?s".to_string(),
        target_var: "?min".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let _agg_max = Aggregate {
        function: AggregateFunction::Max,
        source_var: "?s".to_string(),
        target_var: "?max".to_string(),
        group_vars: vec!["?d".to_string()],
    };
    let rule_max = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/maxSalary".to_string(),
            "?max".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?e".to_string(),
                    "http://example.org/salary".to_string(),
                    "?s".to_string(),
                ),
            },
        ],
    };

    let res1 = store.add_rule_with_aggregate(rule_min, _agg_min);
    let res2 = store.add_rule_with_aggregate(rule_max, _agg_max);
    assert!(res1.is_ok());
    assert!(res2.is_ok());

    let derived = store.materialize().unwrap();

    let min_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("minSalary")
        })
        .map(TripleStore::decode_triple)
        .collect();

    let max_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("maxSalary")
        })
        .map(TripleStore::decode_triple)
        .collect();

    assert!(min_triples
        .iter()
        .any(|s| s.contains("d1") && s.contains("1000")));
    assert!(max_triples
        .iter()
        .any(|s| s.contains("d1") && s.contains("2000")));
}

/// CONFORM-010: Aggregations - Avg with grouping.
#[test]
fn test_aggregate_avg() {
    let mut store = TripleStore::new();

    // Facts: employees, departments, and salaries
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e1".to_string(),
        "http://example.org/salary".to_string(),
        "1000".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e2".to_string(),
        "http://example.org/salary".to_string(),
        "2000".to_string(),
    ));

    // Rule: avg salary grouped by department
    // In N3/Datalog: { ?d :hasEmployee ?e . ?e :salary ?s } => { ?d :avgSalary avg(?s) }
    let rule = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/avgSalary".to_string(),
            "?avg".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?e".to_string(),
                    "http://example.org/salary".to_string(),
                    "?s".to_string(),
                ),
            },
        ],
    };

    let _agg = Aggregate {
        function: AggregateFunction::Avg,
        source_var: "?s".to_string(),
        target_var: "?avg".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    let res = store.add_rule_with_aggregate(rule, _agg);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();

    let avg_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("avgSalary")
        })
        .map(TripleStore::decode_triple)
        .collect();

    assert!(avg_triples
        .iter()
        .any(|s| s.contains("d1") && s.contains("1500")));
}

/// CONFORM-011: Aggregations - Recursive aggregates.
/// Aggregates computed over recursive input relations (e.g. Component Cost Hierarchy).
#[test]
fn test_aggregate_recursive() {
    let mut store = TripleStore::new();

    // Assembly facts: Part contains Subpart
    store.add(Triple::from(
        "http://example.org/car".to_string(),
        "http://example.org/contains".to_string(),
        "http://example.org/engine".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/engine".to_string(),
        "http://example.org/contains".to_string(),
        "http://example.org/piston".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/engine".to_string(),
        "http://example.org/contains".to_string(),
        "http://example.org/valve".to_string(),
    ));

    // Base costs
    store.add(Triple::from(
        "http://example.org/piston".to_string(),
        "http://example.org/cost".to_string(),
        "150".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/valve".to_string(),
        "http://example.org/cost".to_string(),
        "50".to_string(),
    ));

    // Rule 1: Transitive reachability of subparts (recursive)
    // { ?x :contains ?y } => { ?x :subpart ?y }
    // { ?x :contains ?y . ?y :subpart ?z } => { ?x :subpart ?z }
    let r1 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/subpart".to_string(),
            "?y".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/contains".to_string(),
                "?y".to_string(),
            ),
        }],
    };

    let r2 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/subpart".to_string(),
            "?z".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/contains".to_string(),
                    "?y".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?y".to_string(),
                    "http://example.org/subpart".to_string(),
                    "?z".to_string(),
                ),
            },
        ],
    };

    // Rule 2: Cost propagation of subparts
    // { ?x :subpart ?y . ?y :cost ?c } => { ?x :subpartCost ?c }
    let r3 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/subpartCost".to_string(),
            "?c".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?x".to_string(),
                    "http://example.org/subpart".to_string(),
                    "?y".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?y".to_string(),
                    "http://example.org/cost".to_string(),
                    "?c".to_string(),
                ),
            },
        ],
    };

    // Rule 3: Sum of subpart costs to compute total part cost (aggregation over recursive relation)
    // { ?x :subpartCost ?c } => { ?x :totalCost sum(?c) }
    let r4 = Rule {
        head: Triple::from(
            "?x".to_string(),
            "http://example.org/totalCost".to_string(),
            "?tc".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                "http://example.org/subpartCost".to_string(),
                "?c".to_string(),
            ),
        }],
    };

    let _agg = Aggregate {
        function: AggregateFunction::Sum,
        source_var: "?c".to_string(),
        target_var: "?tc".to_string(),
        group_vars: vec!["?x".to_string()],
    };

    let res1 = store.add_rules(vec![r1, r2, r3]);
    let res2 = store.add_rule_with_aggregate(r4, _agg);
    assert!(res1.is_ok());
    assert!(res2.is_ok());

    let derived = store.materialize().unwrap();

    let cost_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("totalCost")
        })
        .map(TripleStore::decode_triple)
        .collect();

    let engine_correct = cost_triples
        .iter()
        .any(|s| s.contains("engine") && s.contains("200"));
    let car_correct = cost_triples
        .iter()
        .any(|s| s.contains("car") && s.contains("200"));
    assert!(engine_correct, "engine totalCost should be 200");
    assert!(car_correct, "car totalCost should be 200");
}

/// CONFORM-012: Aggregations - Count with a TWO-variable group-by.
///
/// Facts model employees who each belong to a department AND a team. Grouping
/// must be performed per unique (department, team) combination, not per
/// department alone and not per team alone.
///
/// Facts:
/// - d1/teamA: e1, e2   (count = 2)
/// - d1/teamB: e3       (count = 1)
/// - d2/teamA: e4       (count = 1)
#[test]
fn test_aggregate_count_multi_variable_group_by() {
    let mut store = TripleStore::new();

    let employees = [
        (
            "http://example.org/e1",
            "http://example.org/d1",
            "http://example.org/teamA",
        ),
        (
            "http://example.org/e2",
            "http://example.org/d1",
            "http://example.org/teamA",
        ),
        (
            "http://example.org/e3",
            "http://example.org/d1",
            "http://example.org/teamB",
        ),
        (
            "http://example.org/e4",
            "http://example.org/d2",
            "http://example.org/teamA",
        ),
    ];

    for (e, d, t) in employees.iter() {
        store.add(Triple::from(
            d.to_string(),
            "http://example.org/hasEmployee".to_string(),
            e.to_string(),
        ));
        store.add(Triple::from(
            e.to_string(),
            "http://example.org/team".to_string(),
            t.to_string(),
        ));
    }

    // Rule: { ?d :hasEmployee ?e . ?e :team ?t } => { ?d :teamCount count(?e) }
    // grouped by the pair (?d, ?t). The Triple head only has 3 slots, so ?t
    // does not appear in the derived triple directly, but if the aggregate
    // only grouped by ?d, d1's employees (e1, e2, e3 across two teams) would
    // collapse into a single count-of-3 row instead of two rows (2 and 1).
    // Asserting 3 distinct rows with the correct per-group counts below
    // proves both group_vars entries are actually used as the grouping key.
    let rule_by_dept_team = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/teamCount".to_string(),
            "?count".to_string(),
        ),
        body: vec![
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            },
            BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?e".to_string(),
                    "http://example.org/team".to_string(),
                    "?t".to_string(),
                ),
            },
        ],
    };

    let agg = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string(), "?t".to_string()],
    };

    let res = store.add_rule_with_aggregate(rule_by_dept_team, agg);
    assert!(res.is_ok());

    let derived = store.materialize().unwrap();

    let count_triples: Vec<String> = derived
        .iter()
        .filter(|t| {
            let s = TripleStore::decode_triple(t);
            s.contains("teamCount")
        })
        .map(TripleStore::decode_triple)
        .collect();

    // Three distinct (?d, ?t) combinations: (d1,teamA), (d1,teamB), (d2,teamA).
    // If grouping only used ?d, we would see just 2 rows (d1 collapsed to one
    // count of 3, d2 to one count of 1). Three rows proves both variables
    // participate in the grouping key.
    assert_eq!(
        count_triples.len(),
        3,
        "expected 3 distinct (department, team) groups, got: {:?}",
        count_triples
    );

    let d1_count_2 = count_triples
        .iter()
        .filter(|s| s.contains("d1") && s.trim_end().ends_with(" 2"))
        .count();
    let d1_count_1 = count_triples
        .iter()
        .filter(|s| s.contains("d1") && s.trim_end().ends_with(" 1"))
        .count();
    let d2_count_1 = count_triples
        .iter()
        .filter(|s| s.contains("d2") && s.trim_end().ends_with(" 1"))
        .count();

    assert_eq!(
        d1_count_2, 1,
        "d1/teamA should have a count-of-2 group: {:?}",
        count_triples
    );
    assert_eq!(
        d1_count_1, 1,
        "d1/teamB should have a count-of-1 group: {:?}",
        count_triples
    );
    assert_eq!(
        d2_count_1, 1,
        "d2/teamA should have a count-of-1 group: {:?}",
        count_triples
    );
}
