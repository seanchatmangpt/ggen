use praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;

/// Test boundary numeric inputs in aggregations.
#[test]
fn test_boundary_numeric_inputs_aggregation() {
    let mut store = TripleStore::new();

    // Facts with negative numbers, floats, large numbers, and invalid numbers.
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e1".to_string(),
        "http://example.org/salary".to_string(),
        "-1000.50".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e2".to_string(),
        "http://example.org/salary".to_string(),
        "2000.75".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e3".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e3".to_string(),
        "http://example.org/salary".to_string(),
        "invalid_number".to_string(),
    ));

    // Sum rule: { ?d :hasEmployee ?e . ?e :salary ?s } => { ?d :totalSalary sum(?s) }
    let rule_sum = Rule {
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
    let agg_sum = Aggregate {
        function: AggregateFunction::Sum,
        source_var: "?s".to_string(),
        target_var: "?total".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    // Min rule
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
    let agg_min = Aggregate {
        function: AggregateFunction::Min,
        source_var: "?s".to_string(),
        target_var: "?min".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    // Max rule
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
    let agg_max = Aggregate {
        function: AggregateFunction::Max,
        source_var: "?s".to_string(),
        target_var: "?max".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    // Avg rule
    let rule_avg = Rule {
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
    let agg_avg = Aggregate {
        function: AggregateFunction::Avg,
        source_var: "?s".to_string(),
        target_var: "?avg".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    assert!(store.add_rule_with_aggregate(rule_sum, agg_sum).is_ok());
    assert!(store.add_rule_with_aggregate(rule_min, agg_min).is_ok());
    assert!(store.add_rule_with_aggregate(rule_max, agg_max).is_ok());
    assert!(store.add_rule_with_aggregate(rule_avg, agg_avg).is_ok());

    let derived = store.materialize().unwrap();

    let mut sum_val = 0.0;
    let mut min_val = 0.0;
    let mut max_val = 0.0;
    let mut avg_val = 0.0;

    for t in derived.iter() {
        let s = TripleStore::decode_triple(t);
        if s.contains("totalSalary") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            sum_val = parts[2].replace("\"", "").parse::<f64>().unwrap();
        } else if s.contains("minSalary") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            min_val = parts[2].replace("\"", "").parse::<f64>().unwrap();
        } else if s.contains("maxSalary") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            max_val = parts[2].replace("\"", "").parse::<f64>().unwrap();
        } else if s.contains("avgSalary") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            avg_val = parts[2].replace("\"", "").parse::<f64>().unwrap();
        }
    }

    assert!(
        (sum_val - 1000.25).abs() < 1e-5,
        "Sum should be 1000.25 but got {}",
        sum_val
    );
    assert!(
        (min_val - -1000.50).abs() < 1e-5,
        "Min should be -1000.50 but got {}",
        min_val
    );
    assert!(
        (max_val - 2000.75).abs() < 1e-5,
        "Max should be 2000.75 but got {}",
        max_val
    );
    assert!(
        (avg_val - 500.125).abs() < 1e-5,
        "Avg should be 500.125 but got {}",
        avg_val
    );
}

/// CONTRACT: Non-numeric aggregate source values never cause an error; they
/// are silently neutralized for numeric aggregates but still counted by COUNT.
///
/// When an aggregate's source variable resolves to a literal that cannot be
/// parsed as a number (e.g. "invalid_number"), the Roxi Datalog engine never
/// fails the rule or errors out of `add_rule_with_aggregate`/`materialize`.
/// Concretely (see `lib/src/aggregation.rs`):
/// - `sum()` treats an unparseable value as `0.0` (`parse::<f64>().unwrap_or(0.0)`),
///   so it contributes nothing to the total but the row is still "seen".
/// - `min()`, `max()`, and `avg()` skip the row entirely on parse failure
///   (`if let Ok(num) = ...`) — it does not affect the min/max bounds nor
///   avg's running sum/denominator.
/// - `count()` does not parse the source value at all; it increments for
///   every row regardless of whether that row's value is numeric.
///
/// This mirrors `test_boundary_numeric_inputs_aggregation` above (which mixes
/// an `"invalid_number"` salary in among valid numeric salaries and asserts
/// the sum/min/max/avg all reflect only the two valid values: -1000.50 and
/// 2000.75). This test isolates the sum/count half of that contract with a
/// minimal fixture so it is explicit and doesn't require inferring behavior
/// from the accumulator source or the larger boundary test.
///
/// Do NOT change this behavior without updating this doc-comment and the
/// corresponding assertions here and in `test_boundary_numeric_inputs_aggregation`.
#[test]
fn test_aggregate_skips_non_numeric_source_values() {
    let mut store = TripleStore::new();

    // One department with three employees: two have numeric salaries, one
    // has a non-numeric salary literal.
    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e1".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e1".to_string(),
        "http://example.org/salary".to_string(),
        "100".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e2".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e2".to_string(),
        "http://example.org/salary".to_string(),
        "invalid_number".to_string(),
    ));

    store.add(Triple::from(
        "http://example.org/d1".to_string(),
        "http://example.org/hasEmployee".to_string(),
        "http://example.org/e3".to_string(),
    ));
    store.add(Triple::from(
        "http://example.org/e3".to_string(),
        "http://example.org/salary".to_string(),
        "300".to_string(),
    ));

    let body = vec![
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
    ];

    // Sum rule: if the non-numeric row were included (or errored), the sum
    // would not cleanly equal 400 (100 + 300 from the two valid rows only).
    let rule_sum = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/totalSalary2".to_string(),
            "?total".to_string(),
        ),
        body: body.clone(),
    };
    let agg_sum = Aggregate {
        function: AggregateFunction::Sum,
        source_var: "?s".to_string(),
        target_var: "?total".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    // Count rule: if the non-numeric row were silently skipped from the row
    // set entirely (rather than just excluded from the numeric aggregation),
    // count would be 2 instead of 3. Count is expected to still see all 3
    // rows since COUNT does not need to parse the value as a number.
    let rule_count = Rule {
        head: Triple::from(
            "?d".to_string(),
            "http://example.org/employeeCount2".to_string(),
            "?count".to_string(),
        ),
        body: body.clone(),
    };
    let agg_count = Aggregate {
        function: AggregateFunction::Count,
        source_var: "?e".to_string(),
        target_var: "?count".to_string(),
        group_vars: vec!["?d".to_string()],
    };

    assert!(
        store.add_rule_with_aggregate(rule_sum, agg_sum).is_ok(),
        "add_rule_with_aggregate must not error out just because some future \
         row will contain a non-numeric value for a numeric aggregate"
    );
    assert!(store.add_rule_with_aggregate(rule_count, agg_count).is_ok());

    let derived = store.materialize().unwrap();

    let mut sum_val: Option<f64> = None;
    let mut count_val: Option<f64> = None;
    for t in derived.iter() {
        let s = TripleStore::decode_triple(t);
        if s.contains("totalSalary2") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            sum_val = Some(parts[2].replace('"', "").parse::<f64>().unwrap());
        } else if s.contains("employeeCount2") {
            let parts: Vec<&str> = s.split_whitespace().collect();
            count_val = Some(parts[2].replace('"', "").parse::<f64>().unwrap());
        }
    }

    assert_eq!(
        sum_val,
        Some(400.0),
        "sum() must silently skip the non-numeric salary and total only the valid rows"
    );
    assert_eq!(
        count_val,
        Some(3.0),
        "count() must still count all 3 rows regardless of whether the salary literal parses as a number"
    );
}
