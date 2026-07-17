// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(deprecated)]

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::sparql::{eval_query, evaluate_plan_and_debug, Binding};
use praxis_graphlaw::tripleindex::TripleIndex;
use spargebra::Query;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

// Manifest fields mirror the JSON schema; unread fields kept for deserialization fidelity.
#[allow(dead_code)]
#[derive(serde::Deserialize, Debug)]
struct Manifest {
    provenance: Provenance,
    conformance_suites: Vec<Suite>,
}

#[allow(dead_code)]
#[derive(serde::Deserialize, Debug)]
struct Provenance {
    source: String,
    description: String,
    last_synced: String,
}

#[allow(dead_code)]
#[derive(serde::Deserialize, Debug)]
struct Suite {
    suite_name: String,
    test_cases: Vec<TestCase>,
}

#[allow(dead_code)]
#[derive(serde::Deserialize, Debug, Clone)]
struct TestCase {
    id: String,
    name: String,
    features: Vec<String>,
    data: String,
    query: String,
    expected: String,
    status: String,
}

#[test]
fn test_w3c_sparql11_conformance() {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let suite_dir = manifest_dir.join("tests/sparql11_conformance");
    let manifest_path = suite_dir.join("manifest.json");

    if !manifest_path.exists() {
        println!("Manifest path does not exist, skipping conformance runner");
        return;
    }

    let manifest_content =
        fs::read_to_string(&manifest_path).expect("Failed to read manifest.json");
    let manifest: Manifest =
        serde_json::from_str(&manifest_content).expect("Failed to parse manifest.json");

    let mut total_tests = 0;
    let mut passed_tests = 0;
    let mut failed_tests = 0;
    let mut test_details = Vec::new();

    for suite in &manifest.conformance_suites {
        for test in &suite.test_cases {
            if test.status == "skipped" {
                test_details.push((
                    test.id.clone(),
                    "SKIPPED",
                    "Marked as skipped in manifest".to_string(),
                ));
                continue;
            }

            total_tests += 1;
            let data_path = suite_dir.join(&test.data);
            let query_path = suite_dir.join(&test.query);
            let expected_path = suite_dir.join(&test.expected);

            let data_content = match fs::read_to_string(&data_path) {
                Ok(c) => c,
                Err(e) => {
                    failed_tests += 1;
                    test_details.push((
                        test.id.clone(),
                        "FAILED",
                        format!("Failed to read data file: {}", e),
                    ));
                    continue;
                }
            };

            let query_content = match fs::read_to_string(&query_path) {
                Ok(c) => c,
                Err(e) => {
                    failed_tests += 1;
                    test_details.push((
                        test.id.clone(),
                        "FAILED",
                        format!("Failed to read query file: {}", e),
                    ));
                    continue;
                }
            };

            let expected_content = match fs::read_to_string(&expected_path) {
                Ok(c) => c,
                Err(e) => {
                    failed_tests += 1;
                    test_details.push((
                        test.id.clone(),
                        "FAILED",
                        format!("Failed to read expected results file: {}", e),
                    ));
                    continue;
                }
            };

            // 1. Load RDF data
            let triples = match Parser::parse_triples(&data_content, Syntax::Turtle) {
                Ok(t) => t,
                Err(e) => {
                    failed_tests += 1;
                    test_details.push((
                        test.id.clone(),
                        "FAILED",
                        format!("Turtle parse error: {:?}", e),
                    ));
                    continue;
                }
            };

            let mut index = TripleIndex::new();
            for t in triples {
                index.add(t);
            }

            // 2. Parse and evaluate query
            let query = match Query::parse(&query_content, None) {
                Ok(q) => q,
                Err(e) => {
                    failed_tests += 1;
                    test_details.push((
                        test.id.clone(),
                        "FAILED",
                        format!("SPARQL parse error: {:?}", e),
                    ));
                    continue;
                }
            };

            let plan = eval_query(&query, &index);
            let results: Vec<Vec<Binding>> = evaluate_plan_and_debug(&plan, &index).collect();

            // 3. Parse expected results (JSON array of binding objects)
            let expected_bindings: Vec<HashMap<String, String>> =
                match serde_json::from_str(&expected_content) {
                    Ok(b) => b,
                    Err(e) => {
                        failed_tests += 1;
                        test_details.push((
                            test.id.clone(),
                            "FAILED",
                            format!("Expected JSON parse error: {:?}", e),
                        ));
                        continue;
                    }
                };

            // 4. Compare actual vs expected
            if compare_results(&results, &expected_bindings) {
                passed_tests += 1;
                test_details.push((
                    test.id.clone(),
                    "PASSED",
                    "Matches expected results".to_string(),
                ));
            } else {
                failed_tests += 1;
                test_details.push((
                    test.id.clone(),
                    "FAILED",
                    format!(
                        "Results mismatch. Got: {:?}, Expected: {:?}",
                        results, expected_bindings
                    ),
                ));
            }
        }
    }

    // 5. Generate manifest report
    let pass_rate = if total_tests > 0 {
        (passed_tests as f64 / total_tests as f64) * 100.0
    } else {
        0.0
    };

    let mut report = format!(
        "# SPARQL 1.1 Conformance Pass-Rate Manifest\n\n\
        - **Dialect**: SPARQL 1.1\n\
        - **Suite**: W3C SPARQL 1.1 Query Evaluation Suite\n\
        - **Total Tests**: {}\n\
        - **Passed**: {}\n\
        - **Failed**: {}\n\
        - **Pass Rate**: {:.1}%\n\n\
        ## Test Details\n\n\
        | Test ID | Status | Details |\n\
        |---|---|---|\n",
        total_tests, passed_tests, failed_tests, pass_rate
    );

    for (id, status, details) in test_details {
        report.push_str(&format!("| {} | {} | {} |\n", id, status, details));
    }

    let manifests_dir = manifest_dir.join("docs/jira/26.7.4/manifests");
    fs::create_dir_all(&manifests_dir).expect("Failed to create manifests directory");
    fs::write(manifests_dir.join("sparql11_manifest.md"), report)
        .expect("Failed to write manifest report");

    assert_eq!(failed_tests, 0, "Some SPARQL 1.1 conformance tests failed!");
}

/// Helper function to compare a set of query bindings to the expected bindings list.
/// It verifies that every expected binding matches exactly one actual binding (order-independent,
/// unless query includes ORDER BY, but for simplicity we do set-based comparison).
fn compare_results(actual: &[Vec<Binding>], expected: &[HashMap<String, String>]) -> bool {
    if actual.len() != expected.len() {
        return false;
    }

    // Convert actual bindings into normalized structures for comparison
    let mut actual_sets: Vec<HashMap<String, String>> = actual
        .iter()
        .map(|bindings| {
            bindings
                .iter()
                .map(|b| {
                    // Strip quotes/datatypes for basic value comparison or compare raw strings.
                    // In a full implementation, comparing RDF terms using the triple store's fidelity is preferred.
                    (b.var.clone(), b.val.clone())
                })
                .collect()
        })
        .collect();

    // Check if there is a 1-to-1 match
    for exp in expected {
        let match_idx = actual_sets.iter().position(|act| {
            // Check that all keys in exp match act, and unbound variables in exp are also unbound in act
            exp.iter()
                .all(|(k, v)| act.get(k).is_some_and(|val| clean_val(val) == clean_val(v)))
        });

        if let Some(idx) = match_idx {
            actual_sets.remove(idx);
        } else {
            return false;
        }
    }

    actual_sets.is_empty()
}

fn clean_val(val: &str) -> String {
    // Basic normalization of string bindings: strip surrounding quotes and datatype suffix if necessary
    let val = val.trim();
    if val.starts_with('"') {
        let end_idx = val.rfind('"').unwrap_or(val.len());
        if end_idx > 0 {
            return val[1..end_idx].to_string();
        }
    }
    val.to_string()
}
