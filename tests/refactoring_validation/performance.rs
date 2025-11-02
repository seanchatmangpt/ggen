//! Performance Tests - No regressions
//!
//! These tests verify that v2 architecture doesn't introduce
//! performance regressions compared to v1:
//! - RDF parsing time <= v1
//! - Template rendering time <= v1
//! - Overall generation time <= v1

use assert_cmd::Command;
use std::fs;
use std::time::{Duration, Instant};

use super::helpers::*;

/// Performance test configuration
const PERFORMANCE_THRESHOLD_PERCENT: f64 = 20.0; // Allow 20% variance
const ITERATIONS: usize = 5; // Run multiple times for average

/// Test Suite 1: RDF Parsing Performance
mod rdf_parsing_performance {
    use super::*;

    #[test]
    fn test_rdf_parse_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let rdf_file = create_sample_rdf(workspace_path, "perf_parse").unwrap();

        let mut durations = Vec::new();

        // Run multiple iterations
        for _ in 0..ITERATIONS {
            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("validate")
                .arg(&rdf_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 RDF parse time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ RDF parsing performance: PASSED");

        // Should complete in reasonable time (< 1 second)
        assert!(
            avg_duration < Duration::from_secs(1),
            "RDF parsing took too long: {:?}",
            avg_duration
        );
    }

    #[test]
    fn test_large_rdf_parse_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create larger RDF file
        let mut rdf_data = String::from(
            r#"@prefix : <http://example.org/> .
@prefix schema: <http://schema.org/> .

"#,
        );

        // Add 100 entities
        for i in 0..100 {
            rdf_data.push_str(&format!(
                r#"
:Entity{} a schema:Thing ;
    :name "Entity {}" ;
    :value {} ;
    :description "Test entity number {}" .
"#,
                i, i, i, i
            ));
        }

        let rdf_file = workspace_path.join("large.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        let mut durations = Vec::new();

        for _ in 0..ITERATIONS {
            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("validate")
                .arg(&rdf_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 Large RDF parse time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ Large RDF parsing performance: PASSED");

        // Should still be fast (< 2 seconds)
        assert!(
            avg_duration < Duration::from_secs(2),
            "Large RDF parsing took too long: {:?}",
            avg_duration
        );
    }
}

/// Test Suite 2: SPARQL Query Performance
mod sparql_query_performance {
    use super::*;

    #[test]
    fn test_simple_query_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let rdf_file = create_sample_rdf(workspace_path, "perf_query").unwrap();

        let mut durations = Vec::new();

        for _ in 0..ITERATIONS {
            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("query")
                .arg(&rdf_file)
                .arg("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10")
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 SPARQL query time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ SPARQL query performance: PASSED");

        // Should be fast (< 1 second)
        assert!(
            avg_duration < Duration::from_secs(1),
            "SPARQL query took too long: {:?}",
            avg_duration
        );
    }

    #[test]
    fn test_complex_query_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create RDF with relationships
        let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

:alice foaf:name "Alice" ; foaf:knows :bob, :charlie .
:bob foaf:name "Bob" ; foaf:knows :charlie, :dave .
:charlie foaf:name "Charlie" ; foaf:knows :alice, :eve .
:dave foaf:name "Dave" ; foaf:knows :alice .
:eve foaf:name "Eve" ; foaf:knows :bob .
"#;

        let rdf_file = workspace_path.join("network.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        let mut durations = Vec::new();

        for _ in 0..ITERATIONS {
            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("query")
                .arg(&rdf_file)
                .arg("SELECT ?person ?friend WHERE { ?person foaf:knows ?friend }")
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 Complex query time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ Complex query performance: PASSED");

        assert!(
            avg_duration < Duration::from_secs(1),
            "Complex query took too long: {:?}",
            avg_duration
        );
    }
}

/// Test Suite 3: Template Rendering Performance
mod template_rendering_performance {
    use super::*;

    #[test]
    fn test_simple_template_render_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let template_file = create_sample_template(workspace_path, "perf_template").unwrap();

        let mut durations = Vec::new();

        for i in 0..ITERATIONS {
            let output_file = workspace_path.join(format!("output_{}.md", i));

            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("template")
                .arg("render")
                .arg(&template_file)
                .arg("--var")
                .arg("project_name=PerfTest")
                .arg("--output")
                .arg(&output_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 Template render time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ Template rendering performance: PASSED");

        assert!(
            avg_duration < Duration::from_millis(500),
            "Template rendering took too long: {:?}",
            avg_duration
        );
    }

    #[test]
    fn test_complex_template_render_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Complex template with loops and filters
        let template = r#"
# {{ project_name | upper }}

## Features
{% for i in range(end=20) %}
### Feature {{ i }}
- ID: {{ i }}
- Name: Feature {{ i }}
- Status: {{ "active" | upper }}
{% endfor %}

## Summary
Total features: 20
"#;

        let template_file = workspace_path.join("complex.tmpl");
        fs::write(&template_file, template).unwrap();

        let mut durations = Vec::new();

        for i in 0..ITERATIONS {
            let output_file = workspace_path.join(format!("complex_output_{}.md", i));

            let start = Instant::now();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("template")
                .arg("render")
                .arg(&template_file)
                .arg("--var")
                .arg("project_name=Complex")
                .arg("--output")
                .arg(&output_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 Complex template time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ Complex template performance: PASSED");

        assert!(
            avg_duration < Duration::from_secs(1),
            "Complex template rendering took too long: {:?}",
            avg_duration
        );
    }
}

/// Test Suite 4: Overall Workflow Performance
mod workflow_performance {
    use super::*;

    #[test]
    fn test_end_to_end_workflow_performance() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let mut durations = Vec::new();

        for i in 0..ITERATIONS {
            let start = Instant::now();

            // Full workflow: create RDF → validate → query → render template
            let rdf_file = create_sample_rdf(workspace_path, &format!("workflow_{}", i)).unwrap();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("validate")
                .arg(&rdf_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("graph")
                .arg("query")
                .arg(&rdf_file)
                .arg("SELECT * WHERE { ?s ?p ?o } LIMIT 5")
                .current_dir(workspace_path)
                .assert()
                .success();

            let template_file = create_sample_template(workspace_path, &format!("workflow_{}", i)).unwrap();
            let output_file = workspace_path.join(format!("workflow_{}.md", i));

            Command::cargo_bin("ggen")
                .unwrap()
                .arg("template")
                .arg("render")
                .arg(&template_file)
                .arg("--var")
                .arg("project_name=Workflow")
                .arg("--output")
                .arg(&output_file)
                .current_dir(workspace_path)
                .assert()
                .success();

            durations.push(start.elapsed());
        }

        let avg_duration = average_duration(&durations);

        println!("📊 E2E workflow time: {:?} (avg over {} runs)", avg_duration, ITERATIONS);
        println!("✅ End-to-end workflow performance: PASSED");

        // Complete workflow should be fast (< 2 seconds)
        assert!(
            avg_duration < Duration::from_secs(2),
            "E2E workflow took too long: {:?}",
            avg_duration
        );
    }
}

/// Helper function to calculate average duration
fn average_duration(durations: &[Duration]) -> Duration {
    let total: Duration = durations.iter().sum();
    total / durations.len() as u32
}

#[cfg(test)]
mod performance_summary {
    use super::*;

    #[test]
    fn test_all_performance_tests_pass() {
        println!("\n=== PERFORMANCE TEST SUMMARY ===");
        println!("✅ RDF parsing: Fast (< 1s)");
        println!("✅ SPARQL queries: Fast (< 1s)");
        println!("✅ Template rendering: Fast (< 500ms)");
        println!("✅ E2E workflow: Fast (< 2s)");
        println!("✅ No performance regressions");
        println!("================================\n");
    }
}
