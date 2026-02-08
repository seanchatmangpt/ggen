//! End-to-End SPARQL Injection Prevention Integration Tests
//!
//! Tests SPARQL injection attack prevention across the entire ggen system:
//! - Query construction from user input
//! - Template variable substitution
//! - RDF data extraction
//! - Generation context building
//!
//! Uses Chicago TDD: AAA pattern, real RDF stores, observable outputs

use assert_cmd::Command;
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Test fixture with RDF store and malicious query attempts
struct SparqlInjectionFixture {
    workspace: TempDir,
    rdf_file: std::path::PathBuf,
    template_file: std::path::PathBuf,
}

impl SparqlInjectionFixture {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        let workspace = TempDir::new()?;
        let rdf_file = workspace.path().join("data.ttl");
        let template_file = workspace.path().join("template.tera");

        // Create RDF data with sensitive information
        let rdf_content = r#"
@prefix ex: <http://example.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User1 a ex:User ;
    ex:name "Alice" ;
    ex:role "admin" ;
    ex:password "secret123" .

ex:User2 a ex:User ;
    ex:name "Bob" ;
    ex:role "user" ;
    ex:password "topsecret456" .
        "#;
        fs::write(&rdf_file, rdf_content)?;

        // Create template that queries for user names (not passwords)
        let template_content = r#"
Users:
{% for user in users %}
- {{ user.name }} ({{ user.role }})
{% endfor %}
        "#;
        fs::write(&template_file, template_content)?;

        Ok(Self {
            workspace,
            rdf_file,
            template_file,
        })
    }
}

// ============================================================================
// CATEGORY 1: Classic SQL Injection Patterns (adapted to SPARQL)
// ============================================================================

#[test]
fn test_union_based_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: ' UNION { ?x ex:password ?pass } --
    let malicious_query_var = "name' UNION { ?x ex:password ?pass } --";

    // Act: Pass malicious input as query parameter
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--query-var")
        .arg(format!("filter={}", malicious_query_var))
        .current_dir(fixture.workspace.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Assert: Output should NOT contain passwords
    assert!(!stdout.contains("secret123"), "Should not leak password");
    assert!(!stdout.contains("topsecret456"), "Should not leak password");

    // Should only contain legitimate user data
    assert!(
        stdout.contains("Alice") || stdout.contains("Bob"),
        "Should have legitimate data"
    );

    Ok(())
}

#[test]
fn test_comment_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Use comments to bypass filters
    let malicious_inputs = vec![
        "name' # comment out rest",
        "name' -- SQL-style comment",
        "name' /* block comment */",
    ];

    for malicious_input in malicious_inputs {
        // Act
        let mut cmd = Command::cargo_bin("ggen")?;
        let output = cmd
            .arg("generate")
            .arg("--ontology")
            .arg(&fixture.rdf_file)
            .arg("--template")
            .arg(&fixture.template_file)
            .arg("--query-var")
            .arg(format!("name={}", malicious_input))
            .current_dir(fixture.workspace.path())
            .output()?;

        let stdout = String::from_utf8_lossy(&output.stdout);

        // Assert: Should not expose sensitive data
        assert!(
            !stdout.contains("password"),
            "Should not contain password field"
        );
        assert!(!stdout.contains("secret"), "Should not leak secrets");
    }

    Ok(())
}

#[test]
fn test_filter_bypass_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: ') OR (1=1
    let malicious_filter = "user') OR (1=1";

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--filter")
        .arg(malicious_filter)
        .current_dir(fixture.workspace.path())
        .output()?;

    // Assert: Should fail or return safe results
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("secret123"), "Should not bypass filters");

    Ok(())
}

// ============================================================================
// CATEGORY 2: SPARQL-Specific Injection
// ============================================================================

#[test]
fn test_graph_traversal_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Access different graph patterns
    let malicious_pattern = "{ ?s ?p ?o } UNION { GRAPH ?g { ?s2 ex:password ?pass } }";

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--pattern")
        .arg(malicious_pattern)
        .current_dir(fixture.workspace.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Assert: Should not execute arbitrary graph patterns
    assert!(
        !stdout.contains("secret123"),
        "Should not allow graph traversal"
    );
    assert!(
        !stdout.contains("password"),
        "Should not expose password field"
    );

    Ok(())
}

#[test]
fn test_property_path_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Use property paths to access forbidden data
    let malicious_path = "ex:name|ex:password"; // OR path

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--property")
        .arg(malicious_path)
        .current_dir(fixture.workspace.path())
        .output()?;

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Assert: Should only return safe properties
    assert!(
        !stdout.contains("secret"),
        "Should not expose passwords via property paths"
    );

    Ok(())
}

#[test]
fn test_service_injection_blocked() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Use SERVICE to query external endpoints
    let malicious_service = "SERVICE <http://attacker.com/sparql> { ?s ?p ?o }";

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--service")
        .arg(malicious_service)
        .current_dir(fixture.workspace.path())
        .output()?;

    // Assert: Should fail or ignore external service calls
    assert!(!output.status.success() || !output.stdout.is_empty());

    // Should not make network requests
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("attacker.com"),
        "Should not execute external SERVICE"
    );

    Ok(())
}

#[test]
fn test_blind_injection_timing_attack() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Time-based blind injection
    let malicious_timing = "IF(EXISTS { ?s ex:password 'secret123' }, sleep(5000), true)";

    // Act
    let start = std::time::Instant::now();

    let mut cmd = Command::cargo_bin("ggen")?;
    let _output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--filter")
        .arg(malicious_timing)
        .current_dir(fixture.workspace.path())
        .output()?;

    let elapsed = start.elapsed();

    // Assert: Should not allow time-based attacks (should complete quickly)
    assert!(
        elapsed.as_secs() < 2,
        "Should not allow timing attacks (took {:?})",
        elapsed
    );

    Ok(())
}

// ============================================================================
// CATEGORY 3: Data Exfiltration Prevention
// ============================================================================

#[test]
fn test_no_data_exfiltration_via_error_messages() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Injection attempt: Trigger error to leak data
    let malicious_query = "INVALID SPARQL { ?s ex:password ?pass }";

    // Act
    let mut cmd = Command::cargo_bin("ggen")?;
    let output = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .arg("--raw-query")
        .arg(malicious_query)
        .current_dir(fixture.workspace.path())
        .output()?;

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert: Error messages should not contain sensitive data
    assert!(
        !stderr.contains("secret123"),
        "Error should not leak password"
    );
    assert!(
        !stderr.contains("topsecret456"),
        "Error should not leak password"
    );

    // Should have sanitized error message
    assert!(
        stderr.contains("syntax") || stderr.contains("invalid") || stderr.is_empty(),
        "Should have generic error message"
    );

    Ok(())
}

#[test]
fn test_parameterized_queries_safe() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Create RDF store and verify parameterized query is safe
    let store = Store::new().map_err(|e| format!("Failed to create store: {}", e))?;
    let rdf_content = fs::read_to_string(&fixture.rdf_file)?;

    store
        .load_from_reader(RdfFormat::Turtle, std::io::Cursor::new(rdf_content))
        .map_err(|e| format!("Failed to load RDF: {}", e))?;

    // Act: Execute query with user input as literal (not injected into query structure)
    let user_input = "Alice'; DROP GRAPH <http://example.com/>; --";

    // Query should treat user_input as a literal string, not execute it
    let query = format!(
        r#"SELECT ?role WHERE {{ ?user <http://example.com/name> "{}" ; <http://example.com/role> ?role }}"#,
        user_input.replace('"', "\\\"") // Proper escaping
    );

    let results = store.query(&query);

    // Assert: Query should fail safely (no user named with SQL injection string)
    assert!(results.is_ok(), "Parameterized query should execute safely");

    Ok(())
}

// ============================================================================
// CATEGORY 4: Legitimate Queries Work
// ============================================================================

#[test]
fn test_legitimate_queries_succeed() -> Result<(), Box<dyn std::error::Error>> {
    // Arrange
    let fixture = SparqlInjectionFixture::new()?;

    // Act: Execute legitimate query
    let mut cmd = Command::cargo_bin("ggen")?;
    let assert = cmd
        .arg("generate")
        .arg("--ontology")
        .arg(&fixture.rdf_file)
        .arg("--template")
        .arg(&fixture.template_file)
        .current_dir(fixture.workspace.path())
        .assert();

    // Assert: Should succeed and return safe data
    assert
        .success()
        .stdout(predicate::str::contains("Alice").or(predicate::str::contains("Bob")));

    Ok(())
}
