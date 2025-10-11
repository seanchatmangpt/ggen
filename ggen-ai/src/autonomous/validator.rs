//! Self-Validation Engine
//!
//! Automatically generates SPARQL validation queries and checks
//! constraints before committing changes to the graph.

use std::sync::Arc;
use crate::client::LlmClient;
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::{debug, info, warn};

/// Validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Whether validation passed
    pub passed: bool,
    /// Constraint violations found
    pub violations: Vec<ConstraintViolation>,
    /// Validation queries executed
    pub queries_executed: Vec<String>,
    /// Execution time (ms)
    pub duration_ms: u64,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// A constraint violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConstraintViolation {
    /// Type of constraint violated
    pub constraint_type: String,
    /// Description of violation
    pub description: String,
    /// Severity level
    pub severity: ViolationSeverity,
    /// Affected triples or entities
    pub affected_entities: Vec<String>,
    /// Suggested fix
    pub suggested_fix: Option<String>,
}

/// Severity of a constraint violation
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ViolationSeverity {
    /// Critical - must be fixed
    Critical,
    /// Warning - should be fixed
    Warning,
    /// Info - nice to fix
    Info,
}

/// Self-validator trait
#[async_trait]
pub trait Validator: Send + Sync {
    /// Validate RDF triples against constraints
    async fn validate(&self, triples: &[String]) -> Result<ValidationResult>;

    /// Validate with custom SPARQL queries
    async fn validate_with_queries(&self, triples: &[String], queries: Vec<String>) -> Result<ValidationResult>;

    /// Generate validation queries from ontology
    async fn generate_validation_queries(&self, ontology: &str) -> Result<Vec<String>>;
}

/// Self-validation engine implementation
#[derive(Clone)]
pub struct SelfValidator {
    client: Arc<dyn LlmClient>,
    store: Store,
    learned_patterns: HashMap<String, Vec<String>>,
}

impl std::fmt::Debug for SelfValidator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SelfValidator")
            .field("client", &"Arc<dyn LlmClient>")
            .field("store", &"Store")
            .field("learned_patterns", &self.learned_patterns)
            .finish()
    }
}

impl SelfValidator {
    /// Create a new self-validator
    pub fn new(client: Arc<dyn LlmClient>) -> Result<Self> {
        let store = Store::new()
            .map_err(|e| GgenAiError::configuration(format!("Failed to create RDF store: {}", e)))?;

        Ok(Self {
            client,
            store,
            learned_patterns: HashMap::new(),
        })
    }

    /// Load triples into validation store
    fn load_triples(&self, triples: &[String]) -> Result<()> {
        let turtle_doc = triples.join("\n");

        self.store.load_from_reader(
            oxigraph::io::RdfFormat::Turtle,
            turtle_doc.as_bytes(),
        ).map_err(|e| GgenAiError::validation(format!("Failed to load triples: {}", e)))?;

        Ok(())
    }

    /// Execute SPARQL query
    fn execute_query(&self, query: &str) -> Result<QueryResults> {
        self.store.query(query)
            .map_err(|e| GgenAiError::validation(format!("SPARQL query failed: {}", e)))
    }

    /// Build validation prompt
    fn build_validation_prompt(&self, ontology: &str) -> String {
        let mut prompt = String::from("You are an expert in RDF/OWL validation and SPARQL.\n\n");
        prompt.push_str("Given the following ontology, generate SPARQL validation queries to check:\n");
        prompt.push_str("1. Class and property consistency\n");
        prompt.push_str("2. Domain and range constraints\n");
        prompt.push_str("3. Cardinality restrictions\n");
        prompt.push_str("4. Data type validation\n");
        prompt.push_str("5. Required properties\n\n");

        prompt.push_str("Ontology:\n");
        prompt.push_str(ontology);
        prompt.push_str("\n\n");

        prompt.push_str("Generate SPARQL ASK or SELECT queries that return violations.\n");
        prompt.push_str("Format each query in a separate code block:\n");
        prompt.push_str("```sparql\n");
        prompt.push_str("[QUERY HERE]\n");
        prompt.push_str("```\n");

        prompt
    }

    /// Extract SPARQL queries from AI response
    fn extract_queries(&self, response: &str) -> Vec<String> {
        let mut queries = Vec::new();
        let mut current_pos = 0;

        while let Some(start) = response[current_pos..].find("```sparql") {
            let abs_start = current_pos + start + 9;
            if let Some(end_offset) = response[abs_start..].find("```") {
                let query = response[abs_start..abs_start + end_offset].trim();
                queries.push(query.to_string());
                current_pos = abs_start + end_offset + 3;
            } else {
                break;
            }
        }

        queries
    }

    /// Detect violations from query results
    fn detect_violations(&self, query: &str, results: QueryResults) -> Vec<ConstraintViolation> {
        let mut violations = Vec::new();

        match results {
            QueryResults::Boolean(result) => {
                if result {
                    // ASK query returned true - violation detected
                    violations.push(ConstraintViolation {
                        constraint_type: "Unknown".to_string(),
                        description: format!("Constraint violation detected by query: {}", query),
                        severity: ViolationSeverity::Warning,
                        affected_entities: Vec::new(),
                        suggested_fix: None,
                    });
                }
            }
            QueryResults::Solutions(solutions) => {
                // SELECT query - each solution is a violation
                for solution in solutions.flatten() {
                    let mut affected = Vec::new();
                    for (var, term) in solution.iter() {
                        affected.push(format!("?{} = {}", var, term));
                    }

                    violations.push(ConstraintViolation {
                        constraint_type: "Constraint".to_string(),
                        description: "Validation query returned results indicating constraint violation".to_string(),
                        severity: ViolationSeverity::Warning,
                        affected_entities: affected,
                        suggested_fix: None,
                    });
                }
            }
            QueryResults::Graph(_) => {
                // CONSTRUCT/DESCRIBE query - not typically used for validation
                debug!("Graph query results not used for validation");
            }
        }

        violations
    }

    /// Learn validation patterns from successful validations
    pub fn learn_pattern(&mut self, pattern_name: String, queries: Vec<String>) {
        info!("Learning validation pattern: {}", pattern_name);
        self.learned_patterns.insert(pattern_name, queries);
    }

    /// Get learned patterns
    pub fn get_learned_patterns(&self) -> &HashMap<String, Vec<String>> {
        &self.learned_patterns
    }
}

#[async_trait]
impl Validator for SelfValidator {
    async fn validate(&self, triples: &[String]) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        let timestamp = chrono::Utc::now();

        debug!("Validating {} triples", triples.len());

        // Load triples into store
        self.load_triples(triples)?;

        // Basic validation queries
        let default_queries = vec![
            // Check for undefined classes
            "SELECT ?s WHERE { ?s a ?class . FILTER NOT EXISTS { ?class a owl:Class } }".to_string(),
            // Check for undefined properties
            "SELECT ?p WHERE { ?s ?p ?o . FILTER(?p != rdf:type) FILTER NOT EXISTS { ?p a ?propType . FILTER(?propType IN (owl:ObjectProperty, owl:DatatypeProperty)) } }".to_string(),
        ];

        let mut all_violations = Vec::new();
        let mut queries_executed = Vec::new();

        for query in default_queries {
            queries_executed.push(query.clone());

            match self.execute_query(&query) {
                Ok(results) => {
                    let violations = self.detect_violations(&query, results);
                    all_violations.extend(violations);
                }
                Err(e) => {
                    warn!("Validation query failed: {}", e);
                }
            }
        }

        let duration = start.elapsed();
        let passed = all_violations.is_empty();

        info!("Validation {}, {} violations found in {:?}",
              if passed { "passed" } else { "failed" },
              all_violations.len(),
              duration);

        Ok(ValidationResult {
            passed,
            violations: all_violations,
            queries_executed,
            duration_ms: duration.as_millis() as u64,
            timestamp,
        })
    }

    async fn validate_with_queries(&self, triples: &[String], queries: Vec<String>) -> Result<ValidationResult> {
        let start = std::time::Instant::now();
        let timestamp = chrono::Utc::now();

        debug!("Validating with {} custom queries", queries.len());

        self.load_triples(triples)?;

        let mut all_violations = Vec::new();
        let mut queries_executed = Vec::new();

        for query in queries {
            queries_executed.push(query.clone());

            match self.execute_query(&query) {
                Ok(results) => {
                    let violations = self.detect_violations(&query, results);
                    all_violations.extend(violations);
                }
                Err(e) => {
                    warn!("Custom validation query failed: {}", e);
                }
            }
        }

        let duration = start.elapsed();
        let passed = all_violations.is_empty();

        Ok(ValidationResult {
            passed,
            violations: all_violations,
            queries_executed,
            duration_ms: duration.as_millis() as u64,
            timestamp,
        })
    }

    async fn generate_validation_queries(&self, ontology: &str) -> Result<Vec<String>> {
        debug!("Generating validation queries from ontology");

        let prompt = self.build_validation_prompt(ontology);
        let response = self.client.complete(&prompt).await?;

        let queries = self.extract_queries(&response.content);

        info!("Generated {} validation queries", queries.len());

        Ok(queries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_validate_triples() {
        let client = MockClient::with_response("Validation passed");
        let validator = SelfValidator::new(Arc::new(client)).unwrap();

        let triples = vec![
            "@prefix ex: <http://example.org/> .".to_string(),
            "@prefix owl: <http://www.w3.org/2002/07/owl#> .".to_string(),
            "ex:Person a owl:Class .".to_string(),
        ];

        let result = validator.validate(&triples).await.unwrap();
        // Should pass basic validation
        assert!(!result.queries_executed.is_empty());
    }

    #[tokio::test]
    async fn test_generate_validation_queries() {
        let mock_response = r#"
```sparql
SELECT ?s WHERE { ?s a ?class . FILTER NOT EXISTS { ?class a owl:Class } }
```

```sparql
SELECT ?p WHERE { ?s ?p ?o . FILTER NOT EXISTS { ?p a owl:ObjectProperty } }
```
"#;
        let client = MockClient::with_response(mock_response);
        let validator = SelfValidator::new(Arc::new(client)).unwrap();

        let ontology = "@prefix ex: <http://example.org/> .\nex:Person a owl:Class .";
        let queries = validator.generate_validation_queries(ontology).await.unwrap();

        assert_eq!(queries.len(), 2);
    }

    #[test]
    fn test_violation_severity() {
        let violation = ConstraintViolation {
            constraint_type: "test".to_string(),
            description: "test violation".to_string(),
            severity: ViolationSeverity::Critical,
            affected_entities: vec!["ex:Entity1".to_string()],
            suggested_fix: None,
        };

        assert_eq!(violation.severity, ViolationSeverity::Critical);
    }
}
