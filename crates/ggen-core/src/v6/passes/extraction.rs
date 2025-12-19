//! μ₂: Extraction Pass
//!
//! Performs ontology → bindings transformation using SELECT queries.
//! Extracts template variables from the RDF graph.

use crate::v6::pass::{Pass, PassContext, PassResult, PassType};
use ggen_utils::error::{Error, Result};
use oxigraph::sparql::QueryResults;
use serde::{Deserialize, Serialize};
use std::time::Instant;

/// An extraction rule that produces template bindings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractionRule {
    /// Rule name for auditing
    pub name: String,

    /// SPARQL SELECT query
    pub query: String,

    /// Key to store results under in bindings
    pub binding_key: String,

    /// Description for documentation
    pub description: Option<String>,
}

/// μ₂: Extraction pass implementation
#[derive(Debug, Clone)]
pub struct ExtractionPass {
    /// Rules to execute
    rules: Vec<ExtractionRule>,
}

impl ExtractionPass {
    /// Create a new extraction pass
    pub fn new() -> Self {
        Self { rules: Vec::new() }
    }

    /// Add an extraction rule
    pub fn add_rule(&mut self, rule: ExtractionRule) {
        self.rules.push(rule);
    }

    /// Create with a set of rules
    pub fn with_rules(mut self, rules: Vec<ExtractionRule>) -> Self {
        self.rules = rules;
        self
    }

    /// Clean a SPARQL term for template use
    fn clean_term(term: &str) -> String {
        if term.starts_with('<') && term.ends_with('>') {
            // IRI: strip angle brackets
            term[1..term.len() - 1].to_string()
        } else if let Some(without_prefix) = term.strip_prefix('"') {
            // Literal: strip quotes and optional datatype/language
            if let Some(quote_end) = without_prefix.find('"') {
                without_prefix[..quote_end].to_string()
            } else {
                term.to_string()
            }
        } else {
            term.to_string()
        }
    }

    /// Execute a single extraction rule
    fn execute_rule(
        &self, ctx: &PassContext<'_>, rule: &ExtractionRule,
    ) -> Result<serde_json::Value> {
        let results = ctx.graph.query(&rule.query).map_err(|e| {
            Error::new(&format!(
                "Extraction query '{}' failed: {}",
                rule.name, e
            ))
        })?;

        match results {
            QueryResults::Solutions(solutions) => {
                let mut rows = Vec::new();

                for solution in solutions {
                    let solution = solution
                        .map_err(|e| Error::new(&format!("SPARQL solution error: {}", e)))?;

                    let mut row = serde_json::Map::new();
                    for (var, term) in solution.iter() {
                        let var_name = var.to_string();
                        let clean_value = Self::clean_term(&term.to_string());
                        row.insert(var_name, serde_json::Value::String(clean_value));
                    }
                    rows.push(serde_json::Value::Object(row));
                }

                Ok(serde_json::Value::Array(rows))
            }
            QueryResults::Boolean(b) => Ok(serde_json::Value::Bool(b)),
            QueryResults::Graph(_) => Err(Error::new(&format!(
                "Extraction rule '{}' must use SELECT, not CONSTRUCT",
                rule.name
            ))),
        }
    }
}

impl Default for ExtractionPass {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for ExtractionPass {
    fn pass_type(&self) -> PassType {
        PassType::Extraction
    }

    fn name(&self) -> &str {
        "μ₂:extraction"
    }

    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();

        for rule in &self.rules {
            let value = self.execute_rule(ctx, rule)?;
            ctx.bindings.insert(rule.binding_key.clone(), value);
        }

        let duration = start.elapsed();
        Ok(PassResult::success().with_duration(duration))
    }
}

/// Standard extraction queries for v6
impl ExtractionPass {
    /// Create a pass with standard v6 extraction rules
    pub fn with_standard_rules() -> Self {
        let mut pass = Self::new();

        // Extract all classes
        pass.add_rule(ExtractionRule {
            name: "extract-classes".to_string(),
            query: r#"
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX owl: <http://www.w3.org/2002/07/owl#>

                SELECT DISTINCT ?class ?label ?comment ?superClass
                WHERE {
                    { ?class a rdfs:Class . }
                    UNION
                    { ?class a owl:Class . }
                    OPTIONAL { ?class rdfs:label ?label . }
                    OPTIONAL { ?class rdfs:comment ?comment . }
                    OPTIONAL { ?class rdfs:subClassOf ?superClass . }
                }
                ORDER BY ?class
            "#.to_string(),
            binding_key: "classes".to_string(),
            description: Some("Extract all class definitions".to_string()),
        });

        // Extract all properties
        pass.add_rule(ExtractionRule {
            name: "extract-properties".to_string(),
            query: r#"
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX owl: <http://www.w3.org/2002/07/owl#>

                SELECT DISTINCT ?property ?label ?domain ?range ?comment
                WHERE {
                    { ?property a rdf:Property . }
                    UNION
                    { ?property a owl:ObjectProperty . }
                    UNION
                    { ?property a owl:DatatypeProperty . }
                    OPTIONAL { ?property rdfs:label ?label . }
                    OPTIONAL { ?property rdfs:domain ?domain . }
                    OPTIONAL { ?property rdfs:range ?range . }
                    OPTIONAL { ?property rdfs:comment ?comment . }
                }
                ORDER BY ?property
            "#.to_string(),
            binding_key: "properties".to_string(),
            description: Some("Extract all property definitions".to_string()),
        });

        // Extract all instances
        pass.add_rule(ExtractionRule {
            name: "extract-instances".to_string(),
            query: r#"
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                PREFIX owl: <http://www.w3.org/2002/07/owl#>

                SELECT DISTINCT ?instance ?type ?label
                WHERE {
                    ?instance rdf:type ?type .
                    FILTER (?type != rdfs:Class)
                    FILTER (?type != owl:Class)
                    FILTER (?type != rdf:Property)
                    FILTER (?type != owl:ObjectProperty)
                    FILTER (?type != owl:DatatypeProperty)
                    OPTIONAL { ?instance rdfs:label ?label . }
                }
                ORDER BY ?type ?instance
            "#.to_string(),
            binding_key: "instances".to_string(),
            description: Some("Extract all named instances".to_string()),
        });

        pass
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use std::path::PathBuf;

    #[test]
    fn test_extraction_pass_empty() {
        let graph = Graph::new().unwrap();
        let pass = ExtractionPass::new();

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
    }

    #[test]
    fn test_extraction_with_rule() {
        let graph = Graph::new().unwrap();
        graph.insert_turtle(r#"
            @prefix ex: <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            ex:Person a rdfs:Class ;
                rdfs:label "Person" .
        "#).unwrap();

        let mut pass = ExtractionPass::new();
        pass.add_rule(ExtractionRule {
            name: "get-classes".to_string(),
            query: r#"
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                SELECT ?class ?label WHERE {
                    ?class a rdfs:Class .
                    OPTIONAL { ?class rdfs:label ?label . }
                }
            "#.to_string(),
            binding_key: "classes".to_string(),
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert!(ctx.bindings.contains_key("classes"));

        let classes = ctx.bindings.get("classes").unwrap();
        assert!(classes.is_array());
        assert!(!classes.as_array().unwrap().is_empty());
    }

    #[test]
    fn test_clean_term() {
        assert_eq!(
            ExtractionPass::clean_term("<http://example.org/Person>"),
            "http://example.org/Person"
        );
        assert_eq!(
            ExtractionPass::clean_term("\"Hello World\""),
            "Hello World"
        );
        assert_eq!(
            ExtractionPass::clean_term("\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>"),
            "42"
        );
    }
}
