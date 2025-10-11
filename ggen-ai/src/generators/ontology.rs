//! AI-powered ontology generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::Result;
use crate::prompts::OntologyPromptBuilder;
use futures::StreamExt;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// JSON representation of graph evolution operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEvolution {
    /// Operations to perform on the graph
    pub operations: Vec<GraphOperation>,
    /// Reasoning for the changes
    pub reasoning: String,
    /// Confidence score for the evolution
    pub confidence: f64,
}

/// Individual graph operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphOperation {
    /// Operation type
    pub operation_type: OperationType,
    /// Subject URI
    pub subject: String,
    /// Predicate URI
    pub predicate: String,
    /// Object (URI, literal, or variable)
    pub object: String,
    /// Optional operation metadata
    pub metadata: Option<OperationMetadata>,
}

/// Types of graph operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    /// Add a new triple
    Add,
    /// Remove a triple
    Remove,
    /// Modify an existing triple
    Modify,
    /// Add a class definition
    AddClass,
    /// Add a property definition
    AddProperty,
    /// Add a subclass relationship
    AddSubclass,
}

/// Operation metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationMetadata {
    /// Human-readable description
    pub description: String,
    /// Impact assessment
    pub impact: String,
    /// Validation requirements
    pub validation: Vec<String>,
}

/// AI-powered ontology generator
#[derive(Debug)]
pub struct OntologyGenerator {
    client: Arc<dyn LlmClient>,
}

impl OntologyGenerator {
    /// Create a new ontology generator
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with custom config
    pub fn with_config(client: Arc<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with a client
    pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Generate an ontology from a natural language description
    pub async fn generate_ontology(&self, domain: &str, requirements: Vec<&str>) -> Result<String> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let response = self.client.complete(&prompt).await?;

        // Extract ontology content from response
        self.extract_ontology_content(&response.content)
    }

    /// Stream ontology generation from a natural language description
    pub async fn stream_ontology(
        &self, domain: &str, requirements: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let stream = self.client.complete_stream(&prompt).await?;

        Ok(Box::pin(stream.map(|chunk| Ok(chunk.content))))
    }

    /// Get the LLM client
    pub fn client(&self) -> &Arc<dyn LlmClient> {
        &self.client
    }

    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    /// Update the configuration
    pub fn set_config(&mut self, _config: LlmConfig) {
        // Note: This would require mutable access to the client
        // For now, we'll skip this functionality
    }

    /// Extract ontology content from AI response
    fn extract_ontology_content(&self, response: &str) -> Result<String> {
        // Look for Turtle/RDF content in code blocks with "turtle" language marker
        if let Some(start) = response.find("```turtle") {
            let search_start = start + 9;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                return Ok(content.to_string());
            }
        }

        // Look for code blocks with "ttl" language marker
        if let Some(start) = response.find("```ttl") {
            let search_start = start + 6;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                return Ok(content.to_string());
            }
        }

        // Look for code blocks with "rdf" language marker
        if let Some(start) = response.find("```rdf") {
            let search_start = start + 6;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                return Ok(content.to_string());
            }
        }

        // Look for any code block that might contain RDF
        if let Some(start) = response.find("```") {
            // Skip the opening ```
            let mut search_start = start + 3;

            // Skip language identifier if present (e.g., "```python" -> skip "python")
            if let Some(newline_pos) = response[search_start..].find('\n') {
                search_start += newline_pos + 1;
            }

            if let Some(end_offset) = response[search_start..].find("```") {
                let content = response[search_start..search_start + end_offset].trim();
                // Check if it looks like RDF/Turtle with more robust detection
                if content.contains("@prefix")
                    || (content.contains(" a ") && content.contains(";"))
                    || (content.contains("rdfs:") || content.contains("owl:") || content.contains("rdf:")) {
                    return Ok(content.to_string());
                }
            }
        }

        // Check if the entire response looks like Turtle without code blocks
        let trimmed = response.trim();
        if trimmed.contains("@prefix")
            || (trimmed.contains(" a ") && trimmed.contains(";"))
            || (trimmed.contains("rdfs:") || trimmed.contains("owl:") || trimmed.contains("rdf:")) {
            return Ok(trimmed.to_string());
        }

        // If we couldn't find valid Turtle content, return a more helpful error
        Err(crate::error::GgenAiError::ontology_generation(
            format!(
                "No Turtle code block found in response. Please ensure the LLM provider returns Turtle/RDF in a code block. Response preview: {}",
                &response[..response.len().min(200)]
            )
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_ontology_generation() {
        let client = MockClient::with_response("```turtle\n@prefix ex: <http://example.org/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\nex:Person a rdf:Class .\n```");
        let generator = OntologyGenerator::new(Arc::new(client));

        let ontology = generator
            .generate_ontology(
                "Person management",
                vec!["Include Person class", "Use RDF/OWL"],
            )
            .await
            .unwrap();

        assert!(ontology.contains("@prefix"));
        assert!(ontology.contains("Person"));
    }

    /// Evolve an existing graph based on natural language requirements
    pub async fn evolve_graph(
        &self, graph: &Graph, requirements: &str, context: Option<&str>,
    ) -> Result<GraphEvolution> {
        // Analyze current graph structure
        let schema = self.analyze_graph_for_evolution(graph).await?;
        let current_stats = self.get_graph_statistics(graph).await?;

        // Create evolution prompt
        let prompt = format!(
            "You are an expert knowledge engineer. I need you to analyze an RDF graph and propose specific evolution operations based on new requirements.

Current Graph Statistics:
- Classes: {}
- Properties: {}
- Triples: {}

Current Schema:
{}

New Requirements: {}

Please respond with a JSON object that specifies exactly which operations to perform on the graph. Each operation should be a specific addition, modification, or removal of triples.

JSON Format:
{{
  \"operations\": [
    {{
      \"operation_type\": \"Add|Remove|Modify|AddClass|AddProperty\",
      \"subject\": \"URI or variable\",
      \"predicate\": \"URI or variable\",
      \"object\": \"URI, literal, or variable\",
      \"metadata\": {{
        \"description\": \"Human-readable description\",
        \"impact\": \"Impact assessment\",
        \"validation\": [\"validation requirement 1\", \"requirement 2\"]
      }}
    }}
  ],
  \"reasoning\": \"Explanation of why these changes are needed\",
  \"confidence\": 0.0-1.0
}}

Return only the JSON object.",
            current_stats.classes, current_stats.properties, current_stats.triples, schema, requirements
        );

        let response = self
            .client
            .complete(&prompt, Some(self.config.clone()))
            .await?;

        // Parse JSON response
        let evolution: GraphEvolution = serde_json::from_str(&response.content).map_err(|e| {
            GgenAiError::ontology_generation(format!("Invalid evolution JSON: {}", e))
        })?;

        // Validate evolution operations
        self.validate_evolution_operations(&evolution, graph)
            .await?;

        Ok(evolution)
    }

    /// Apply graph evolution operations to a graph
    pub async fn apply_evolution(
        &self, graph: &mut Graph, evolution: &GraphEvolution,
    ) -> Result<()> {
        for operation in &evolution.operations {
            match operation.operation_type {
                OperationType::Add => {
                    // Add the triple
                    graph.insert_turtle(&format!(
                        "{} {} {} .",
                        operation.subject, operation.predicate, operation.object
                    ))?;
                }
                OperationType::Remove => {
                    // Remove the triple (would need more sophisticated logic)
                    // For now, this is a placeholder
                }
                OperationType::Modify => {
                    // Modify existing triple
                    // This would require finding and replacing existing triples
                }
                OperationType::AddClass => {
                    // Add class definition
                    let class_def = format!(
                        "{} a owl:Class ; rdfs:label \"{}\" .",
                        operation.subject,
                        operation.object.replace("\"", "")
                    );
                    graph.insert_turtle(&class_def)?;
                }
                OperationType::AddProperty => {
                    // Add property definition
                    let prop_def = format!(
                        "{} a owl:DatatypeProperty ; rdfs:domain {} ; rdfs:range {} .",
                        operation.subject, operation.predicate, operation.object
                    );
                    graph.insert_turtle(&prop_def)?;
                }
                OperationType::AddSubclass => {
                    // Add subclass relationship
                    let subclass_def = format!(
                        "{} rdfs:subClassOf {} .",
                        operation.subject, operation.object
                    );
                    graph.insert_turtle(&subclass_def)?;
                }
            }
        }

        Ok(())
    }

    /// Validate evolution operations before applying them
    async fn validate_evolution_operations(
        &self, evolution: &GraphEvolution, graph: &Graph,
    ) -> Result<()> {
        // Check confidence threshold
        if evolution.confidence < 0.7 {
            return Err(GgenAiError::ontology_generation(format!(
                "Evolution confidence too low: {:.2}",
                evolution.confidence
            )));
        }

        // Validate each operation
        for operation in &evolution.operations {
            match operation.operation_type {
                OperationType::Add | OperationType::Remove | OperationType::Modify => {
                    // Basic URI validation
                    if !operation.subject.starts_with('<') && !operation.subject.starts_with('?') {
                        return Err(GgenAiError::ontology_generation(format!(
                            "Invalid subject URI: {}",
                            operation.subject
                        )));
                    }
                    if !operation.predicate.starts_with('<')
                        && !operation.predicate.starts_with('?')
                    {
                        return Err(GgenAiError::ontology_generation(format!(
                            "Invalid predicate URI: {}",
                            operation.predicate
                        )));
                    }
                }
                _ => {} // Class/property operations are handled differently
            }
        }

        // Check for potential conflicts (simplified)
        let operation_count = evolution.operations.len();
        if operation_count > 50 {
            return Err(GgenAiError::ontology_generation(
                "Too many operations in single evolution (max 50)",
            ));
        }

        Ok(())
    }

    /// Analyze graph for evolution context
    async fn analyze_graph_for_evolution(&self, graph: &Graph) -> Result<String> {
        let classes_query = r#"
            SELECT DISTINCT ?class WHERE {
                ?s a ?class .
            }
            ORDER BY ?class
        "#;

        let properties_query = r#"
            SELECT DISTINCT ?property WHERE {
                ?s ?property ?o .
            }
            ORDER BY ?property
        "#;

        let classes = graph.query(classes_query)?;
        let properties = graph.query(properties_query)?;

        let mut schema = String::new();
        schema.push_str("Classes:\n");

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = classes {
            for solution in solutions.take(20) {
                if let Ok(solution) = solution {
                    if let Some(class) = solution.get("class") {
                        schema.push_str(&format!("- {}\n", class));
                    }
                }
            }
        }

        schema.push_str("\nProperties:\n");
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = properties {
            for solution in solutions.take(20) {
                if let Ok(solution) = solution {
                    if let Some(property) = solution.get("property") {
                        schema.push_str(&format!("- {}\n", property));
                    }
                }
            }
        }

        Ok(schema)
    }

    /// Get basic graph statistics
    async fn get_graph_statistics(&self, graph: &Graph) -> Result<GraphStats> {
        let classes_query = "SELECT (COUNT(DISTINCT ?class) as ?count) WHERE { ?s a ?class }";
        let properties_query =
            "SELECT (COUNT(DISTINCT ?property) as ?count) WHERE { ?s ?property ?o }";
        let triples_query = "SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }";

        let classes_count = self.extract_count(graph, classes_query).await?;
        let properties_count = self.extract_count(graph, properties_query).await?;
        let triples_count = self.extract_count(graph, triples_query).await?;

        Ok(GraphStats {
            classes: classes_count,
            properties: properties_count,
            triples: triples_count,
        })
    }

    /// Extract count from SPARQL query result
    async fn extract_count(&self, graph: &Graph, query: &str) -> Result<usize> {
        let results = graph.query(query)?;
        if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
            if let Some(solution) = solutions.next() {
                if let Ok(solution) = solution {
                    if let Some(count_binding) = solution.get("count") {
                        if let Some(count_str) = count_binding.to_string().strip_prefix("\"") {
                            if let Some(count_str) = count_str.strip_suffix("\"") {
                                return Ok(count_str.parse().unwrap_or(0));
                            }
                        }
                    }
                }
            }
        }
        Ok(0)
    }
}

/// Graph statistics for evolution context
#[derive(Debug)]
struct GraphStats {
    classes: usize,
    properties: usize,
    triples: usize,
}
