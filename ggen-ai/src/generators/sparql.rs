//! AI-powered SPARQL query generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::prompts::{SparqlPromptBuilder, SparqlPrompts};
use futures::StreamExt;
use ggen_core::Graph;
use oxigraph::sparql::Query;

/// AI-powered SPARQL query generator
pub struct SparqlGenerator {
    client: Box<dyn LlmClient>,
    config: LlmConfig,
}

impl SparqlGenerator {
    /// Create a new SPARQL generator
    pub fn new(client: Box<dyn LlmClient>) -> Self {
        Self {
            client,
            config: LlmConfig::default(),
        }
    }
    
    /// Create a new SPARQL generator with custom config
    pub fn with_config(client: Box<dyn LlmClient>, config: LlmConfig) -> Self {
        Self { client, config }
    }
    
    /// Generate a SPARQL query from natural language intent
    pub async fn generate_query(
        &self,
        graph: &Graph,
        intent: &str,
    ) -> Result<String> {
        // Analyze graph schema
        let schema = self.analyze_graph_schema(graph).await?;
        let prefixes = self.extract_prefixes(graph).await?;
        
        let prompt = SparqlPromptBuilder::new(intent.to_string())
            .with_schema(schema)
            .with_prefixes(prefixes)
            .build()?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        // Extract and validate the query
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find all instances of a class
    pub async fn generate_find_instances(
        &self,
        graph: &Graph,
        class_uri: &str,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_instances(class_uri, prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find properties of a resource
    pub async fn generate_find_properties(
        &self,
        graph: &Graph,
        resource_uri: &str,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_properties(resource_uri, prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find relationships between resources
    pub async fn generate_find_relationships(
        &self,
        graph: &Graph,
        subject_uri: &str,
        object_uri: &str,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_relationships(subject_uri, object_uri, prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find resources by property value
    pub async fn generate_find_by_property_value(
        &self,
        graph: &Graph,
        property_uri: &str,
        value: &str,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_by_property_value(property_uri, value, prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find resources with missing properties
    pub async fn generate_find_missing_properties(
        &self,
        graph: &Graph,
        class_uri: &str,
        property_uri: &str,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_missing_properties(class_uri, property_uri, prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Generate a query to find the most connected resources
    pub async fn generate_find_most_connected(
        &self,
        graph: &Graph,
    ) -> Result<String> {
        let prefixes = self.extract_prefixes(graph).await?;
        let prompt = SparqlPrompts::find_most_connected(prefixes)?;
        
        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;
        
        self.extract_and_validate_query(&response.content, graph).await
    }
    
    /// Analyze graph schema to provide context for query generation
    async fn analyze_graph_schema(&self, graph: &Graph) -> Result<String> {
        // Get all classes
        let classes_query = r#"
            SELECT DISTINCT ?class WHERE {
                ?s a ?class .
            }
            ORDER BY ?class
        "#;
        
        // Get all properties
        let properties_query = r#"
            SELECT DISTINCT ?property WHERE {
                ?s ?property ?o .
            }
            ORDER BY ?property
        "#;
        
        // Get all subjects
        let subjects_query = r#"
            SELECT DISTINCT ?subject WHERE {
                ?subject ?p ?o .
            }
            ORDER BY ?subject
            LIMIT 10
        "#;
        
        let mut schema = String::new();
        
        // Execute queries and build schema description
        if let Ok(classes_result) = graph.query(classes_query) {
            schema.push_str("Classes found:\n");
            match classes_result {
                oxigraph::sparql::QueryResults::Solutions(solutions) => {
                    for solution in solutions {
                        if let Ok(solution) = solution {
                            if let Some(class) = solution.get("class") {
                                schema.push_str(&format!("- {}\n", class));
                            }
                        }
                    }
                }
                _ => {}
            }
            schema.push_str("\n");
        }
        
        if let Ok(properties_result) = graph.query(properties_query) {
            schema.push_str("Properties found:\n");
            match properties_result {
                oxigraph::sparql::QueryResults::Solutions(solutions) => {
                    for solution in solutions {
                        if let Ok(solution) = solution {
                            if let Some(property) = solution.get("property") {
                                schema.push_str(&format!("- {}\n", property));
                            }
                        }
                    }
                }
                _ => {}
            }
            schema.push_str("\n");
        }
        
        if let Ok(subjects_result) = graph.query(subjects_query) {
            schema.push_str("Sample subjects:\n");
            match subjects_result {
                oxigraph::sparql::QueryResults::Solutions(solutions) => {
                    for solution in solutions {
                        if let Ok(solution) = solution {
                            if let Some(subject) = solution.get("subject") {
                                schema.push_str(&format!("- {}\n", subject));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        Ok(schema)
    }
    
    /// Extract prefixes from the graph
    async fn extract_prefixes(&self, graph: &Graph) -> Result<Vec<(String, String)>> {
        // Common prefixes to look for
        let common_prefixes = vec![
            ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
            ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
            ("owl", "http://www.w3.org/2002/07/owl#"),
            ("xsd", "http://www.w3.org/2001/XMLSchema#"),
            ("foaf", "http://xmlns.com/foaf/0.1/"),
            ("dc", "http://purl.org/dc/elements/1.1/"),
            ("dcterms", "http://purl.org/dc/terms/"),
            ("skos", "http://www.w3.org/2004/02/skos/core#"),
        ];
        
        // Check which prefixes are actually used in the graph
        let mut used_prefixes = Vec::new();
        
        for (prefix, uri) in common_prefixes {
            let check_query = format!(
                "ASK WHERE {{ ?s ?p ?o . FILTER(STRSTARTS(STR(?s), \"{}\") || STRSTARTS(STR(?p), \"{}\") || STRSTARTS(STR(?o), \"{}\")) }}",
                uri, uri, uri
            );
            
            if let Ok(result) = graph.query(&check_query) {
                if let oxigraph::sparql::QueryResults::Boolean(true) = result {
                    used_prefixes.push((prefix.to_string(), uri.to_string()));
                }
            }
        }
        
        Ok(used_prefixes)
    }
    
    /// Extract and validate SPARQL query from LLM response
    async fn extract_and_validate_query(&self, content: &str, graph: &Graph) -> Result<String> {
        // Extract query from markdown code blocks if present
        let query = if content.contains("```sparql") && content.contains("```") {
            let start = content.find("```sparql").unwrap_or(0);
            let end = content.rfind("```").unwrap_or(content.len());
            content[start + 9..end].trim().to_string()
        } else if content.contains("```") {
            let start = content.find("```").unwrap_or(0);
            let end = content.rfind("```").unwrap_or(content.len());
            content[start + 3..end].trim().to_string()
        } else {
            content.trim().to_string()
        };
        
        // Validate the query syntax
        Query::parse(&query, None)
            .map_err(|e| GgenAiError::sparql_generation(format!("Invalid SPARQL syntax: {}", e)))?;
        
        // Test the query against the graph
        graph.query(&query)
            .map_err(|e| GgenAiError::sparql_generation(format!("Query execution failed: {}", e)))?;
        
        Ok(query)
    }
    
    /// Stream query generation for long-running operations
    pub async fn stream_generate_query(
        &self,
        graph: &Graph,
        intent: &str,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let schema = self.analyze_graph_schema(graph).await?;
        let prefixes = self.extract_prefixes(graph).await?;
        
        let prompt = SparqlPromptBuilder::new(intent.to_string())
            .with_schema(schema)
            .with_prefixes(prefixes)
            .build()?;
        
        let stream = self.client.stream_complete(&prompt, Some(self.config.clone())).await?;
        
        Ok(Box::pin(stream.map(|chunk_result| {
            chunk_result.map(|chunk| chunk.content)
        })))
    }
    
    /// Get the LLM client
    pub fn client(&self) -> &dyn LlmClient {
        self.client.as_ref()
    }
    
    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        &self.config
    }
    
    /// Update the configuration
    pub fn set_config(&mut self, config: LlmConfig) {
        self.config = config;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;
    
    #[tokio::test]
    async fn test_sparql_generator_creation() {
        let client = Box::new(MockClient::with_response("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"));
        let generator = SparqlGenerator::new(client);
        
        assert_eq!(generator.client().provider_name(), "mock");
    }
    
    #[tokio::test]
    async fn test_sparql_generator_with_config() {
        let client = Box::new(MockClient::with_response("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"));
        let config = LlmConfig {
            model: "test-model".to_string(),
            temperature: Some(0.3),
            ..Default::default()
        };
        let generator = SparqlGenerator::with_config(client, config);
        
        assert_eq!(generator.config().model, "test-model");
        assert_eq!(generator.config().temperature, Some(0.3));
    }
    
    #[tokio::test]
    async fn test_generate_query() {
        let client = Box::new(MockClient::with_response("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"));
        let generator = SparqlGenerator::new(client);
        
        // Create a mock graph
        let graph = ggen_core::Graph::new().unwrap();
        graph.insert_turtle("@prefix ex: <http://example.org/> . ex:test a ex:Class .").unwrap();
        
        let result = generator.generate_query(&graph, "Find all triples").await;
        
        // This should succeed with a valid SPARQL query
        assert!(result.is_ok());
        let query = result.unwrap();
        assert!(query.contains("SELECT"));
    }
    
    #[tokio::test]
    async fn test_stream_generate_query() {
        let client = Box::new(MockClient::with_response("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"));
        let generator = SparqlGenerator::new(client);
        
        let graph = ggen_core::Graph::new().unwrap();
        graph.insert_turtle("@prefix ex: <http://example.org/> . ex:test a ex:Class .").unwrap();
        
        let mut stream = generator.stream_generate_query(&graph, "Find all triples").await.unwrap();
        
        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            content.push_str(&chunk.unwrap());
        }
        
        assert!(content.contains("SELECT"));
    }
}
