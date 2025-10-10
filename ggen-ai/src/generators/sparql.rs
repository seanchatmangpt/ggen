//! AI-powered SPARQL query generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::prompts::SparqlPromptBuilder;
use futures::StreamExt;
use ggen_core::Graph;
use oxigraph::sparql::Query;
use serde::{Deserialize, Serialize};

/// JSON representation of a SPARQL query for AI generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlQueryJson {
    /// Query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
    pub query_type: String,
    /// Variables to select (for SELECT queries)
    pub variables: Vec<String>,
    /// WHERE clause patterns
    pub where_clause: Vec<TriplePattern>,
    /// Optional ORDER BY clause
    pub order_by: Option<Vec<OrderByClause>>,
    /// Optional LIMIT clause
    pub limit: Option<u32>,
    /// Optional OFFSET clause
    pub offset: Option<u32>,
    /// FILTER conditions
    pub filters: Vec<String>,
}

/// Represents a triple pattern in the WHERE clause
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TriplePattern {
    /// Subject (can be variable like ?s or URI)
    pub subject: String,
    /// Predicate (can be variable like ?p or URI)
    pub predicate: String,
    /// Object (can be variable like ?o, literal, or URI)
    pub object: String,
}

/// ORDER BY clause specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderByClause {
    /// Variable to order by
    pub variable: String,
    /// Direction (ASC or DESC)
    pub direction: String,
}

/// AI-powered SPARQL query generator
#[derive(Debug)]
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
    
    /// Create a new SPARQL generator optimized for Ollama qwen3-coder:30b
    pub fn with_ollama_qwen3_coder(client: Box<dyn LlmClient>) -> Self {
        use crate::providers::OllamaClient;
        Self {
            client,
            config: OllamaClient::qwen3_coder_config(),
        }
    }
    
    /// Generate a SPARQL query from natural language intent
    pub async fn generate_query(
        &self,
        graph: &Graph,
        intent: &str,
    ) -> Result<String> {
        self.generate_query_from_json(graph, intent).await
    }
    
    /// Generate a query to find all instances of a class
    pub async fn generate_find_instances(
        &self,
        graph: &Graph,
        class_uri: &str,
    ) -> Result<String> {
        let intent = format!("Find all instances of the class {}", class_uri);
        self.generate_query_from_json(graph, &intent).await
    }
    
    /// Generate a query to find properties of a resource
    pub async fn generate_find_properties(
        &self,
        graph: &Graph,
        resource_uri: &str,
    ) -> Result<String> {
        let intent = format!("Find all properties and values for the resource {}", resource_uri);
        self.generate_query_from_json(graph, &intent).await
    }

    /// Generate a query to find relationships between resources
    pub async fn generate_find_relationships(
        &self,
        graph: &Graph,
        subject_uri: &str,
        object_uri: &str,
    ) -> Result<String> {
        let intent = format!("Find all relationships between {} and {}", subject_uri, object_uri);
        self.generate_query_from_json(graph, &intent).await
    }

    /// Generate a query to find resources by property value
    pub async fn generate_find_by_property_value(
        &self,
        graph: &Graph,
        property_uri: &str,
        value: &str,
    ) -> Result<String> {
        let intent = format!("Find all resources where {} has value {}", property_uri, value);
        self.generate_query_from_json(graph, &intent).await
    }
    
    /// Generate a query to find resources with missing properties
    pub async fn generate_find_missing_properties(
        &self,
        graph: &Graph,
        class_uri: &str,
        property_uri: &str,
    ) -> Result<String> {
        let intent = format!("Find all instances of {} that are missing the {} property", class_uri, property_uri);
        self.generate_query_from_json(graph, &intent).await
    }

    /// Generate a query to find the most connected resources
    pub async fn generate_find_most_connected(
        &self,
        graph: &Graph,
    ) -> Result<String> {
        let intent = "Find the most connected resources (resources with the most relationships)";
        self.generate_query_from_json(graph, intent).await
    }
    
    /// Convert JSON representation to SPARQL string
    fn json_to_sparql(&self, json_query: &SparqlQueryJson, prefixes: &[(String, String)]) -> String {
        let mut sparql = String::new();

        // Add prefixes
        for (prefix, uri) in prefixes {
            sparql.push_str(&format!("PREFIX {}: <{}>\n", prefix, uri));
        }
        if !prefixes.is_empty() {
            sparql.push('\n');
        }

        // Query type and variables
        match json_query.query_type.to_uppercase().as_str() {
            "SELECT" => {
                sparql.push_str("SELECT ");
                if json_query.variables.is_empty() {
                    sparql.push_str("*");
                } else {
                    sparql.push_str(&json_query.variables.join(" "));
                }
                sparql.push_str("\nWHERE {\n");
            }
            "ASK" => {
                sparql.push_str("ASK\nWHERE {\n");
            }
            "CONSTRUCT" => {
                sparql.push_str("CONSTRUCT {\n");
                // For CONSTRUCT, we would need additional fields for the construct template
                // For now, assume it's a simple case
                sparql.push_str("}\nWHERE {\n");
            }
            "DESCRIBE" => {
                sparql.push_str("DESCRIBE ");
                sparql.push_str(&json_query.variables.join(" "));
                sparql.push_str("\nWHERE {\n");
            }
            _ => {
                // Default to SELECT
                sparql.push_str("SELECT *\nWHERE {\n");
            }
        }

        // WHERE clause
        for pattern in &json_query.where_clause {
            sparql.push_str(&format!("    {} {} {} .\n", pattern.subject, pattern.predicate, pattern.object));
        }

        sparql.push_str("}\n");

        // FILTER conditions
        for filter in &json_query.filters {
            if !filter.trim().is_empty() {
                sparql.push_str(&format!("FILTER ({})\n", filter));
            }
        }

        // ORDER BY clause
        if let Some(order_by) = &json_query.order_by {
            if !order_by.is_empty() {
                sparql.push_str("ORDER BY ");
                let clauses: Vec<String> = order_by.iter()
                    .map(|clause| format!("{}({})", clause.direction, clause.variable))
                    .collect();
                sparql.push_str(&clauses.join(" "));
                sparql.push('\n');
            }
        }

        // LIMIT clause
        if let Some(limit) = json_query.limit {
            sparql.push_str(&format!("LIMIT {}\n", limit));
        }

        // OFFSET clause
        if let Some(offset) = json_query.offset {
            sparql.push_str(&format!("OFFSET {}\n", offset));
        }

        sparql
    }

    /// Generate SPARQL query from JSON format
    async fn generate_query_from_json(
        &self,
        graph: &Graph,
        intent: &str,
    ) -> Result<String> {
        // Analyze graph schema
        let schema = self.analyze_graph_schema(graph).await?;
        let prefixes = self.extract_prefixes(graph).await?;

        // Create prompt that requests JSON format
        let prompt = format!(
            "You are an expert SPARQL query generator. I need you to generate a SPARQL query and return it as a JSON object.

Graph Schema:
{}

Query Intent: {}

IMPORTANT: Respond ONLY with a valid JSON object in this exact format:
{{
  \"query_type\": \"SELECT\",
  \"variables\": [\"?s\", \"?p\", \"?o\"],
  \"where_clause\": [
    {{\"subject\": \"?s\", \"predicate\": \"?p\", \"object\": \"?o\"}}
  ],
  \"filters\": [],
  \"order_by\": [],
  \"limit\": null,
  \"offset\": null
}}

Do not include any markdown formatting, explanations, or additional text. Only the JSON object.",
            schema, intent
        );

        let response = self.client.complete(&prompt, Some(self.config.clone())).await?;

        // Parse JSON response
        let json_query: SparqlQueryJson = serde_json::from_str(&response.content)
            .map_err(|e| GgenAiError::sparql_generation(format!("Invalid JSON response: {}", e)))?;

        // Convert to SPARQL
        let sparql = self.json_to_sparql(&json_query, &prefixes);

        // Validate the generated SPARQL
        Query::parse(&sparql, None)
            .map_err(|e| GgenAiError::sparql_generation(format!("Generated SPARQL syntax error: {}", e)))?;

        // Test the query against the graph
        graph.query(&sparql)
            .map_err(|e| GgenAiError::sparql_generation(format!("Generated query execution failed: {}", e)))?;

        Ok(sparql)
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
    
    /// Stream query generation for long-running operations
    pub async fn stream_generate_query(
        &self,
        graph: &Graph,
        intent: &str,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        // For streaming, we still use the text-based approach since JSON conversion
        // needs the complete response. In the future, this could be enhanced
        // to stream JSON parsing and conversion.
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

    /// Convert JSON SPARQL representation back to SPARQL query string
    pub fn json_to_sparql_with_prefixes(&self, json_query: &SparqlQueryJson) -> Result<String> {
        let mut query = String::new();

        // Add query type and variables for SELECT queries
        match json_query.query_type.to_uppercase().as_str() {
            "SELECT" => {
                if json_query.variables.is_empty() {
                    query.push_str("SELECT *");
                } else {
                    query.push_str(&format!("SELECT {}", json_query.variables.join(" ")));
                }
            }
            "ASK" => query.push_str("ASK"),
            "CONSTRUCT" => query.push_str("CONSTRUCT"),
            "DESCRIBE" => query.push_str("DESCRIBE"),
            _ => return Err(GgenAiError::sparql_generation(format!("Unknown query type: {}", json_query.query_type))),
        }

        query.push_str("\nWHERE {\n");

        // Add WHERE clause patterns
        for pattern in &json_query.where_clause {
            query.push_str(&format!("    {} {} {} .\n", pattern.subject, pattern.predicate, pattern.object));
        }

        query.push_str("}\n");

        // Add filters
        for filter in &json_query.filters {
            query.push_str(&format!("FILTER({})\n", filter));
        }

        // Add ORDER BY clause
        if let Some(order_clauses) = &json_query.order_by {
            query.push_str("ORDER BY ");
            let order_parts: Vec<String> = order_clauses
                .iter()
                .map(|clause| format!("{}({})", clause.direction.to_uppercase(), clause.variable))
                .collect();
            query.push_str(&order_parts.join(" "));
            query.push('\n');
        }

        // Add LIMIT and OFFSET
        if let Some(limit) = json_query.limit {
            query.push_str(&format!("LIMIT {}\n", limit));
        }

        if let Some(offset) = json_query.offset {
            query.push_str(&format!("OFFSET {}\n", offset));
        }

        Ok(query)
    }

    /// Parse JSON string to SparqlQueryJson and convert to SPARQL
    pub fn json_string_to_sparql(&self, json_str: &str) -> Result<String> {
        let json_query: SparqlQueryJson = serde_json::from_str(json_str)
            .map_err(|e| GgenAiError::sparql_generation(format!("Invalid JSON: {}", e)))?;

        Ok(self.json_to_sparql(&json_query, &[]))
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
        let client = Box::new(MockClient::with_response(r#"{"query_type":"SELECT","variables":["?s","?p","?o"],"where_clause":[{"subject":"?s","predicate":"?p","object":"?o"}],"filters":[],"order_by":[],"limit":null,"offset":null}"#));
        let generator = SparqlGenerator::new(client);

        // Create a mock graph
        let graph = ggen_core::Graph::new().expect("Failed to create graph");
        graph.insert_turtle("@prefix ex: <http://example.org/> . ex:test a ex:Class .")
            .expect("Failed to insert turtle data");

        let result = generator.generate_query(&graph, "Find all triples").await;

        // This should succeed with a valid SPARQL query
        assert!(result.is_ok());
        let query = result.expect("Failed to generate SPARQL query");
        assert!(query.contains("SELECT"));
    }
    
    #[tokio::test]
    async fn test_stream_generate_query() {
        let client = Box::new(MockClient::with_response(r#"{"query_type":"SELECT","variables":["?s","?p","?o"],"where_clause":[{"subject":"?s","predicate":"?p","object":"?o"}],"filters":[],"order_by":[],"limit":null,"offset":null}"#));
        let generator = SparqlGenerator::new(client);
        
        let graph = ggen_core::Graph::new().expect("Failed to create graph");
        graph.insert_turtle("@prefix ex: <http://example.org/> . ex:test a ex:Class .")
            .expect("Failed to insert turtle data");

        let mut stream = generator.stream_generate_query(&graph, "Find all triples")
            .await.expect("Failed to create SPARQL query stream");

        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            content.push_str(&chunk.expect("Failed to read chunk from SPARQL stream"));
        }

        assert!(content.contains("SELECT"));
    }
}
