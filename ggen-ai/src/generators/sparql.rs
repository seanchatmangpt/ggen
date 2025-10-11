//! AI-powered SPARQL query generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use crate::prompts::SparqlPromptBuilder;
use futures::StreamExt;
use ggen_core::Graph;
use serde_json::Value;
use std::sync::Arc;

/// AI-powered SPARQL query generator
#[derive(Debug)]
pub struct SparqlGenerator {
    client: Arc<dyn LlmClient>,
}

impl SparqlGenerator {
    /// Create a new SPARQL generator
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new SPARQL generator with custom config
    pub fn with_config(client: Arc<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new SPARQL generator with a client
    pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }
    
    /// Create a new SPARQL generator optimized for Ollama qwen3-coder:30b
    pub fn with_ollama_qwen3_coder(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Generate a SPARQL query from a natural language description
    pub async fn generate_query(&self, graph: &Graph, intent: &str) -> Result<String> {
        // Build prompt with graph schema and intent
        let prompt = SparqlPromptBuilder::new(intent.to_string())
            .with_schema("Graph schema not available".to_string())
            .with_prefixes(vec![])
            .build()?;

        let response = self.client.complete(&prompt).await?;
        
        // Extract SPARQL query from response
        self.extract_sparql_query(&response.content)
    }

    /// Stream SPARQL query generation from a natural language description
    pub async fn stream_sparql(
        &self,
        graph: &Graph,
        intent: &str,
        prefixes: &[(&str, &str)],
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prefix_vec: Vec<(String, String)> = prefixes.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect();
        let prompt = SparqlPromptBuilder::new(intent.to_string())
            .with_schema(format!("Graph with {} triples", graph.len()))
            .with_prefixes(prefix_vec)
            .build()?;

        let stream = self.client.complete_stream(&prompt).await?;

        Ok(Box::pin(stream.map(|chunk| Ok(chunk.content))))
    }

    /// Get the LLM client
    pub fn client(&self) -> &dyn LlmClient {
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

    /// Extract SPARQL query from AI response
    fn extract_sparql_query(&self, response: &str) -> Result<String> {
        // Look for SPARQL query in code blocks
        if let Some(start) = response.find("```sparql") {
            let search_start = start + 9;
            if let Some(end_offset) = response[search_start..].find("```") {
                let query = &response[search_start..search_start + end_offset].trim();
                return Ok(query.to_string());
            }
        }

        // Look for SPARQL query in any code block
        if let Some(start) = response.find("```") {
            let search_start = start + 3;
            if let Some(end_offset) = response[search_start..].find("```") {
                let query = &response[search_start..search_start + end_offset].trim();
                // Check if it looks like SPARQL
                if query.to_uppercase().contains("SELECT") ||
                   query.to_uppercase().contains("CONSTRUCT") ||
                   query.to_uppercase().contains("ASK") ||
                   query.to_uppercase().contains("DESCRIBE") {
                    return Ok(query.to_string());
                }
            }
        }

        // Fallback: return the entire response
        Ok(response.trim().to_string())
    }

    /// Convert JSON to SPARQL query
    pub fn json_to_sparql(json: &Value, prefixes: &[(&str, &str)]) -> Result<String> {
        let mut query = String::new();
        
        // Add prefixes
        for (prefix, uri) in prefixes {
            query.push_str(&format!("PREFIX {}: <{}>\n", prefix, uri));
        }
        query.push('\n');
        
        // Extract query type and variables
        if let Some(query_type) = json.get("type").and_then(|v| v.as_str()) {
            match query_type {
                "select" => {
                    query.push_str("SELECT ");
                    if let Some(vars) = json.get("variables").and_then(|v| v.as_array()) {
                        let var_list: Vec<String> = vars.iter()
                            .filter_map(|v| v.as_str())
                            .map(|s| format!("?{}", s))
                            .collect();
                        query.push_str(&var_list.join(" "));
                    } else {
                        query.push('*');
                    }
                    query.push_str(" WHERE {\n");
                    
                    // Add WHERE clause
                    if let Some(where_clause) = json.get("where").and_then(|v| v.as_str()) {
                        query.push_str(&format!("  {}\n", where_clause));
                    }
                    
                    query.push('}');
                }
                "construct" => {
                    query.push_str("CONSTRUCT {\n");
                    if let Some(template) = json.get("template").and_then(|v| v.as_str()) {
                        query.push_str(&format!("  {}\n", template));
                    }
                    query.push_str("} WHERE {\n");
                    
                    if let Some(where_clause) = json.get("where").and_then(|v| v.as_str()) {
                        query.push_str(&format!("  {}\n", where_clause));
                    }
                    
                    query.push('}');
                }
                "ask" => {
                    query.push_str("ASK WHERE {\n");
                    if let Some(where_clause) = json.get("where").and_then(|v| v.as_str()) {
                        query.push_str(&format!("  {}\n", where_clause));
                    }
                    query.push('}');
                }
                _ => {
                    return Err(GgenAiError::sparql_generation(
                        format!("Unsupported query type: {}", query_type)
                    ));
                }
            }
        } else {
            return Err(GgenAiError::sparql_generation(
                "Missing query type in JSON".to_string()
            ));
        }
        
        Ok(query)
    }

    /// Convert JSON to SPARQL query with prefixes
    pub fn json_to_sparql_with_prefixes(json: &Value, prefixes: &[(&str, &str)]) -> Result<String> {
        Self::json_to_sparql(json, prefixes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::adapter::MockClient;
    use serde_json::json;

    #[tokio::test]
    async fn test_sparql_generation() {
        let client = MockClient::with_response("```sparql\nSELECT ?name WHERE {\n  ?person foaf:name ?name .\n}\n```");
        let generator = SparqlGenerator::new(Arc::new(client));
        
        let graph = Graph::new().unwrap();
        let query = generator.generate_query(&graph, "Find all person names").await.unwrap();
        
        assert!(query.contains("SELECT"));
        assert!(query.contains("WHERE"));
    }

    #[test]
    fn test_json_to_sparql() {
        let json = json!({
            "type": "select",
            "variables": ["name"],
            "where": "?person foaf:name ?name ."
        });
        
        let prefixes = [("foaf", "http://xmlns.com/foaf/0.1/")];
        let query = SparqlGenerator::json_to_sparql(&json, &prefixes).unwrap();
        
        assert!(query.contains("PREFIX foaf:"));
        assert!(query.contains("SELECT ?name"));
        assert!(query.contains("WHERE"));
    }
}