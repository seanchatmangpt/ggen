//! AI-powered ontology generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::Result;
use crate::prompts::{OntologyPromptBuilder, OntologyPrompts};
use futures::StreamExt;

/// AI-powered ontology generator
#[derive(Debug)]
pub struct OntologyGenerator {
    client: Box<dyn LlmClient>,
}

impl OntologyGenerator {
    /// Create a new ontology generator
    pub fn new(client: Box<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with custom config
    pub fn with_config(client: Box<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with a client
    pub fn with_client(client: Box<dyn LlmClient>) -> Self {
        Self { client }
    }
    
    /// Create a new ontology generator optimized for Ollama qwen3-coder:30b
    pub fn with_ollama_qwen3_coder(client: Box<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Generate an ontology from a natural language description
    pub async fn generate_ontology(
        &self,
        domain: &str,
        requirements: Vec<&str>,
    ) -> Result<String> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let response = self.client.complete(&prompt).await?;
        
        // Extract ontology content from response
        self.extract_ontology_content(&response.content)
    }

    /// Stream ontology generation from a natural language description
    pub async fn stream_ontology(
        &self,
        domain: &str,
        requirements: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let stream = self.client.complete_stream(&prompt).await?;

        Ok(Box::pin(stream.map(|chunk| Ok(chunk.content))))
    }

    /// Get the LLM client
    pub fn client(&self) -> &dyn LlmClient {
        &*self.client
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
        // Look for Turtle/RDF content in code blocks
        if let Some(start) = response.find("```turtle") {
            let search_start = start + 9;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                return Ok(content.to_string());
            }
        }

        if let Some(start) = response.find("```ttl") {
            let search_start = start + 6;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                return Ok(content.to_string());
            }
        }

        // Look for any code block that might contain RDF
        if let Some(start) = response.find("```") {
            let search_start = start + 3;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();
                // Check if it looks like RDF/Turtle
                if content.contains("@prefix") || content.contains("a ") || content.contains(":") {
                    return Ok(content.to_string());
                }
            }
        }

        // Fallback: return the entire response
        Ok(response.trim().to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_ontology_generation() {
        let client = MockClient::with_response("```turtle\n@prefix ex: <http://example.org/> .\n@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\nex:Person a rdf:Class .\n```");
        let generator = OntologyGenerator::new(Box::new(client));
        
        let ontology = generator.generate_ontology(
            "Person management",
            vec!["Include Person class", "Use RDF/OWL"]
        ).await.unwrap();
        
        assert!(ontology.contains("@prefix"));
        assert!(ontology.contains("Person"));
    }
}