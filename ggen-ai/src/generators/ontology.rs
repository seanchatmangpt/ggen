//! AI-powered ontology generator

use crate::client::{LlmClient, LlmConfig};
use crate::error::{GgenAiError, Result};
use futures::StreamExt;
use ggen_core::Graph;

/// AI-powered ontology generator
#[derive(Debug)]
pub struct OntologyGenerator {
    client: Box<dyn LlmClient>,
    config: LlmConfig,
}

impl OntologyGenerator {
    /// Create a new ontology generator
    pub fn new(client: Box<dyn LlmClient>) -> Self {
        Self {
            client,
            config: LlmConfig::default(),
        }
    }
    
    /// Create a new ontology generator with custom config
    pub fn with_config(client: Box<dyn LlmClient>, config: LlmConfig) -> Self {
        Self { client, config }
    }
    
    /// Create a new ontology generator optimized for Ollama qwen3-coder:30b
    pub fn with_ollama_qwen3_coder(client: Box<dyn LlmClient>) -> Self {
        use crate::providers::OllamaClient;
        Self {
            client,
            config: OllamaClient::qwen3_coder_config(),
        }
    }
    
    /// Generate an ontology from domain description
    pub async fn generate_ontology(
        &self,
        domain: &str,
        requirements: Vec<&str>,
    ) -> Result<String> {
        let prompt = self.build_ontology_prompt(domain, requirements)?;
        
        let response = self.client.complete(&prompt).await?;
        
        // Extract and validate the ontology
        self.extract_and_validate_ontology(&response.content).await
    }
    
    /// Generate a domain-specific ontology
    pub async fn generate_domain_ontology(
        &self,
        domain: &str,
        entities: Vec<&str>,
        relationships: Vec<&str>,
    ) -> Result<String> {
        let prompt = self.build_domain_ontology_prompt(domain, entities, relationships)?;
        
        let response = self.client.complete(&prompt).await?;
        
        self.extract_and_validate_ontology(&response.content).await
    }
    
    /// Generate an ontology with specific patterns
    pub async fn generate_pattern_ontology(
        &self,
        domain: &str,
        patterns: Vec<&str>,
    ) -> Result<String> {
        let prompt = self.build_pattern_ontology_prompt(domain, patterns)?;
        
        let response = self.client.complete(&prompt).await?;
        
        self.extract_and_validate_ontology(&response.content).await
    }
    
    /// Build ontology generation prompt
    fn build_ontology_prompt(&self, domain: &str, requirements: Vec<&str>) -> Result<String> {
        let mut prompt = String::new();
        
        prompt.push_str("You are an expert ontology engineer. Generate a comprehensive RDF/OWL ontology ");
        prompt.push_str("in Turtle format based on the domain description and requirements.\n\n");
        
        prompt.push_str("## Domain Description\n");
        prompt.push_str(domain);
        prompt.push_str("\n\n");
        
        if !requirements.is_empty() {
            prompt.push_str("## Requirements\n");
            for req in requirements {
                prompt.push_str(&format!("- {}\n", req));
            }
            prompt.push_str("\n");
        }
        
        prompt.push_str("## Ontology Structure\n");
        prompt.push_str("Generate an ontology that includes:\n");
        prompt.push_str("1. Class definitions with rdfs:Class\n");
        prompt.push_str("2. Property definitions with rdf:Property\n");
        prompt.push_str("3. Domain and range specifications\n");
        prompt.push_str("4. Subclass relationships\n");
        prompt.push_str("5. Object and datatype properties\n");
        prompt.push_str("6. Appropriate namespaces and prefixes\n\n");
        
        prompt.push_str("## Example Structure\n");
        prompt.push_str("```turtle\n");
        prompt.push_str("@prefix : <http://example.org/ontology#> .\n");
        prompt.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        prompt.push_str("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n");
        prompt.push_str("@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\n");
        prompt.push_str(":Person a rdfs:Class .\n");
        prompt.push_str(":hasName a rdf:Property ;\n");
        prompt.push_str("    rdfs:domain :Person ;\n");
        prompt.push_str("    rdfs:range rdfs:Literal .\n");
        prompt.push_str("```\n\n");
        
        prompt.push_str("Generate the complete ontology now:\n\n");
        
        Ok(prompt)
    }
    
    /// Build domain-specific ontology prompt
    fn build_domain_ontology_prompt(
        &self,
        domain: &str,
        entities: Vec<&str>,
        relationships: Vec<&str>,
    ) -> Result<String> {
        let mut prompt = String::new();
        
        prompt.push_str("You are an expert ontology engineer. Generate a domain-specific RDF/OWL ontology ");
        prompt.push_str("in Turtle format for the specified domain with the given entities and relationships.\n\n");
        
        prompt.push_str("## Domain\n");
        prompt.push_str(domain);
        prompt.push_str("\n\n");
        
        if !entities.is_empty() {
            prompt.push_str("## Entities\n");
            for entity in entities {
                prompt.push_str(&format!("- {}\n", entity));
            }
            prompt.push_str("\n");
        }
        
        if !relationships.is_empty() {
            prompt.push_str("## Relationships\n");
            for relationship in relationships {
                prompt.push_str(&format!("- {}\n", relationship));
            }
            prompt.push_str("\n");
        }
        
        prompt.push_str("## Ontology Requirements\n");
        prompt.push_str("1. Define each entity as a class\n");
        prompt.push_str("2. Define each relationship as a property\n");
        prompt.push_str("3. Specify domain and range for properties\n");
        prompt.push_str("4. Add appropriate subclass relationships\n");
        prompt.push_str("5. Include data properties for attributes\n");
        prompt.push_str("6. Use clear, descriptive names\n");
        prompt.push_str("7. Follow OWL best practices\n\n");
        
        prompt.push_str("Generate the complete ontology now:\n\n");
        
        Ok(prompt)
    }
    
    /// Build pattern-based ontology prompt
    fn build_pattern_ontology_prompt(
        &self,
        domain: &str,
        patterns: Vec<&str>,
    ) -> Result<String> {
        let mut prompt = String::new();
        
        prompt.push_str("You are an expert ontology engineer. Generate an RDF/OWL ontology ");
        prompt.push_str("in Turtle format that implements the specified design patterns.\n\n");
        
        prompt.push_str("## Domain\n");
        prompt.push_str(domain);
        prompt.push_str("\n\n");
        
        if !patterns.is_empty() {
            prompt.push_str("## Design Patterns\n");
            for pattern in patterns {
                prompt.push_str(&format!("- {}\n", pattern));
            }
            prompt.push_str("\n");
        }
        
        prompt.push_str("## Common Ontology Patterns\n");
        prompt.push_str("1. **Class Hierarchy**: Use rdfs:subClassOf for inheritance\n");
        prompt.push_str("2. **Property Chains**: Use owl:propertyChainAxiom\n");
        prompt.push_str("3. **Disjoint Classes**: Use owl:disjointWith\n");
        prompt.push_str("4. **Functional Properties**: Use owl:FunctionalProperty\n");
        prompt.push_str("5. **Inverse Properties**: Use owl:inverseOf\n");
        prompt.push_str("6. **Transitive Properties**: Use owl:TransitiveProperty\n");
        prompt.push_str("7. **Symmetric Properties**: Use owl:SymmetricProperty\n");
        prompt.push_str("8. **Reflexive Properties**: Use owl:ReflexiveProperty\n\n");
        
        prompt.push_str("Generate the complete ontology implementing these patterns:\n\n");
        
        Ok(prompt)
    }
    
    /// Extract and validate ontology from LLM response
    async fn extract_and_validate_ontology(&self, content: &str) -> Result<String> {
        // Extract ontology from markdown code blocks if present
        let ontology = if content.contains("```turtle") && content.contains("```") {
            let start = content.find("```turtle").unwrap_or(0);
            let end = content.rfind("```").unwrap_or(content.len());
            content[start + 9..end].trim().to_string()
        } else if content.contains("```") {
            let start = content.find("```").unwrap_or(0);
            let end = content.rfind("```").unwrap_or(content.len());
            content[start + 3..end].trim().to_string()
        } else {
            content.trim().to_string()
        };
        
        // Validate the ontology by loading it into a graph
        let graph = Graph::new()?;
        graph.insert_turtle(&ontology)
            .map_err(|e| GgenAiError::ontology_generation(format!("Invalid Turtle syntax: {}", e)))?;
        
        // Check for basic ontology structure
        if !ontology.contains("rdfs:Class") && !ontology.contains("rdf:Property") {
            return Err(GgenAiError::ontology_generation(
                "Ontology must contain at least one class or property definition"
            ));
        }
        
        Ok(ontology)
    }
    
    /// Stream ontology generation for long-running operations
    pub async fn stream_generate_ontology(
        &self,
        domain: &str,
        requirements: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = self.build_ontology_prompt(domain, requirements)?;
        
        let stream = self.client.stream_complete(&prompt).await?;
        
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
    async fn test_ontology_generator_creation() {
        let client = Box::new(MockClient::with_response("Generated ontology"));
        let generator = OntologyGenerator::new(client);
        
        assert_eq!(generator.client().provider_name(), "mock");
    }
    
    #[tokio::test]
    async fn test_ontology_generator_with_config() {
        let client = Box::new(MockClient::with_response("Generated ontology"));
        let config = LlmConfig {
            model: "test-model".to_string(),
            temperature: Some(0.2),
            ..Default::default()
        };
        let generator = OntologyGenerator::with_config(client, config);
        
        assert_eq!(generator.config().model, "test-model");
        assert_eq!(generator.config().temperature, Some(0.2));
    }
    
    #[tokio::test]
    async fn test_generate_ontology() {
        let mock_ontology = r#"@prefix : <http://example.org/ontology#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:Person a rdfs:Class .
:hasName a rdf:Property ;
    rdfs:domain :Person ;
    rdfs:range rdfs:Literal ."#;
        
        let client = Box::new(MockClient::with_response(mock_ontology));
        let generator = OntologyGenerator::new(client);
        
        let result = generator.generate_ontology(
            "Person management system",
            vec!["Include name property"]
        ).await;
        
        // This should succeed with a valid ontology
        assert!(result.is_ok());
        let ontology = result.expect("Failed to generate ontology");
        assert!(ontology.contains(":Person"));
        assert!(ontology.contains(":hasName"));
    }
    
    #[tokio::test]
    async fn test_generate_domain_ontology() {
        let client = Box::new(MockClient::with_response("Domain ontology"));
        let generator = OntologyGenerator::new(client);
        
        let result = generator.generate_domain_ontology(
            "E-commerce",
            vec!["Product", "Customer", "Order"],
            vec!["hasProduct", "placesOrder", "containsItem"]
        ).await;
        
        // This will fail because we can't create a real ontology without proper validation
        // But it demonstrates the API
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_stream_generate_ontology() {
        let client = Box::new(MockClient::with_response("Streamed ontology"));
        let generator = OntologyGenerator::new(client);
        
        let mut stream = generator.stream_generate_ontology(
            "Test domain",
            vec!["Test requirement"]
        ).await.expect("Failed to create ontology stream");

        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            content.push_str(&chunk.expect("Failed to read chunk from ontology stream"));
        }

        assert_eq!(content, "Streamed ontology");
    }
}
