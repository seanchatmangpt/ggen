//! Natural Language to RDF Parser
//!
//! Converts natural language specifications, documentation, and traces
//! into RDF triples using AI inference.

use crate::client::LlmClient;
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info, warn};

/// Parsed RDF triples from natural language
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParsedTriples {
    /// Generated RDF triples in Turtle format
    pub triples: Vec<String>,
    /// Inferred relationships
    pub relations: Vec<InferredRelation>,
    /// Confidence scores (0.0 to 1.0)
    pub confidence: HashMap<String, f32>,
    /// Source text that was parsed
    pub source: String,
    /// Parsing metadata
    pub metadata: ParsingMetadata,
}

/// Inferred relationship from natural language
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferredRelation {
    /// Subject URI or blank node
    pub subject: String,
    /// Predicate URI
    pub predicate: String,
    /// Object URI, literal, or blank node
    pub object: String,
    /// Confidence score (0.0 to 1.0)
    pub confidence: f32,
    /// Reasoning for inference
    pub reasoning: String,
}

/// Metadata about the parsing process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParsingMetadata {
    /// Number of tokens in source text
    pub token_count: usize,
    /// Time taken for parsing (ms)
    pub duration_ms: u64,
    /// Model used for inference
    pub model: String,
    /// Timestamp of parsing
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Natural language parser trait
#[async_trait]
pub trait NlParser: Send + Sync {
    /// Parse natural language into RDF triples
    async fn parse_to_rdf(&self, text: &str) -> Result<ParsedTriples>;

    /// Parse with existing ontology context
    async fn parse_with_context(&self, text: &str, ontology_uri: &str) -> Result<ParsedTriples>;

    /// Batch parse multiple texts
    async fn batch_parse(&self, texts: Vec<&str>) -> Result<Vec<ParsedTriples>>;
}

/// Natural language to RDF parser implementation
#[derive(Debug)]
pub struct NaturalLanguageParser {
    client: Arc<dyn LlmClient>,
    base_uri: String,
    namespace_prefixes: HashMap<String, String>,
}

impl NaturalLanguageParser {
    /// Create a new NL parser
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self {
            client,
            base_uri: "http://example.org/".to_string(),
            namespace_prefixes: Self::default_prefixes(),
        }
    }

    /// Create parser with custom base URI
    pub fn with_base_uri(client: Arc<dyn LlmClient>, base_uri: String) -> Self {
        Self {
            client,
            base_uri,
            namespace_prefixes: Self::default_prefixes(),
        }
    }

    /// Add namespace prefix
    pub fn add_prefix(&mut self, prefix: String, uri: String) {
        self.namespace_prefixes.insert(prefix, uri);
    }

    /// Get default namespace prefixes
    fn default_prefixes() -> HashMap<String, String> {
        let mut prefixes = HashMap::new();
        prefixes.insert(
            "rdf".to_string(),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
        );
        prefixes.insert(
            "rdfs".to_string(),
            "http://www.w3.org/2000/01/rdf-schema#".to_string(),
        );
        prefixes.insert(
            "owl".to_string(),
            "http://www.w3.org/2002/07/owl#".to_string(),
        );
        prefixes.insert(
            "xsd".to_string(),
            "http://www.w3.org/2001/XMLSchema#".to_string(),
        );
        prefixes
    }

    /// Build parsing prompt
    fn build_prompt(&self, text: &str, ontology_context: Option<&str>) -> String {
        let mut prompt = String::from("You are an expert RDF/OWL knowledge engineer. ");
        prompt.push_str("Convert the following natural language description into RDF triples in Turtle format.\n\n");

        // Add namespace prefixes
        for (prefix, uri) in &self.namespace_prefixes {
            prompt.push_str(&format!("@prefix {}: <{}> .\n", prefix, uri));
        }
        prompt.push_str(&format!("@prefix ex: <{}> .\n\n", self.base_uri));

        // Add ontology context if provided
        if let Some(context) = ontology_context {
            prompt.push_str("Existing ontology context:\n");
            prompt.push_str(context);
            prompt.push_str("\n\n");
        }

        prompt.push_str("Natural language description:\n");
        prompt.push_str(text);
        prompt.push_str("\n\n");

        prompt.push_str("Requirements:\n");
        prompt.push_str("1. Generate valid Turtle RDF triples\n");
        prompt.push_str("2. Infer relationships and properties not explicitly stated\n");
        prompt.push_str(
            "3. Use appropriate RDF/OWL constructs (classes, properties, restrictions)\n",
        );
        prompt.push_str("4. Include rdfs:label and rdfs:comment for human readability\n");
        prompt.push_str("5. Provide confidence scores (0.0-1.0) for inferred relationships\n\n");

        prompt.push_str("Output format:\n");
        prompt.push_str("```turtle\n");
        prompt.push_str("[Your RDF triples here]\n");
        prompt.push_str("```\n\n");

        prompt.push_str("Then provide inferred relations in JSON format:\n");
        prompt.push_str("```json\n");
        prompt.push_str("[\n");
        prompt.push_str("  {\"subject\": \"ex:Entity1\", \"predicate\": \"rdf:type\", \"object\": \"owl:Class\", \"confidence\": 0.95, \"reasoning\": \"...\"}\n");
        prompt.push_str("]\n");
        prompt.push_str("```");

        prompt
    }

    /// Extract triples from AI response
    fn extract_triples(&self, response: &str) -> Result<Vec<String>> {
        // Find Turtle code block
        if let Some(start) = response.find("```turtle") {
            let search_start = start + 9;
            if let Some(end_offset) = response[search_start..].find("```") {
                let content = &response[search_start..search_start + end_offset].trim();

                // Parse individual triples
                let triples: Vec<String> = content
                    .lines()
                    .filter(|line| !line.trim().is_empty() && !line.trim().starts_with('@'))
                    .map(|s| s.trim().to_string())
                    .collect();

                return Ok(triples);
            }
        }

        Err(GgenAiError::ontology_generation(
            "No Turtle code block found in response",
        ))
    }

    /// Extract inferred relations from AI response
    fn extract_relations(&self, response: &str) -> Result<Vec<InferredRelation>> {
        // Find JSON code block
        if let Some(start) = response.find("```json") {
            let search_start = start + 7;
            if let Some(end_offset) = response[search_start..].find("```") {
                let json_str = &response[search_start..search_start + end_offset].trim();

                let relations: Vec<InferredRelation> =
                    serde_json::from_str(json_str).map_err(|e| {
                        GgenAiError::parse_error("NL Parser", format!("Invalid JSON: {}", e))
                    })?;

                return Ok(relations);
            }
        }

        // Return empty if no relations found
        Ok(Vec::new())
    }
}

#[async_trait]
impl NlParser for NaturalLanguageParser {
    async fn parse_to_rdf(&self, text: &str) -> Result<ParsedTriples> {
        let start = std::time::Instant::now();

        debug!("Parsing natural language to RDF: {} chars", text.len());

        let prompt = self.build_prompt(text, None);
        let response = self.client.complete(&prompt).await?;

        let triples = self.extract_triples(&response.content)?;
        let relations = self.extract_relations(&response.content)?;

        // Build confidence map
        let mut confidence = HashMap::new();
        for relation in &relations {
            let key = format!(
                "{}-{}-{}",
                relation.subject, relation.predicate, relation.object
            );
            confidence.insert(key, relation.confidence);
        }

        let duration = start.elapsed();

        info!(
            "Parsed {} triples, {} inferred relations in {:?}",
            triples.len(),
            relations.len(),
            duration
        );

        Ok(ParsedTriples {
            triples,
            relations,
            confidence,
            source: text.to_string(),
            metadata: ParsingMetadata {
                token_count: text.split_whitespace().count(),
                duration_ms: duration.as_millis() as u64,
                model: response.model,
                timestamp: chrono::Utc::now(),
            },
        })
    }

    async fn parse_with_context(&self, text: &str, ontology_uri: &str) -> Result<ParsedTriples> {
        let start = std::time::Instant::now();

        debug!("Parsing with ontology context: {}", ontology_uri);

        // In production, would load ontology from URI
        let context = format!(
            "# Ontology: {}\n# (context would be loaded here)",
            ontology_uri
        );

        let prompt = self.build_prompt(text, Some(&context));
        let response = self.client.complete(&prompt).await?;

        let triples = self.extract_triples(&response.content)?;
        let relations = self.extract_relations(&response.content)?;

        let mut confidence = HashMap::new();
        for relation in &relations {
            let key = format!(
                "{}-{}-{}",
                relation.subject, relation.predicate, relation.object
            );
            confidence.insert(key, relation.confidence);
        }

        let duration = start.elapsed();

        Ok(ParsedTriples {
            triples,
            relations,
            confidence,
            source: text.to_string(),
            metadata: ParsingMetadata {
                token_count: text.split_whitespace().count(),
                duration_ms: duration.as_millis() as u64,
                model: response.model,
                timestamp: chrono::Utc::now(),
            },
        })
    }

    async fn batch_parse(&self, texts: Vec<&str>) -> Result<Vec<ParsedTriples>> {
        let mut results = Vec::new();

        for text in texts {
            match self.parse_to_rdf(text).await {
                Ok(parsed) => results.push(parsed),
                Err(e) => {
                    warn!("Failed to parse text: {}", e);
                    // Continue with other texts
                }
            }
        }

        Ok(results)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_parse_to_rdf() {
        let mock_response = r#"
```turtle
ex:Person a owl:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .

ex:name a owl:DatatypeProperty ;
    rdfs:domain ex:Person ;
    rdfs:range xsd:string .
```

```json
[
    {"subject": "ex:Person", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Explicitly stated as a class"},
    {"subject": "ex:name", "predicate": "rdf:type", "object": "owl:DatatypeProperty", "confidence": 0.90, "reasoning": "Inferred from domain and range"}
]
```
"#;

        let client = MockClient::with_response(mock_response);
        let parser = NaturalLanguageParser::new(Arc::new(client));

        let result = parser.parse_to_rdf("A person has a name").await.unwrap();

        assert!(!result.triples.is_empty());
        assert_eq!(result.relations.len(), 2);
        assert!(result.confidence.len() > 0);
    }

    #[test]
    fn test_default_prefixes() {
        let prefixes = NaturalLanguageParser::default_prefixes();
        assert!(prefixes.contains_key("rdf"));
        assert!(prefixes.contains_key("rdfs"));
        assert!(prefixes.contains_key("owl"));
        assert!(prefixes.contains_key("xsd"));
    }
}
