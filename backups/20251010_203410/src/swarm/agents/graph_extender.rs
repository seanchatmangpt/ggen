//! Graph Extender Agent - AI-powered RDF graph extension from events
//!
//! Uses LLM inference to extract structured facts from natural language events,
//! business requirements, and runtime data, then extends the RDF graph accordingly.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    AgentHealth, HealthStatus, SwarmAgent, SwarmContext, AgentInput, AgentOutput,
    BaseAgent, AgentConfig, PerformanceThresholds, SystemEvent
};
use crate::client::{LlmClient, LlmConfig};
use crate::providers::adapter::OllamaClient;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::collections::HashMap;
use tracing::debug;

/// Real Graph Extender Agent implementation
#[derive(Debug)]
pub struct GraphExtenderAgent {
    base: BaseAgent,
    inference_client: Box<dyn LlmClient>,
    graph_context: GraphContext,
}

/// Graph context for inference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphContext {
    /// Current graph schema
    pub schema: String,
    /// Existing namespaces
    pub namespaces: HashMap<String, String>,
    /// Domain knowledge
    pub domain_knowledge: HashMap<String, String>,
}

/// Inferred facts from AI processing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferredFacts {
    /// New triples to add
    pub triples: Vec<InferredTriple>,
    /// Confidence score (0.0-1.0)
    pub confidence: f64,
    /// Inference reasoning
    pub reasoning: String,
    /// Source event
    pub source_event: String,
}

/// Inferred triple for graph extension
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferredTriple {
    /// Subject (entity)
    pub subject: String,
    /// Predicate (relationship)
    pub predicate: String,
    /// Object (value or entity)
    pub object: String,
    /// Triple type (assertion, inference, etc.)
    pub triple_type: TripleType,
}

/// Triple types for classification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TripleType {
    /// Direct assertion from source
    Assertion,
    /// Inferred relationship
    Inference,
    /// Derived property
    Derived,
}

impl GraphExtenderAgent {
    /// Create a new Graph Extender Agent
    pub fn new(inference_client: Box<dyn LlmClient>, graph_context: GraphContext) -> Self {
        let base = BaseAgent::new(
            "graph_extender",
            vec![
                "rdf_inference".to_string(),
                "fact_extraction".to_string(),
                "graph_extension".to_string(),
                "semantic_analysis".to_string(),
            ],
            AgentConfig {
                timeout_seconds: 60,
                retry_attempts: 3,
                verbose_logging: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 10000,
                    max_memory_usage_mb: 200,
                    min_quality_score: 0.8,
                },
            },
        );

        Self {
            base,
            inference_client,
            graph_context,
        }
    }

    /// Create with Ollama qwen3-coder configuration
    pub fn with_ollama_qwen3_coder(graph_context: GraphContext) -> Result<Self> {
        let config = OllamaClient::qwen3_coder_config();
        let client = Box::new(OllamaClient::new(config)?);

        Ok(Self::new(client, graph_context))
    }

    /// Create with default configuration
    pub fn default_config(graph_context: GraphContext) -> Self {
        let config = LlmConfig {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: Some(2048),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        };
        let client = Box::new(OllamaClient::new(config).expect("Failed to create Ollama client"));

        Self::new(client, graph_context)
    }

    /// Infer facts from system events
    async fn infer_facts_from_events(&self, events: &[SystemEvent]) -> Result<InferredFacts> {
        let prompt = self.build_inference_prompt(events)?;

        let response = self.inference_client.complete(&prompt).await?;

        self.parse_inference_response(&response.content)
    }

    /// Build inference prompt for AI
    fn build_inference_prompt(&self, events: &[SystemEvent]) -> Result<String> {
        let events_json = serde_json::to_string_pretty(events)
            .map_err(|e| GgenAiError::serialization_error(&format!("Failed to serialize events: {}", e)))?;

        let context_json = serde_json::to_string_pretty(&self.graph_context)
            .map_err(|e| GgenAiError::serialization_error(&format!("Failed to serialize context: {}", e)))?;

        Ok(format!(
            r#"You are an expert RDF graph engineer tasked with inferring new facts from system events.

Current Graph Context:
{}

Recent Events:
{}

Your task is to:
1. Analyze each event for information that should be captured in the RDF graph
2. Infer relationships and properties that aren't explicitly stated
3. Create well-formed RDF triples using appropriate vocabularies
4. Provide confidence scores for each inference
5. Explain your reasoning for each triple

Return your response as a JSON object with this structure:
{{
    "triples": [
        {{
            "subject": "Entity or URI",
            "predicate": "Property or relationship",
            "object": "Value or entity reference",
            "triple_type": "assertion|inference|derived",
            "confidence": 0.9,
            "reasoning": "Explanation of why this triple should be added"
        }}
    ],
    "overall_confidence": 0.85,
    "analysis_summary": "Brief summary of what was inferred"
}}

Guidelines:
- Use existing namespaces when possible (from context)
- Create new URIs following RDF best practices
- Focus on business-relevant facts and relationships
- Be conservative - only infer what has strong evidence
- Prefer standard vocabularies (FOAF, DC, PROV, etc.) when applicable

Events to analyze:
{}

Return only valid JSON."#,
            context_json, events_json, events_json
        ))
    }

    /// Parse AI inference response
    fn parse_inference_response(&self, response: &str) -> Result<InferredFacts> {
        // Try to extract JSON from response
        let json_start = response.find('{').unwrap_or(0);
        let json_end = response.rfind('}').map(|i| i + 1).unwrap_or(response.len());
        let json_content = &response[json_start..json_end];

        let parsed: Value = serde_json::from_str(json_content)
            .map_err(|e| GgenAiError::parsing_error(&format!("Failed to parse AI response: {}", e)))?;

        let triples_value = parsed.get("triples")
            .ok_or_else(|| GgenAiError::parsing_error("No 'triples' field in AI response"))?;

        let mut triples = Vec::new();
        if let Value::Array(triple_array) = triples_value {
            for triple_value in triple_array {
                if let Ok(triple) = serde_json::from_value::<InferredTriple>(triple_value.clone()) {
                    triples.push(triple);
                }
            }
        }

        let confidence = parsed.get("overall_confidence")
            .and_then(|v| v.as_f64())
            .unwrap_or(0.5);

        let reasoning = parsed.get("analysis_summary")
            .and_then(|v| v.as_str())
            .unwrap_or("AI-generated inference")
            .to_string();

        Ok(InferredFacts {
            triples,
            confidence,
            reasoning,
            source_event: "ai_inference".to_string(),
        })
    }
}

impl SwarmAgent for GraphExtenderAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        let start_time = std::time::Instant::now();

        // Extract events from input
        let events: Vec<SystemEvent> = if let Some(events_data) = input.data.get("events") {
            serde_json::from_value(events_data.clone())?
        } else {
            serde_json::from_value(input.data)?
        };

        // Infer facts from events
        let inferred_facts = self.execute_with_retry(|| async {
            self.infer_facts_from_events(&events).await
        }).await?;

        // Convert to output format
        let output_data = json!({
            "inferred_facts": inferred_facts,
            "graph_delta": {
                "added_triples": inferred_facts.triples,
                "removed_triples": [],
                "metadata": {
                    "source": "ai_inference",
                    "confidence": inferred_facts.confidence,
                    "reasoning": inferred_facts.reasoning
                }
            }
        });

        let execution_time = start_time.elapsed().as_millis();

        Ok(AgentOutput {
            data: output_data,
            output_type: "graph_delta".to_string(),
            target_agents: vec!["validator".to_string()],
            metadata: {
                let mut metadata = HashMap::new();
                metadata.insert("execution_time_ms".to_string(), execution_time.to_string());
                metadata.insert("confidence".to_string(), inferred_facts.confidence.to_string());
                metadata.insert("triples_inferred".to_string(), inferred_facts.triples.len().to_string());
                metadata
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate that we have an inference client
        if self.inference_client.get_config().model.is_empty() {
            return Ok(false);
        }

        // Validate graph context
        if self.graph_context.schema.is_empty() {
            return Ok(false);
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        let mut issues = Vec::new();

        // Check inference client health
        if self.inference_client.get_config().model.is_empty() {
            issues.push("No inference model configured".to_string());
        }

        // Check graph context
        if self.graph_context.schema.is_empty() {
            issues.push("No graph schema available".to_string());
        }

        let score = if issues.is_empty() { 1.0 } else { 0.5 };

        let status = if score > 0.8 {
            HealthStatus::Healthy
        } else if score > 0.5 {
            HealthStatus::Degraded
        } else {
            HealthStatus::Unhealthy
        };

        AgentHealth {
            status,
            score,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::adapter::MockClient;

    #[tokio::test]
    async fn test_graph_extender_creation() {
        let graph_context = GraphContext {
            schema: "test schema".to_string(),
            namespaces: HashMap::new(),
            domain_knowledge: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test response"));
        let agent = GraphExtenderAgent::new(client, graph_context);

        assert_eq!(agent.name(), "graph_extender");
        assert!(agent.capabilities().contains(&"rdf_inference".to_string()));
    }

    #[test]
    fn test_prompt_building() {
        let graph_context = GraphContext {
            schema: "Person class with name property".to_string(),
            namespaces: HashMap::new(),
            domain_knowledge: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test"));
        let agent = GraphExtenderAgent::new(client, graph_context);

        let events = vec![SystemEvent::BusinessRequirement {
            requirement_id: "REQ-001".to_string(),
            description: "Add user authentication".to_string(),
            priority: "high".to_string(),
            stakeholder: "product_team".to_string(),
        }];

        let prompt = agent.build_inference_prompt(&events).unwrap();
        assert!(prompt.contains("Add user authentication"));
        assert!(prompt.contains("RDF triples"));
    }

    #[tokio::test]
    async fn test_health_check() {
        let graph_context = GraphContext {
            schema: "test schema".to_string(),
            namespaces: HashMap::new(),
            domain_knowledge: HashMap::new(),
        };

        let client = Box::new(MockClient::with_response("test"));
        let agent = GraphExtenderAgent::new(client, graph_context);

        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert_eq!(health.score, 1.0);
    }
}
