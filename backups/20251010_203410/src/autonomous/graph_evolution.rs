//! Graph Evolution Engine
//!
//! Core orchestrator for autonomous RDF graph evolution combining
//! NL parsing, delta detection, validation, and atomic commits.

use crate::autonomous::{
    nl_parser::{NaturalLanguageParser, NlParser, ParsedTriples},
    validator::{SelfValidator, Validator, ValidationResult},
    delta_detector::{DeltaDetector, GraphDelta},
};
use crate::client::LlmClient;
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::{debug, info, warn, error};

/// Evolution engine configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvolutionConfig {
    /// Base URI for generated entities
    pub base_uri: String,
    /// Minimum confidence threshold for accepting inferred triples (0.0-1.0)
    pub confidence_threshold: f32,
    /// Enable automatic validation
    pub auto_validate: bool,
    /// Enable automatic rollback on validation failure
    pub auto_rollback: bool,
    /// Minimum changes to trigger regeneration
    pub regeneration_threshold: usize,
    /// Path to store evolution history
    pub history_path: Option<PathBuf>,
}

impl Default for EvolutionConfig {
    fn default() -> Self {
        Self {
            base_uri: "http://example.org/".to_string(),
            confidence_threshold: 0.7,
            auto_validate: true,
            auto_rollback: true,
            regeneration_threshold: 5,
            history_path: None,
        }
    }
}

/// Result of an evolution operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvolutionResult {
    /// Whether operation succeeded
    pub success: bool,
    /// Parsed triples from NL
    pub parsed: Option<ParsedTriples>,
    /// Graph delta detected
    pub delta: Option<GraphDelta>,
    /// Validation result
    pub validation: Option<ValidationResult>,
    /// Error message if failed
    pub error: Option<String>,
    /// Evolution metadata
    pub metadata: EvolutionMetadata,
}

/// Metadata about evolution operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvolutionMetadata {
    /// Total duration (ms)
    pub duration_ms: u64,
    /// Timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Number of operations performed
    pub operations_count: usize,
    /// Whether changes were committed
    pub committed: bool,
}

/// Graph evolution engine
#[derive(Debug)]
pub struct GraphEvolutionEngine {
    config: EvolutionConfig,
    parser: NaturalLanguageParser,
    validator: SelfValidator,
    delta_detector: DeltaDetector,
    current_graph: Vec<String>,
}

impl GraphEvolutionEngine {
    /// Create a new evolution engine
    pub fn new(parser_client: Arc<dyn LlmClient>, validator_client: Arc<dyn LlmClient>, config: EvolutionConfig) -> Result<Self> {
        let mut parser = NaturalLanguageParser::new(parser_client);
        parser.add_prefix("ex".to_string(), config.base_uri.clone());

        let validator = SelfValidator::new(validator_client)?;
        let delta_detector = DeltaDetector::new()?;

        Ok(Self {
            config,
            parser,
            validator,
            delta_detector,
            current_graph: Vec::new(),
        })
    }

    /// Create with default configuration
    pub fn with_defaults(parser_client: Arc<dyn LlmClient>, validator_client: Arc<dyn LlmClient>) -> Result<Self> {
        Self::new(parser_client, validator_client, EvolutionConfig::default())
    }

    /// Evolve graph from natural language description
    pub async fn evolve_from_nl(&mut self, text: &str) -> Result<EvolutionResult> {
        let start = std::time::Instant::now();
        let timestamp = chrono::Utc::now();
        let mut operations_count = 0;

        info!("Starting graph evolution from NL");

        // 1. Parse natural language to RDF
        debug!("Step 1: Parsing natural language");
        let parsed = match self.parser.parse_to_rdf(text).await {
            Ok(p) => {
                operations_count += 1;
                p
            }
            Err(e) => {
                error!("NL parsing failed: {}", e);
                return Ok(EvolutionResult {
                    success: false,
                    parsed: None,
                    delta: None,
                    validation: None,
                    error: Some(format!("Parsing failed: {}", e)),
                    metadata: EvolutionMetadata {
                        duration_ms: start.elapsed().as_millis() as u64,
                        timestamp,
                        operations_count,
                        committed: false,
                    },
                });
            }
        };

        // Filter by confidence threshold
        let accepted_triples: Vec<String> = parsed.triples.iter()
            .filter(|triple| {
                // Check if this triple has a confidence score
                let has_confidence = parsed.relations.iter().any(|rel| {
                    let triple_str = format!("{} {} {}", rel.subject, rel.predicate, rel.object);
                    triple.contains(&triple_str) && rel.confidence >= self.config.confidence_threshold
                });

                // Accept if no confidence info or meets threshold
                !has_confidence || has_confidence
            })
            .cloned()
            .collect();

        info!("Accepted {}/{} triples after confidence filtering",
              accepted_triples.len(), parsed.triples.len());

        // 2. Compute delta
        debug!("Step 2: Computing graph delta");
        let delta = match self.delta_detector.compute_delta(&accepted_triples) {
            Ok(d) => {
                operations_count += 1;
                d
            }
            Err(e) => {
                error!("Delta computation failed: {}", e);
                return Ok(EvolutionResult {
                    success: false,
                    parsed: Some(parsed),
                    delta: None,
                    validation: None,
                    error: Some(format!("Delta computation failed: {}", e)),
                    metadata: EvolutionMetadata {
                        duration_ms: start.elapsed().as_millis() as u64,
                        timestamp,
                        operations_count,
                        committed: false,
                    },
                });
            }
        };

        // 3. Validate if enabled
        let validation_result = if self.config.auto_validate {
            debug!("Step 3: Validating changes");
            match self.validator.validate(&accepted_triples).await {
                Ok(v) => {
                    operations_count += 1;
                    Some(v)
                }
                Err(e) => {
                    warn!("Validation failed: {}", e);
                    if self.config.auto_rollback {
                        error!("Auto-rollback enabled, rejecting changes");
                        return Ok(EvolutionResult {
                            success: false,
                            parsed: Some(parsed),
                            delta: Some(delta),
                            validation: None,
                            error: Some(format!("Validation failed: {}", e)),
                            metadata: EvolutionMetadata {
                                duration_ms: start.elapsed().as_millis() as u64,
                                timestamp,
                                operations_count,
                                committed: false,
                            },
                        });
                    }
                    None
                }
            }
        } else {
            None
        };

        // Check validation passed
        if let Some(ref val) = validation_result {
            if !val.passed && self.config.auto_rollback {
                error!("Validation failed with {} violations, rolling back",
                       val.violations.len());
                return Ok(EvolutionResult {
                    success: false,
                    parsed: Some(parsed),
                    delta: Some(delta),
                    validation: validation_result,
                    error: Some("Validation failed".to_string()),
                    metadata: EvolutionMetadata {
                        duration_ms: start.elapsed().as_millis() as u64,
                        timestamp,
                        operations_count,
                        committed: false,
                    },
                });
            }
        }

        // 4. Commit changes atomically
        debug!("Step 4: Committing changes");
        self.current_graph = accepted_triples.clone();
        self.delta_detector.apply_delta(&delta)?;
        operations_count += 1;

        info!("Graph evolution completed successfully in {:?}", start.elapsed());

        Ok(EvolutionResult {
            success: true,
            parsed: Some(parsed),
            delta: Some(delta),
            validation: validation_result,
            error: None,
            metadata: EvolutionMetadata {
                duration_ms: start.elapsed().as_millis() as u64,
                timestamp,
                operations_count,
                committed: true,
            },
        })
    }

    /// Get current graph state
    pub fn get_current_graph(&self) -> &[String] {
        &self.current_graph
    }

    /// Get evolution history
    pub fn get_evolution_history(&self) -> &[GraphDelta] {
        self.delta_detector.get_history()
    }

    /// Check if regeneration is needed based on threshold
    pub fn needs_regeneration(&self) -> bool {
        if let Some(last_delta) = self.delta_detector.get_history().last() {
            self.delta_detector.is_significant(last_delta, self.config.regeneration_threshold)
        } else {
            false
        }
    }

    /// Export current graph to Turtle format
    pub fn export_turtle(&self) -> String {
        let mut output = String::new();

        // Add standard prefixes
        output.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        output.push_str("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n");
        output.push_str("@prefix owl: <http://www.w3.org/2002/07/owl#> .\n");
        output.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
        output.push_str(&format!("@prefix ex: <{}> .\n\n", self.config.base_uri));

        // Add triples
        for triple in &self.current_graph {
            output.push_str(triple);
            if !triple.ends_with('.') {
                output.push('.');
            }
            output.push('\n');
        }

        output
    }

    /// Get configuration
    pub fn config(&self) -> &EvolutionConfig {
        &self.config
    }

    /// Update configuration
    pub fn update_config(&mut self, config: EvolutionConfig) {
        self.config = config;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_evolution_from_nl() {
        let mock_response = r#"
```turtle
ex:Person a owl:Class ;
    rdfs:label "Person" .
```

```json
[
    {"subject": "ex:Person", "predicate": "rdf:type", "object": "owl:Class", "confidence": 0.95, "reasoning": "Stated"}
]
```
"#;
        let client = Arc::new(MockClient::with_response(mock_response));
        let validator_client = Arc::new(MockClient::with_response("Validation passed"));
        let mut engine = GraphEvolutionEngine::with_defaults(client, validator_client).unwrap();

        let result = engine.evolve_from_nl("A person is a class").await.unwrap();

        assert!(result.success);
        assert!(result.parsed.is_some());
        assert!(result.delta.is_some());
        assert!(result.metadata.committed);
    }

    #[test]
    fn test_export_turtle() {
        let client = Arc::new(MockClient::with_response("test"));
        let validator_client = Arc::new(MockClient::with_response("Validation passed"));
        let engine = GraphEvolutionEngine::with_defaults(client, validator_client).unwrap();

        let turtle = engine.export_turtle();

        assert!(turtle.contains("@prefix rdf:"));
        assert!(turtle.contains("@prefix owl:"));
    }

    #[test]
    fn test_default_config() {
        let config = EvolutionConfig::default();

        assert_eq!(config.confidence_threshold, 0.7);
        assert!(config.auto_validate);
        assert!(config.auto_rollback);
    }
}
