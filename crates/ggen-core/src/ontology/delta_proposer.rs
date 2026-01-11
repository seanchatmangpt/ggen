/// LLM-based ΔΣ Proposer: Autonomous ontology refinement via streaming LLM
///
/// Uses streaming LLMs to propose ontology changes (ΔΣ) based on:
/// - Detected patterns (from pattern_miner)
/// - Current Σ² context
/// - Sector-specific policies
///
/// All proposals are instances of Σ² (never raw text patches).
use async_trait::async_trait;
use futures::stream::{Stream, StreamExt};
use serde::{Deserialize, Serialize};
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::ontology::pattern_miner::{Pattern, PatternType};
use crate::ontology::SigmaSnapshot;

/// A change proposal (ΔΣ² object)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DeltaSigmaProposal {
    /// Unique proposal ID
    pub id: String,

    /// Type of change (AddClass, RemoveProperty, etc.)
    pub change_type: String,

    /// Target element being modified
    pub target_element: String,

    /// The pattern(s) that prompted this proposal
    pub source_patterns: Vec<String>,

    /// Confidence in this proposal (0.0 to 1.0)
    pub confidence: f64,

    /// RDF triples to add
    pub triples_to_add: Vec<String>,

    /// RDF triples to remove (patterns)
    pub triples_to_remove: Vec<String>,

    /// Sector affected
    pub sector: String,

    /// Justification (from LLM)
    pub justification: String,

    /// Estimated impact (bytes of new/modified code)
    pub estimated_impact_bytes: usize,

    /// Backward compatibility: none, compatible, requires_migration
    pub compatibility: String,
}

/// Streaming response from LLM proposer
pub type ProposalStream = Pin<Box<dyn Stream<Item = DeltaSigmaProposal> + Send>>;

/// Trait for LLM-based proposers
#[async_trait]
pub trait DeltaSigmaProposer: Send + Sync {
    /// Propose changes based on patterns and current snapshot
    async fn propose_deltas(
        &self, patterns: Vec<Pattern>, current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<Vec<DeltaSigmaProposal>, String>;

    /// Stream proposals as they are generated (for real-time feedback)
    async fn stream_proposals(
        &self, patterns: Vec<Pattern>, current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<ProposalStream, String>;
}

/// Configuration for the LLM proposer
#[derive(Debug, Clone)]
pub struct ProposerConfig {
    /// LLM model to use
    pub model: String,

    /// Maximum tokens per proposal
    pub max_tokens: usize,

    /// Temperature (0.0 to 1.0)
    pub temperature: f64,

    /// Cache proposals for identical patterns
    pub enable_cache: bool,

    /// Minimum confidence threshold
    pub min_confidence: f64,

    /// Sector-specific policies
    /// **FMEA Fix**: Use BTreeMap for deterministic iteration order
    pub sector_policies: std::collections::BTreeMap<String, String>,
}

impl Default for ProposerConfig {
    fn default() -> Self {
        let mut policies = std::collections::BTreeMap::new();
        policies.insert(
            "support".to_string(),
            "Prioritize ticket tracking and customer communication".to_string(),
        );
        policies.insert(
            "finance".to_string(),
            "Ensure PII masking, audit trails, and compliance with ISO-20022".to_string(),
        );
        policies.insert(
            "papers".to_string(),
            "Generate LaTeX structures; support academic citations".to_string(),
        );

        Self {
            model: "claude-opus".to_string(),
            max_tokens: 1024,
            temperature: 0.3,
            enable_cache: true,
            min_confidence: 0.75,
            sector_policies: policies,
        }
    }
}

/// In-memory cache for proposals (keyed by pattern signature)
/// **FMEA Fix**: Use BTreeMap for deterministic iteration order
#[derive(Debug, Clone)]
struct ProposalCache {
    entries: Arc<RwLock<std::collections::BTreeMap<String, Vec<DeltaSigmaProposal>>>>,
}

impl ProposalCache {
    fn new() -> Self {
        Self {
            entries: Arc::new(RwLock::new(std::collections::BTreeMap::new())),
        }
    }

    async fn get(&self, key: &str) -> Option<Vec<DeltaSigmaProposal>> {
        self.entries.read().await.get(key).cloned()
    }

    async fn insert(&self, key: String, proposals: Vec<DeltaSigmaProposal>) {
        self.entries.write().await.insert(key, proposals);
    }

    fn compute_key(patterns: &[Pattern], sector: &str) -> String {
        let pattern_names: Vec<&str> = patterns.iter().map(|p| p.name.as_str()).collect();
        let pattern_names = pattern_names.join("|");
        format!("{}_{}", sector, pattern_names)
    }
}

/// Mock LLM-based proposer (for demonstration; in production, uses Claude/OpenAI API)
pub struct MockLLMProposer {
    config: ProposerConfig,
    cache: ProposalCache,
}

impl MockLLMProposer {
    pub fn new(config: ProposerConfig) -> Self {
        Self {
            config,
            cache: ProposalCache::new(),
        }
    }

    /// Generate mock proposals from patterns
    fn generate_proposals_from_patterns(
        &self, patterns: Vec<Pattern>, sector: &str,
    ) -> Vec<DeltaSigmaProposal> {
        patterns
            .iter()
            .enumerate()
            .map(|(idx, pattern)| {
                let change_type = match pattern.pattern_type {
                    PatternType::RepeatedStructure => "AddClass",
                    PatternType::RepeatedProperty => "AddProperty",
                    PatternType::SchemaMismatch => "TightenConstraint",
                    PatternType::PerformanceDegradation => "OptimizeOperator",
                    PatternType::OrphanedElement => "RemoveProperty",
                    _ => "RefineConstraint",
                };

                let target = pattern
                    .affected_entities
                    .first()
                    .cloned()
                    .unwrap_or_else(|| format!("element_{}", idx));

                DeltaSigmaProposal {
                    id: format!("proposal_{}_{}_{}", sector, idx, uuid::Uuid::new_v4()),
                    change_type: change_type.to_string(),
                    target_element: target.clone(),
                    source_patterns: vec![pattern.name.clone()],
                    confidence: (pattern.confidence * 0.95).min(1.0),
                    triples_to_add: self.generate_triples_to_add(change_type, &target),
                    triples_to_remove: vec![],
                    sector: sector.to_string(),
                    justification: pattern.description.clone(),
                    estimated_impact_bytes: (pattern.occurrences * 100).clamp(50, 500),
                    compatibility: "compatible".to_string(),
                }
            })
            .filter(|p| p.confidence >= self.config.min_confidence)
            .collect()
    }

    fn generate_triples_to_add(&self, change_type: &str, target: &str) -> Vec<String> {
        match change_type {
            "AddClass" => vec![
                format!("<{}> rdf:type owl:Class .", target),
                format!("<{}> rdfs:label \"{}\" .", target, target),
                format!("<{}> meta:implementsPattern \"GeneratedClass\" .", target),
            ],
            "AddProperty" => vec![
                format!("<{}> rdf:type owl:ObjectProperty .", target),
                format!("<{}> rdfs:label \"{}\" .", target, target),
            ],
            "TightenConstraint" => {
                vec![format!("<{}> meta:hasConstraint _:constraint_1 .", target)]
            }
            _ => vec![format!("<{}> meta:refined true .", target)],
        }
    }
}

#[async_trait]
impl DeltaSigmaProposer for MockLLMProposer {
    async fn propose_deltas(
        &self, patterns: Vec<Pattern>, _current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<Vec<DeltaSigmaProposal>, String> {
        if patterns.is_empty() {
            return Ok(vec![]);
        }

        // Check cache
        let cache_key = ProposalCache::compute_key(&patterns, sector);
        if self.config.enable_cache {
            if let Some(cached) = self.cache.get(&cache_key).await {
                return Ok(cached);
            }
        }

        // Generate proposals
        let proposals = self.generate_proposals_from_patterns(patterns, sector);

        // Store in cache
        if self.config.enable_cache {
            self.cache.insert(cache_key, proposals.clone()).await;
        }

        Ok(proposals)
    }

    async fn stream_proposals(
        &self, patterns: Vec<Pattern>, current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<ProposalStream, String> {
        let proposals = self
            .propose_deltas(patterns, current_snapshot, sector)
            .await?;

        let stream = futures::stream::iter(proposals)
            .then(|proposal| async move { proposal })
            .boxed();

        Ok(stream)
    }
}

/// Real LLM proposer (integrates with Claude/OpenAI via genai crate)
pub struct RealLLMProposer {
    config: ProposerConfig,
    cache: ProposalCache,
    // In production: use genai client
}

impl RealLLMProposer {
    pub fn new(config: ProposerConfig) -> Self {
        Self {
            config,
            cache: ProposalCache::new(),
        }
    }

    /// Build the prompt for the LLM
    fn build_prompt(
        &self, patterns: &[Pattern], current_snapshot: &SigmaSnapshot, sector: &str,
    ) -> String {
        let sector_policy = self
            .config
            .sector_policies
            .get(sector)
            .cloned()
            .unwrap_or_default();

        format!(
            r#"
You are an ontology refinement agent. Given detected patterns in an RDF ontology, propose changes (ΔΣ²).

Patterns detected:
{}

Current ontology version: {}
Sector: {}
Sector Policy: {}

For each pattern, propose a change as a JSON object with:
- change_type: AddClass, RemoveProperty, TightenConstraint, etc.
- target_element: RDF entity to modify
- confidence: 0.0-1.0
- justification: why this change improves the ontology
- triples_to_add: RDF triples to insert
- compatibility: "none", "compatible", "requires_migration"

Output ONLY valid JSON objects, one per line.
"#,
            patterns
                .iter()
                .map(|p| format!("- {} ({}): {}", p.name, p.occurrences, p.description))
                .collect::<Vec<_>>()
                .join("\n"),
            current_snapshot.version,
            sector,
            sector_policy
        )
    }
}

#[async_trait]
impl DeltaSigmaProposer for RealLLMProposer {
    async fn propose_deltas(
        &self, patterns: Vec<Pattern>, current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<Vec<DeltaSigmaProposal>, String> {
        if patterns.is_empty() {
            return Ok(vec![]);
        }

        // Check cache
        let cache_key = ProposalCache::compute_key(&patterns, sector);
        if self.config.enable_cache {
            if let Some(cached) = self.cache.get(&cache_key).await {
                return Ok(cached);
            }
        }

        // Build prompt (currently unused in mock implementation)
        let _prompt = self.build_prompt(&patterns, &current_snapshot, sector);

        // In production: call genai with streaming
        // For now, return mock proposals
        let mut proposals = vec![];
        for (idx, pattern) in patterns.iter().enumerate() {
            proposals.push(DeltaSigmaProposal {
                id: format!("proposal_{}_{}", idx, uuid::Uuid::new_v4()),
                change_type: "RefineConstraint".to_string(),
                target_element: pattern
                    .affected_entities
                    .first()
                    .cloned()
                    .unwrap_or_default(),
                source_patterns: vec![pattern.name.clone()],
                confidence: pattern.confidence,
                triples_to_add: vec![],
                triples_to_remove: vec![],
                sector: sector.to_string(),
                justification: format!("LLM proposal: {}", pattern.description),
                estimated_impact_bytes: 100,
                compatibility: "compatible".to_string(),
            });
        }

        // Cache
        if self.config.enable_cache {
            self.cache.insert(cache_key, proposals.clone()).await;
        }

        Ok(proposals)
    }

    async fn stream_proposals(
        &self, patterns: Vec<Pattern>, current_snapshot: Arc<SigmaSnapshot>, sector: &str,
    ) -> Result<ProposalStream, String> {
        let proposals = self
            .propose_deltas(patterns, current_snapshot, sector)
            .await?;

        let stream = futures::stream::iter(proposals)
            .then(|proposal| async move { proposal })
            .boxed();

        Ok(stream)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::StreamExt;

    #[tokio::test]
    async fn test_propose_deltas_uses_cache() {
        let config = ProposerConfig::default();
        let proposer = MockLLMProposer::new(config);
        let patterns = vec![create_test_pattern()];

        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        let snap_arc = Arc::new(snapshot);

        // First call
        let proposals1 = proposer
            .propose_deltas(patterns.clone(), snap_arc.clone(), "support")
            .await
            .unwrap();

        // Second call (should be cached)
        let proposals2 = proposer
            .propose_deltas(patterns, snap_arc, "support")
            .await
            .unwrap();

        assert_eq!(proposals1, proposals2);
    }

    #[tokio::test]
    async fn test_stream_proposals() {
        let config = ProposerConfig::default();
        let proposer = MockLLMProposer::new(config);
        let patterns = vec![create_test_pattern()];

        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        let mut stream = proposer
            .stream_proposals(patterns, Arc::new(snapshot), "support")
            .await
            .unwrap();

        let mut count = 0;
        while let Some(_proposal) = stream.next().await {
            count += 1;
        }

        assert!(count > 0);
    }

    /// Helper function to create a test pattern for unit tests
    fn create_test_pattern() -> Pattern {
        Pattern {
            name: "test_pattern".to_string(),
            pattern_type: PatternType::RepeatedStructure,
            description: "A test pattern for unit testing".to_string(),
            confidence: 0.8,
            occurrences: 1,
            proposed_changes: vec![],
            affected_entities: vec!["test_entity".to_string()],
        }
    }
}
