/// Pattern Miner: Automated ontology evolution via anomaly and drift detection
///
/// This module scans observation data (O) and generated artifacts (A) to detect:
/// - Repeated structures (candidates for new classes)
/// - Schema mismatches (type errors or missing constraints)
/// - Performance anomalies (degradation in operator latency)
/// - Usage patterns (candidate refactorings and new projections)

use std::collections::{BTreeMap, HashMap, HashSet};
use oxigraph::model::{Statement, NamedNode};
use serde::{Deserialize, Serialize};

/// A detected pattern in the ontology or data
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Pattern {
    /// Name of the pattern
    pub name: String,

    /// Type of pattern
    pub pattern_type: PatternType,

    /// Description
    pub description: String,

    /// Confidence score (0.0 to 1.0)
    pub confidence: f64,

    /// Count of occurrences
    pub occurrences: usize,

    /// Proposed change(s)
    pub proposed_changes: Vec<ProposedChange>,

    /// Affected entities
    pub affected_entities: Vec<String>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum PatternType {
    /// Repeated class structure (candidate for new class)
    RepeatedStructure,

    /// Repeated property pattern (candidate for new property)
    RepeatedProperty,

    /// Type mismatch or schema constraint violation
    SchemaMismatch,

    /// Performance degradation
    PerformanceDegradation,

    /// Unused or orphaned element
    OrphanedElement,

    /// Class hierarchy inconsistency
    HierarchyInconsistency,

    /// Missing projection or code generation gap
    MissingProjection,

    /// Guard near-miss (almost violated a constraint)
    GuardNearMiss,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProposedChange {
    pub change_type: String,
    pub target: String,
    pub rationale: String,
}

/// Statistical summary of ontology observations
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct OntologyStats {
    /// Total number of classes
    pub class_count: usize,

    /// Total number of properties
    pub property_count: usize,

    /// Total number of constraints
    pub constraint_count: usize,

    /// Most frequently used classes
    pub top_classes: BTreeMap<String, usize>,

    /// Most frequently used properties
    pub top_properties: BTreeMap<String, usize>,

    /// Ratio of used to defined elements
    pub utilization_ratio: f64,

    /// Average property per class
    pub avg_properties_per_class: f64,
}

/// Pattern mining engine
pub struct PatternMiner {
    /// Collected observations from (O, A)
    observations: Vec<Observation>,

    /// Detected patterns
    patterns: Vec<Pattern>,

    /// Configuration
    config: MinerConfig,
}

#[derive(Debug, Clone)]
pub struct Observation {
    pub entity: String,
    pub properties: BTreeMap<String, String>,
    pub timestamp: u64,
    pub source: ObservationSource,
}

#[derive(Debug, Clone, Copy)]
pub enum ObservationSource {
    /// From O (observation data)
    Data,

    /// From A (generated artifacts)
    Artifact,

    /// From operator receipts/logs
    Receipt,
}

#[derive(Debug, Clone)]
pub struct MinerConfig {
    /// Minimum confidence score to report pattern
    pub min_confidence: f64,

    /// Minimum occurrences to constitute a pattern
    pub min_occurrences: usize,

    /// Maximum number of patterns to return
    pub max_patterns: usize,

    /// Enable schema mismatch detection
    pub detect_schema_mismatches: bool,

    /// Enable performance anomaly detection
    pub detect_performance_anomalies: bool,

    /// Enable orphaned element detection
    pub detect_orphaned_elements: bool,

    /// Performance threshold: latency change % to flag anomaly
    pub perf_anomaly_threshold_pct: f64,
}

impl Default for MinerConfig {
    fn default() -> Self {
        Self {
            min_confidence: 0.75,
            min_occurrences: 3,
            max_patterns: 50,
            detect_schema_mismatches: true,
            detect_performance_anomalies: true,
            detect_orphaned_elements: true,
            perf_anomaly_threshold_pct: 10.0,
        }
    }
}

impl PatternMiner {
    /// Create a new pattern miner
    pub fn new(config: MinerConfig) -> Self {
        Self {
            observations: Vec::new(),
            patterns: Vec::new(),
            config,
        }
    }

    /// Add an observation
    pub fn add_observation(&mut self, obs: Observation) {
        self.observations.push(obs);
    }

    /// Add multiple observations
    pub fn add_observations(&mut self, obs: Vec<Observation>) {
        self.observations.extend(obs);
    }

    /// Run pattern mining
    pub fn mine(&mut self) -> Result<Vec<Pattern>, String> {
        self.patterns.clear();

        if self.observations.is_empty() {
            return Ok(vec![]);
        }

        // Mine different pattern types
        self.mine_repeated_structures()?;
        self.mine_schema_mismatches()?;
        self.mine_performance_anomalies()?;
        self.mine_orphaned_elements()?;

        // Filter by min occurrences and sort by confidence
        let mut patterns = self.patterns.clone();
        patterns.retain(|p| p.occurrences >= self.config.min_occurrences &&
                           p.confidence >= self.config.min_confidence);
        patterns.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap_or(std::cmp::Ordering::Equal));

        patterns.truncate(self.config.max_patterns);
        self.patterns = patterns.clone();

        Ok(patterns)
    }

    /// Mine repeated structures (candidate for new classes)
    fn mine_repeated_structures(&mut self) -> Result<(), String> {
        let mut structure_counts: HashMap<String, Vec<String>> = HashMap::new();

        // Group observations by property signature
        for obs in &self.observations {
            let mut props: Vec<_> = obs.properties.keys().cloned().collect();
            props.sort();
            let signature = props.join("|");

            structure_counts.entry(signature).or_insert_with(Vec::new)
                .push(obs.entity.clone());
        }

        // Find repeated structures
        for (signature, entities) in structure_counts {
            if entities.len() >= self.config.min_occurrences {
                let props: Vec<&str> = signature.split('|').collect();
                let confidence = (entities.len() as f64) / (self.observations.len() as f64);

                let pattern = Pattern {
                    name: format!("Repeated_{}", signature.replace("|", "_")),
                    pattern_type: PatternType::RepeatedStructure,
                    description: format!(
                        "Found {} instances with properties: {}",
                        entities.len(),
                        props.join(", ")
                    ),
                    confidence,
                    occurrences: entities.len(),
                    proposed_changes: vec![
                        ProposedChange {
                            change_type: "AddClass".to_string(),
                            target: format!("Class_{}", signature.replace("|", "_")),
                            rationale: format!(
                                "Consolidate repeated structure into a new class"
                            ),
                        }
                    ],
                    affected_entities: entities,
                };

                self.patterns.push(pattern);
            }
        }

        Ok(())
    }

    /// Mine schema mismatches
    fn mine_schema_mismatches(&mut self) -> Result<(), String> {
        if !self.config.detect_schema_mismatches {
            return Ok(());
        }

        let mut type_violations: HashMap<String, Vec<String>> = HashMap::new();

        // Simple heuristic: detect value inconsistencies
        for (key, values) in self.group_by_property() {
            let mut value_types = HashSet::new();
            for val in &values {
                let inferred_type = self.infer_type(val);
                value_types.insert(inferred_type);
            }

            if value_types.len() > 1 {
                let types: Vec<_> = value_types.iter().cloned().collect();
                type_violations.entry(key.clone())
                    .or_insert_with(Vec::new)
                    .extend(types);
            }
        }

        for (property, types) in type_violations {
            if types.len() > 1 {
                let pattern = Pattern {
                    name: format!("SchemaMismatch_{}", property),
                    pattern_type: PatternType::SchemaMismatch,
                    description: format!(
                        "Property '{}' has inconsistent types: {:?}",
                        property, types
                    ),
                    confidence: 0.85,
                    occurrences: types.len(),
                    proposed_changes: vec![
                        ProposedChange {
                            change_type: "TightenConstraint".to_string(),
                            target: property.clone(),
                            rationale: "Add type constraint to prevent mixed types".to_string(),
                        }
                    ],
                    affected_entities: vec![property],
                };

                self.patterns.push(pattern);
            }
        }

        Ok(())
    }

    /// Mine performance anomalies
    fn mine_performance_anomalies(&mut self) -> Result<(), String> {
        if !self.config.detect_performance_anomalies {
            return Ok(());
        }

        // Group observations by entity and track latency over time
        let mut latencies: BTreeMap<String, Vec<(u64, f64)>> = BTreeMap::new();

        for obs in &self.observations {
            if let Some(latency_str) = obs.properties.get("latency_us") {
                if let Ok(latency) = latency_str.parse::<f64>() {
                    latencies.entry(obs.entity.clone())
                        .or_insert_with(Vec::new)
                        .push((obs.timestamp, latency));
                }
            }
        }

        // Detect degradation
        for (entity, mut latency_samples) in latencies {
            latency_samples.sort_by_key(|x| x.0); // Sort by timestamp

            if latency_samples.len() >= 2 {
                let first_latency = latency_samples[0].1;
                let last_latency = latency_samples[latency_samples.len() - 1].1;

                let degradation_pct = ((last_latency - first_latency) / first_latency) * 100.0;

                if degradation_pct > self.config.perf_anomaly_threshold_pct {
                    let pattern = Pattern {
                        name: format!("PerfDegradation_{}", entity),
                        pattern_type: PatternType::PerformanceDegradation,
                        description: format!(
                            "Entity '{}' latency degraded {:.1}% (from {:.0}μs to {:.0}μs)",
                            entity, degradation_pct, first_latency, last_latency
                        ),
                        confidence: 0.90,
                        occurrences: latency_samples.len(),
                        proposed_changes: vec![
                            ProposedChange {
                                change_type: "OptimizeOperator".to_string(),
                                target: entity.clone(),
                                rationale: format!(
                                    "Operator latency degraded; recommend profiling and optimization"
                                ),
                            }
                        ],
                        affected_entities: vec![entity],
                    };

                    self.patterns.push(pattern);
                }
            }
        }

        Ok(())
    }

    /// Mine orphaned elements (unused classes/properties)
    fn mine_orphaned_elements(&mut self) -> Result<(), String> {
        if !self.config.detect_orphaned_elements {
            return Ok(());
        }

        let mut usage_counts: HashMap<String, usize> = HashMap::new();

        for obs in &self.observations {
            for key in obs.properties.keys() {
                *usage_counts.entry(key.clone()).or_insert(0) += 1;
            }
        }

        // Low usage = potentially orphaned
        for (element, count) in usage_counts {
            let usage_ratio = count as f64 / self.observations.len() as f64;

            if usage_ratio < 0.1 { // Less than 10% usage
                let pattern = Pattern {
                    name: format!("Orphaned_{}", element),
                    pattern_type: PatternType::OrphanedElement,
                    description: format!(
                        "Element '{}' used in only {:.1}% of observations",
                        element, usage_ratio * 100.0
                    ),
                    confidence: 0.7,
                    occurrences: count,
                    proposed_changes: vec![
                        ProposedChange {
                            change_type: "Review".to_string(),
                            target: element.clone(),
                            rationale: "Low usage; consider removal or deprecation".to_string(),
                        }
                    ],
                    affected_entities: vec![element],
                };

                self.patterns.push(pattern);
            }
        }

        Ok(())
    }

    /// Group observations by property
    fn group_by_property(&self) -> BTreeMap<String, Vec<String>> {
        let mut grouped = BTreeMap::new();

        for obs in &self.observations {
            for (key, val) in &obs.properties {
                grouped.entry(key.clone())
                    .or_insert_with(Vec::new)
                    .push(val.clone());
            }
        }

        grouped
    }

    /// Infer the type of a value
    fn infer_type(&self, value: &str) -> String {
        if value.parse::<i64>().is_ok() {
            "integer".to_string()
        } else if value.parse::<f64>().is_ok() {
            "float".to_string()
        } else if value.to_lowercase() == "true" || value.to_lowercase() == "false" {
            "boolean".to_string()
        } else {
            "string".to_string()
        }
    }

    /// Get statistics about the ontology
    pub fn stats(&self) -> OntologyStats {
        let mut stats = OntologyStats::default();
        let mut property_freq: BTreeMap<String, usize> = BTreeMap::new();

        for obs in &self.observations {
            for key in obs.properties.keys() {
                *property_freq.entry(key.clone()).or_insert(0) += 1;
            }
        }

        // Sort by frequency
        let mut sorted_props: Vec<_> = property_freq.into_iter().collect();
        sorted_props.sort_by(|a, b| b.1.cmp(&a.1));

        stats.property_count = sorted_props.len();
        stats.top_properties = sorted_props.into_iter().take(10).collect();
        stats.utilization_ratio = if self.observations.is_empty() {
            0.0
        } else {
            (self.patterns.len() as f64) / (self.observations.len() as f64)
        };

        stats
    }

    /// Get all detected patterns
    pub fn patterns(&self) -> &[Pattern] {
        &self.patterns
    }

    /// Clear observations and patterns
    pub fn clear(&mut self) {
        self.observations.clear();
        self.patterns.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_observations() -> Vec<Observation> {
        vec![
            Observation {
                entity: "entity_1".to_string(),
                properties: [("type".to_string(), "user".to_string()),
                             ("name".to_string(), "Alice".to_string())].iter().cloned().collect(),
                timestamp: 1000,
                source: ObservationSource::Data,
            },
            Observation {
                entity: "entity_2".to_string(),
                properties: [("type".to_string(), "user".to_string()),
                             ("name".to_string(), "Bob".to_string())].iter().cloned().collect(),
                timestamp: 2000,
                source: ObservationSource::Data,
            },
            Observation {
                entity: "entity_3".to_string(),
                properties: [("type".to_string(), "user".to_string()),
                             ("name".to_string(), "Charlie".to_string())].iter().cloned().collect(),
                timestamp: 3000,
                source: ObservationSource::Data,
            },
        ]
    }

    #[test]
    fn test_pattern_miner_creation() {
        let miner = PatternMiner::new(MinerConfig::default());
        assert_eq!(miner.patterns.len(), 0);
        assert_eq!(miner.observations.len(), 0);
    }

    #[test]
    fn test_add_observations() {
        let mut miner = PatternMiner::new(MinerConfig::default());
        let obs = create_test_observations();

        miner.add_observations(obs.clone());
        assert_eq!(miner.observations.len(), 3);
    }

    #[test]
    fn test_mine_repeated_structures() {
        let mut miner = PatternMiner::new(MinerConfig::default());
        let obs = create_test_observations();

        miner.add_observations(obs);
        let patterns = miner.mine().unwrap();

        assert!(!patterns.is_empty());
        let repeated = patterns.iter()
            .find(|p| p.pattern_type == PatternType::RepeatedStructure);
        assert!(repeated.is_some());
    }

    #[test]
    fn test_stats() {
        let mut miner = PatternMiner::new(MinerConfig::default());
        let obs = create_test_observations();

        miner.add_observations(obs);
        miner.mine().unwrap();

        let stats = miner.stats();
        assert_eq!(stats.property_count, 2);
    }
}
