use crate::codegen::{SwarmCoordinator, SwarmSummary};
use crate::manifest::{GenerationRule, GgenManifest};
use ggen_utils::error::Result;

pub struct SwarmExecutorBridge {
    coordinator: SwarmCoordinator,
    high_risk_rules: Vec<String>,
}

impl SwarmExecutorBridge {
    pub fn new(agent_count: usize, _manifest: GgenManifest, high_risk_rules: Vec<String>) -> Self {
        Self {
            coordinator: SwarmCoordinator::new(agent_count),
            high_risk_rules,
        }
    }

    pub async fn prepare_execution(&self, rules_to_run: Vec<GenerationRule>) -> Result<()> {
        self.coordinator.enqueue_rules(rules_to_run).await?;
        self.coordinator.distribute_work().await?;
        Ok(())
    }

    pub async fn get_execution_summary(&self) -> Result<SwarmSummary> {
        self.coordinator.get_swarm_summary().await
    }

    pub async fn record_rule_completion(&self, rule_name: &str) -> Result<()> {
        self.coordinator.record_completion(rule_name).await
    }

    pub async fn record_rule_failure(&self, rule_name: &str) -> Result<()> {
        self.coordinator.record_failure(rule_name).await
    }

    pub fn get_agent_prioritization(&self, rule_name: &str) -> u8 {
        if self.high_risk_rules.contains(&rule_name.to_string()) {
            10
        } else {
            1
        }
    }

    pub fn estimate_concurrent_speedup(&self, total_rules: usize) -> f64 {
        if total_rules == 0 {
            return 1.0;
        }

        let agent_count = 4;
        let parallelizable_ratio = 0.7;

        1.0 / (1.0 - parallelizable_ratio + (parallelizable_ratio / agent_count as f64))
    }
}

pub struct ExecutionStrategy {
    pub use_swarm: bool,
    pub agent_count: usize,
    pub prioritize_high_risk: bool,
    pub estimated_speedup: f64,
}

pub fn determine_execution_strategy(
    rule_count: usize,
    high_risk_count: usize,
    manifest_changed: bool,
) -> ExecutionStrategy {
    let use_swarm = rule_count > 10 && !manifest_changed;
    let agent_count = if use_swarm { 4 } else { 1 };

    let mut estimated_speedup = 1.0;
    if use_swarm {
        estimated_speedup = 1.0 / (1.0 - 0.7 + (0.7 / 4.0));
    }

    ExecutionStrategy {
        use_swarm,
        agent_count,
        prioritize_high_risk: high_risk_count > 0,
        estimated_speedup,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn create_test_manifest() -> GgenManifest {
        GgenManifest {
            project: crate::manifest::ProjectConfig {
                name: "test".to_string(),
                version: "1.0.0".to_string(),
                description: None,
            },
            ontology: crate::manifest::OntologyConfig {
                source: "ontology.ttl".into(),
                imports: vec![],
                base_iri: None,
                prefixes: BTreeMap::new(),
            },
            inference: crate::manifest::InferenceConfig::default(),
            generation: crate::manifest::GenerationConfig {
                rules: vec![],
                output_dir: "output".into(),
                require_audit_trail: false,
                determinism_salt: None,
                max_sparql_timeout_ms: 5000,
            },
            validation: crate::manifest::ValidationConfig::default(),
        }
    }

    #[test]
    fn test_execution_strategy_small_batch() {
        let strategy = determine_execution_strategy(3, 0, false);
        assert!(!strategy.use_swarm);
        assert_eq!(strategy.agent_count, 1);
    }

    #[test]
    fn test_execution_strategy_large_batch() {
        let strategy = determine_execution_strategy(20, 2, false);
        assert!(strategy.use_swarm);
        assert_eq!(strategy.agent_count, 4);
        assert!(strategy.estimated_speedup > 1.0);
    }

    #[test]
    fn test_execution_strategy_manifest_changed() {
        let strategy = determine_execution_strategy(50, 5, true);
        assert!(!strategy.use_swarm);
    }

    #[test]
    fn test_prioritization_high_risk() {
        let manifest = create_test_manifest();
        let bridge = SwarmExecutorBridge::new(4, manifest, vec!["high-risk-rule".to_string()]);

        let priority = bridge.get_agent_prioritization("high-risk-rule");
        assert_eq!(priority, 10);
    }

    #[test]
    fn test_prioritization_normal() {
        let manifest = create_test_manifest();
        let bridge = SwarmExecutorBridge::new(4, manifest, vec![]);

        let priority = bridge.get_agent_prioritization("normal-rule");
        assert_eq!(priority, 1);
    }

    #[test]
    fn test_speedup_estimation() {
        let manifest = create_test_manifest();
        let bridge = SwarmExecutorBridge::new(4, manifest, vec![]);

        let speedup = bridge.estimate_concurrent_speedup(20);
        assert!(speedup > 1.0);
        assert!(speedup < 4.0);
    }
}
