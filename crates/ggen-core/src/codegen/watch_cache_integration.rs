use crate::codegen::IncrementalCache;
use crate::graph::Graph;
use crate::manifest::GgenManifest;
use ggen_utils::error::Result;
use std::fs;
use std::path::Path;

pub struct WatchCacheIntegration;

impl WatchCacheIntegration {
    pub fn detect_affected_rules(
        manifest: &GgenManifest, base_path: &Path, cache: &IncrementalCache,
    ) -> Result<AffectedRulesAnalysis> {
        // Read current ontology content
        let ontology_path = base_path.join(&manifest.ontology.source);
        let ontology_content = fs::read_to_string(&ontology_path).unwrap_or_default();

        // Check what changed (use empty graph for inference state)
        let empty_graph =
            Graph::new().map_err(|e| ggen_utils::error::Error::new(&e.to_string()))?;
        let invalidation = cache.check_invalidation(manifest, &ontology_content, &empty_graph);

        let mut affected_rules = vec![];
        let mut high_impact_rules = vec![];
        let mut unaffected_rules = vec![];

        for rule in &manifest.generation.rules {
            let should_rerun = invalidation.changed_rules.iter().any(|r| r == &rule.name);

            if should_rerun {
                affected_rules.push(rule.name.clone());

                if !manifest.ontology.imports.is_empty() {
                    high_impact_rules.push(rule.name.clone());
                }
            } else {
                unaffected_rules.push(rule.name.clone());
            }
        }

        Ok(AffectedRulesAnalysis {
            manifest_changed: invalidation.manifest_changed,
            ontology_changed: invalidation.ontology_changed,
            inference_state_changed: invalidation.inference_state_changed,
            affected_rule_count: affected_rules.len(),
            unaffected_rule_count: unaffected_rules.len(),
            affected_rules,
            high_impact_rules,
            unaffected_rules,
            rerun_all: invalidation.should_rerun_all,
        })
    }

    pub fn get_rule_execution_order(
        analysis: &AffectedRulesAnalysis, manifest: &GgenManifest,
    ) -> Vec<String> {
        if analysis.rerun_all {
            return manifest
                .generation
                .rules
                .iter()
                .map(|r| r.name.clone())
                .collect();
        }

        let mut ordered = vec![];

        for rule in &manifest.generation.rules {
            if analysis.affected_rules.contains(&rule.name) {
                ordered.push(rule.name.clone());
            }
        }

        ordered
    }

    pub fn estimate_speedup(analysis: &AffectedRulesAnalysis, total_rules: usize) -> f64 {
        if total_rules == 0 {
            return 1.0;
        }

        let unaffected_ratio = analysis.unaffected_rule_count as f64 / total_rules as f64;
        1.0 + (unaffected_ratio * 4.0)
    }
}

pub struct AffectedRulesAnalysis {
    pub manifest_changed: bool,
    pub ontology_changed: bool,
    pub inference_state_changed: bool,
    pub affected_rule_count: usize,
    pub unaffected_rule_count: usize,
    pub affected_rules: Vec<String>,
    pub high_impact_rules: Vec<String>,
    pub unaffected_rules: Vec<String>,
    pub rerun_all: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_speedup_calculation_no_changes() {
        let analysis = AffectedRulesAnalysis {
            manifest_changed: false,
            ontology_changed: false,
            inference_state_changed: false,
            affected_rule_count: 0,
            unaffected_rule_count: 10,
            affected_rules: vec![],
            high_impact_rules: vec![],
            unaffected_rules: (0..10).map(|i| format!("rule-{}", i)).collect(),
            rerun_all: false,
        };

        let speedup = WatchCacheIntegration::estimate_speedup(&analysis, 10);
        assert!(speedup > 1.0);
    }

    #[test]
    fn test_speedup_calculation_all_changed() {
        let analysis = AffectedRulesAnalysis {
            manifest_changed: true,
            ontology_changed: false,
            inference_state_changed: false,
            affected_rule_count: 10,
            unaffected_rule_count: 0,
            affected_rules: (0..10).map(|i| format!("rule-{}", i)).collect(),
            high_impact_rules: vec![],
            unaffected_rules: vec![],
            rerun_all: true,
        };

        let speedup = WatchCacheIntegration::estimate_speedup(&analysis, 10);
        assert_eq!(speedup, 1.0);
    }

    #[test]
    fn test_rule_execution_order_partial() {
        use std::collections::BTreeMap;

        let mut manifest = GgenManifest {
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
        };

        manifest.generation.rules = vec![
            crate::manifest::GenerationRule {
                name: "rule-1".to_string(),
                query: crate::manifest::QuerySource::Inline {
                    inline: "q1".to_string(),
                },
                template: crate::manifest::TemplateSource::Inline {
                    inline: "t1".to_string(),
                },
                output_file: "out1.rs".to_string(),
                skip_empty: false,
                mode: Default::default(),
                when: None,
            },
            crate::manifest::GenerationRule {
                name: "rule-2".to_string(),
                query: crate::manifest::QuerySource::Inline {
                    inline: "q2".to_string(),
                },
                template: crate::manifest::TemplateSource::Inline {
                    inline: "t2".to_string(),
                },
                output_file: "out2.rs".to_string(),
                skip_empty: false,
                mode: Default::default(),
                when: None,
            },
        ];

        let analysis = AffectedRulesAnalysis {
            manifest_changed: false,
            ontology_changed: true,
            inference_state_changed: false,
            affected_rule_count: 1,
            unaffected_rule_count: 1,
            affected_rules: vec!["rule-1".to_string()],
            high_impact_rules: vec!["rule-1".to_string()],
            unaffected_rules: vec!["rule-2".to_string()],
            rerun_all: false,
        };

        let order = WatchCacheIntegration::get_rule_execution_order(&analysis, &manifest);
        assert_eq!(order, vec!["rule-1"]);
    }
}
