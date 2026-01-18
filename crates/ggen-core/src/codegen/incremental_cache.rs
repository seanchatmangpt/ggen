use crate::graph::Graph;
use crate::manifest::{GenerationRule, GgenManifest};
use ggen_utils::error::Result;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub struct IncrementalCache {
    cache_dir: PathBuf,
    manifest_hash: String,
    ontology_hash: String,
    rule_hashes: HashMap<String, String>,
    inference_state_hash: String,
}

pub struct CacheInvalidation {
    pub manifest_changed: bool,
    pub ontology_changed: bool,
    pub changed_rules: Vec<String>,
    pub inference_state_changed: bool,
    pub should_rerun_all: bool,
}

impl IncrementalCache {
    pub fn new(cache_dir: PathBuf) -> Self {
        Self {
            cache_dir,
            manifest_hash: String::new(),
            ontology_hash: String::new(),
            rule_hashes: HashMap::new(),
            inference_state_hash: String::new(),
        }
    }

    pub fn load_cache_state(&mut self) -> Result<()> {
        let manifest_hash_path = self.cache_dir.join("manifest.sha256");
        let ontology_hash_path = self.cache_dir.join("ontology.sha256");
        let rules_hash_path = self.cache_dir.join("rules.sha256");
        let inference_hash_path = self.cache_dir.join("inference_state.sha256");

        if manifest_hash_path.exists() {
            self.manifest_hash = fs::read_to_string(&manifest_hash_path)?;
        }

        if ontology_hash_path.exists() {
            self.ontology_hash = fs::read_to_string(&ontology_hash_path)?;
        }

        if rules_hash_path.exists() {
            let rules_content = fs::read_to_string(&rules_hash_path)?;
            for line in rules_content.lines() {
                if let Some((name, hash)) = line.split_once('=') {
                    self.rule_hashes.insert(name.to_string(), hash.to_string());
                }
            }
        }

        if inference_hash_path.exists() {
            self.inference_state_hash = fs::read_to_string(&inference_hash_path)?;
        }

        Ok(())
    }

    pub fn save_cache_state(
        &self, manifest: &GgenManifest, ontology_content: &str, inference_graph: &Graph,
    ) -> Result<()> {
        fs::create_dir_all(&self.cache_dir)?;

        // Save manifest hash
        let manifest_hash = Self::hash_manifest(manifest);
        fs::write(self.cache_dir.join("manifest.sha256"), &manifest_hash)?;

        // Save ontology hash
        let ontology_hash = Self::hash_string(ontology_content);
        fs::write(self.cache_dir.join("ontology.sha256"), &ontology_hash)?;

        // Save rule hashes
        let mut rules_content = String::new();
        for rule in &manifest.generation.rules {
            let rule_hash = Self::hash_generation_rule(rule);
            rules_content.push_str(&format!("{}={}\n", rule.name, rule_hash));
        }
        fs::write(self.cache_dir.join("rules.sha256"), &rules_content)?;

        // Save inference state hash
        let inference_hash = Self::hash_graph_state(inference_graph);
        fs::write(
            self.cache_dir.join("inference_state.sha256"),
            &inference_hash,
        )?;

        Ok(())
    }

    pub fn check_invalidation(
        &self, manifest: &GgenManifest, ontology_content: &str, inference_graph: &Graph,
    ) -> CacheInvalidation {
        let manifest_changed = self.manifest_hash != Self::hash_manifest(manifest);
        let ontology_changed = self.ontology_hash != Self::hash_string(ontology_content);
        let inference_state_changed =
            self.inference_state_hash != Self::hash_graph_state(inference_graph);

        let mut changed_rules = vec![];
        for rule in &manifest.generation.rules {
            let current_hash = Self::hash_generation_rule(rule);
            let empty_hash = String::new();
            let cached_hash = self.rule_hashes.get(&rule.name).unwrap_or(&empty_hash);

            if current_hash != *cached_hash {
                changed_rules.push(rule.name.clone());
            }
        }

        let should_rerun_all = manifest_changed || ontology_changed || inference_state_changed;

        CacheInvalidation {
            manifest_changed,
            ontology_changed,
            changed_rules,
            inference_state_changed,
            should_rerun_all,
        }
    }

    pub fn get_rules_to_rerun(
        &self, manifest: &GgenManifest, invalidation: &CacheInvalidation,
        rule_dependencies: &HashMap<String, Vec<String>>,
    ) -> Vec<String> {
        if invalidation.should_rerun_all {
            return manifest
                .generation
                .rules
                .iter()
                .map(|r| r.name.clone())
                .collect();
        }

        let mut to_rerun = invalidation.changed_rules.clone();
        let mut visited = std::collections::HashSet::new();

        // Propagate changes to dependent rules
        for rule_name in invalidation.changed_rules.iter() {
            Self::propagate_changes(rule_name, rule_dependencies, &mut to_rerun, &mut visited);
        }

        to_rerun
    }

    fn propagate_changes(
        rule_name: &str, dependencies: &HashMap<String, Vec<String>>, to_rerun: &mut Vec<String>,
        visited: &mut std::collections::HashSet<String>,
    ) {
        if visited.contains(rule_name) {
            return;
        }

        visited.insert(rule_name.to_string());

        // Find all rules that depend on this rule
        for (dependent, deps) in dependencies {
            if deps.contains(&rule_name.to_string()) && !to_rerun.contains(dependent) {
                to_rerun.push(dependent.clone());
                Self::propagate_changes(dependent, dependencies, to_rerun, visited);
            }
        }
    }

    fn hash_manifest(manifest: &GgenManifest) -> String {
        let content = format!(
            "{:?}{:?}{:?}{:?}",
            manifest.project, manifest.ontology, manifest.inference, manifest.generation,
        );
        Self::hash_string(&content)
    }

    fn hash_generation_rule(rule: &GenerationRule) -> String {
        let content = format!("{:?}", rule);
        Self::hash_string(&content)
    }

    fn hash_graph_state(graph: &Graph) -> String {
        let content = format!("{} triples", graph.len());
        Self::hash_string(&content)
    }

    fn hash_string(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_consistency() {
        let content = "test content";
        let hash1 = IncrementalCache::hash_string(content);
        let hash2 = IncrementalCache::hash_string(content);
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_hash_difference() {
        let content1 = "test content 1";
        let content2 = "test content 2";
        let hash1 = IncrementalCache::hash_string(content1);
        let hash2 = IncrementalCache::hash_string(content2);
        assert_ne!(hash1, hash2);
    }
}
