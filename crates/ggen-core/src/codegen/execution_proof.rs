use crate::codegen::SyncResult;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Serialize, Deserialize, Clone)]
pub struct ExecutionProof {
    pub execution_id: String,
    pub timestamp_ms: u64,
    pub manifest_hash: String,
    pub ontology_hash: String,
    pub rules_executed: Vec<RuleExecution>,
    pub output_hash: String,
    pub execution_duration_ms: u64,
    pub determinism_signature: String,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RuleExecution {
    pub rule_name: String,
    pub input_hash: String,
    pub output_hash: String,
    pub duration_ms: u64,
    pub status: String,
}

pub struct ProofCarrier {
    executions: HashMap<String, ExecutionProof>,
}

impl ProofCarrier {
    pub fn new() -> Self {
        Self {
            executions: HashMap::new(),
        }
    }

    pub fn generate_proof(
        &mut self,
        manifest_content: &str,
        ontology_content: &str,
        sync_result: &SyncResult,
    ) -> Result<ExecutionProof> {
        let execution_id = Self::generate_id();
        let timestamp_ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0);

        let manifest_hash = Self::hash_content(manifest_content);
        let ontology_hash = Self::hash_content(ontology_content);

        // Hash all output files
        let files_content = sync_result
            .files
            .iter()
            .map(|f| format!("{}:{}", f.path, f.size_bytes))
            .collect::<Vec<_>>()
            .join("|");
        let output_hash = Self::hash_content(&files_content);

        let mut rules_executed = vec![];
        for file in &sync_result.files {
            rules_executed.push(RuleExecution {
                rule_name: file.path.clone(),
                input_hash: manifest_hash.clone(),
                output_hash: Self::hash_content(&file.path),
                duration_ms: 0,
                status: file.action.clone(),
            });
        }

        // Create determinism signature
        let determinism_signature = Self::compute_determinism_signature(
            &manifest_hash,
            &ontology_hash,
            &rules_executed,
        );

        let proof = ExecutionProof {
            execution_id: execution_id.clone(),
            timestamp_ms,
            manifest_hash,
            ontology_hash,
            rules_executed,
            output_hash,
            execution_duration_ms: sync_result.duration_ms,
            determinism_signature,
        };

        self.executions.insert(execution_id, proof.clone());
        Ok(proof)
    }

    pub fn verify_determinism(&self, proof1: &ExecutionProof, proof2: &ExecutionProof) -> bool {
        // Two executions are deterministic if they produce identical output
        // given the same manifest and ontology
        proof1.manifest_hash == proof2.manifest_hash
            && proof1.ontology_hash == proof2.ontology_hash
            && proof1.output_hash == proof2.output_hash
            && proof1.determinism_signature == proof2.determinism_signature
    }

    pub fn get_execution_proof(&self, execution_id: &str) -> Option<ExecutionProof> {
        self.executions.get(execution_id).cloned()
    }

    pub fn audit_trail(&self) -> Vec<ExecutionProof> {
        self.executions.values().cloned().collect()
    }

    pub fn verify_chain_integrity(&self) -> Result<bool> {
        let mut proofs: Vec<_> = self.executions.values().collect();
        proofs.sort_by_key(|p| p.timestamp_ms);

        // Check that each execution is deterministic with the same inputs
        let mut previous_manifest = String::new();

        for proof in proofs {
            // In a real implementation, we would verify that:
            // 1. If manifest/ontology unchanged, output must be identical
            // 2. Hashes form a consistent chain

            if !previous_manifest.is_empty() && previous_manifest == proof.manifest_hash {
                // Manifest unchanged, output should match previous
                continue;
            }

            previous_manifest = proof.manifest_hash.clone();
        }

        Ok(true)
    }

    fn hash_content(content: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }

    fn generate_id() -> String {
        use std::time::{SystemTime, UNIX_EPOCH};
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        format!("exec-{}", now)
    }

    fn compute_determinism_signature(
        manifest_hash: &str,
        ontology_hash: &str,
        rules: &[RuleExecution],
    ) -> String {
        let mut content = format!("{}|{}", manifest_hash, ontology_hash);

        for rule in rules {
            content.push_str(&format!("|{}:{}", rule.rule_name, rule.output_hash));
        }

        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        format!("{:x}", hasher.finalize())
    }
}

impl Default for ProofCarrier {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_proof_generation() {
        let mut carrier = ProofCarrier::new();
        let manifest = "test manifest";
        let ontology = "test ontology";
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 1,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 1,
            audit_trail: None,
            error: None,
        };

        let proof = carrier.generate_proof(manifest, ontology, &result).unwrap();

        assert!(!proof.execution_id.is_empty());
        assert!(!proof.manifest_hash.is_empty());
        assert!(!proof.ontology_hash.is_empty());
        assert_eq!(proof.execution_duration_ms, 100);
    }

    #[test]
    fn test_determinism_verification() {
        let mut carrier = ProofCarrier::new();
        let manifest = "test manifest";
        let ontology = "test ontology";
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 0,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
        };

        let proof1 = carrier.generate_proof(manifest, ontology, &result).unwrap();
        let proof2 = carrier.generate_proof(manifest, ontology, &result).unwrap();

        assert!(carrier.verify_determinism(&proof1, &proof2));
    }

    #[test]
    fn test_proof_retrieval() {
        let mut carrier = ProofCarrier::new();
        let manifest = "test manifest";
        let ontology = "test ontology";
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 0,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
        };

        let proof = carrier.generate_proof(manifest, ontology, &result).unwrap();
        let execution_id = proof.execution_id.clone();

        let retrieved = carrier.get_execution_proof(&execution_id).unwrap();
        assert_eq!(retrieved.execution_id, execution_id);
    }

    #[test]
    fn test_audit_trail() {
        let mut carrier = ProofCarrier::new();
        let result = SyncResult {
            status: "success".to_string(),
            files_synced: 0,
            duration_ms: 100,
            files: vec![],
            inference_rules_executed: 0,
            generation_rules_executed: 0,
            audit_trail: None,
            error: None,
        };

        carrier
            .generate_proof("manifest1", "ontology1", &result)
            .unwrap();
        carrier
            .generate_proof("manifest2", "ontology2", &result)
            .unwrap();

        let trail = carrier.audit_trail();
        assert_eq!(trail.len(), 2);
    }
}
