use crate::codegen::ExecutionProof;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Clone)]
pub struct ProofEntry {
    pub execution_id: String,
    pub timestamp_ms: u64,
    pub proof: ExecutionProof,
}

pub struct ProofArchive {
    archive_dir: PathBuf,
}

impl ProofArchive {
    pub fn new(archive_dir: PathBuf) -> Result<Self> {
        fs::create_dir_all(&archive_dir)?;
        Ok(Self { archive_dir })
    }

    pub fn store_proof(&self, proof: &ExecutionProof) -> Result<()> {
        let proof_file = self.archive_dir.join(format!("{}.json", proof.execution_id));
        let json = serde_json::to_string_pretty(proof)?;
        fs::write(proof_file, json)?;
        Ok(())
    }

    pub fn load_proof(&self, execution_id: &str) -> Result<Option<ExecutionProof>> {
        let proof_file = self.archive_dir.join(format!("{}.json", execution_id));
        if !proof_file.exists() {
            return Ok(None);
        }

        let json = fs::read_to_string(proof_file)?;
        let proof = serde_json::from_str(&json)?;
        Ok(Some(proof))
    }

    pub fn list_proofs(&self) -> Result<Vec<ProofEntry>> {
        let mut entries = vec![];

        for entry in fs::read_dir(&self.archive_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.extension().map_or(false, |ext| ext == "json") {
                if let Ok(json) = fs::read_to_string(&path) {
                    if let Ok(proof) = serde_json::from_str::<ExecutionProof>(&json) {
                        entries.push(ProofEntry {
                            execution_id: proof.execution_id.clone(),
                            timestamp_ms: proof.timestamp_ms,
                            proof,
                        });
                    }
                }
            }
        }

        entries.sort_by_key(|e| e.timestamp_ms);
        Ok(entries)
    }

    pub fn verify_chain(&self) -> Result<ChainVerification> {
        let entries = self.list_proofs()?;

        if entries.is_empty() {
            return Ok(ChainVerification {
                is_valid: true,
                proof_count: 0,
                determinism_violations: vec![],
                last_manifest_hash: None,
                last_output_hash: None,
            });
        }

        let mut determinism_violations = vec![];
        let mut last_manifest = entries[0].proof.manifest_hash.clone();
        let mut last_output = entries[0].proof.output_hash.clone();

        for i in 1..entries.len() {
            let current = &entries[i];
            let previous = &entries[i - 1];

            if previous.proof.manifest_hash == current.proof.manifest_hash
                && previous.proof.ontology_hash == current.proof.ontology_hash
            {
                if previous.proof.output_hash != current.proof.output_hash {
                    determinism_violations.push(DeterminismViolation {
                        execution_id_previous: previous.execution_id.clone(),
                        execution_id_current: current.execution_id.clone(),
                        reason: "Same manifest/ontology produced different output".to_string(),
                    });
                }
            }

            last_manifest = current.proof.manifest_hash.clone();
            last_output = current.proof.output_hash.clone();
        }

        let is_valid = determinism_violations.is_empty();

        Ok(ChainVerification {
            is_valid,
            proof_count: entries.len(),
            determinism_violations,
            last_manifest_hash: Some(last_manifest),
            last_output_hash: Some(last_output),
        })
    }

    pub fn get_latest_proof(&self) -> Result<Option<ExecutionProof>> {
        let entries = self.list_proofs()?;
        Ok(entries.last().map(|e| e.proof.clone()))
    }
}

pub struct ChainVerification {
    pub is_valid: bool,
    pub proof_count: usize,
    pub determinism_violations: Vec<DeterminismViolation>,
    pub last_manifest_hash: Option<String>,
    pub last_output_hash: Option<String>,
}

pub struct DeterminismViolation {
    pub execution_id_previous: String,
    pub execution_id_current: String,
    pub reason: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_proof_archive_creation() {
        let temp = TempDir::new().unwrap();
        let _archive = ProofArchive::new(temp.path().to_path_buf()).unwrap();
        assert!(temp.path().exists());
    }

    #[test]
    fn test_proof_persistence() {
        let temp = TempDir::new().unwrap();
        let archive = ProofArchive::new(temp.path().to_path_buf()).unwrap();

        let proof = ExecutionProof {
            execution_id: "test-1".to_string(),
            timestamp_ms: 1000,
            manifest_hash: "hash1".to_string(),
            ontology_hash: "ohash1".to_string(),
            rules_executed: vec![],
            output_hash: "out1".to_string(),
            execution_duration_ms: 100,
            determinism_signature: "sig1".to_string(),
        };

        archive.store_proof(&proof).unwrap();
        let loaded = archive.load_proof("test-1").unwrap().unwrap();

        assert_eq!(loaded.execution_id, proof.execution_id);
        assert_eq!(loaded.manifest_hash, proof.manifest_hash);
    }

    #[test]
    fn test_chain_verification_empty() {
        let temp = TempDir::new().unwrap();
        let archive = ProofArchive::new(temp.path().to_path_buf()).unwrap();
        let verification = archive.verify_chain().unwrap();

        assert!(verification.is_valid);
        assert_eq!(verification.proof_count, 0);
    }

    #[test]
    fn test_determinism_violation_detection() {
        let temp = TempDir::new().unwrap();
        let archive = ProofArchive::new(temp.path().to_path_buf()).unwrap();

        let proof1 = ExecutionProof {
            execution_id: "exec-1".to_string(),
            timestamp_ms: 1000,
            manifest_hash: "same-manifest".to_string(),
            ontology_hash: "same-ontology".to_string(),
            rules_executed: vec![],
            output_hash: "output-1".to_string(),
            execution_duration_ms: 100,
            determinism_signature: "sig1".to_string(),
        };

        let proof2 = ExecutionProof {
            execution_id: "exec-2".to_string(),
            timestamp_ms: 2000,
            manifest_hash: "same-manifest".to_string(),
            ontology_hash: "same-ontology".to_string(),
            rules_executed: vec![],
            output_hash: "output-2".to_string(),
            execution_duration_ms: 100,
            determinism_signature: "sig2".to_string(),
        };

        archive.store_proof(&proof1).unwrap();
        archive.store_proof(&proof2).unwrap();

        let verification = archive.verify_chain().unwrap();

        assert!(!verification.is_valid);
        assert_eq!(verification.determinism_violations.len(), 1);
    }
}
