use serde::{Deserialize, Serialize};

/// Complete audit trail for algorithm execution results
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ProvenanceChain {
    /// BLAKE3 hash of the input event log (64 hex chars)
    pub input_hash: String,

    /// BLAKE3 hash of the resolved configuration (64 hex chars)
    pub config_hash: String,

    /// BLAKE3 hash of the execution plan (64 hex chars)
    pub plan_hash: String,

    /// BLAKE3 hash of the output payload (64 hex chars)
    pub output_hash: String,

    /// BLAKE3 hash of combined input/config/plan/output (64 hex chars)
    pub combined_hash: String,

    /// Algorithm identifier (e.g., "dfg", "inductive_miner")
    pub algorithm_id: String,

    /// Algorithm version (semver or CalVer)
    pub algorithm_version: String,

    /// Backend identifier (e.g., "wasm", "pm4wasm", "pm4py")
    pub backend_id: String,

    /// Kernel/orchestration version
    pub kernel_version: String,

    /// Content hash of the WASM binary (64 hex chars)
    pub wasm_build_hash: String,
}

impl ProvenanceChain {
    pub fn builder() -> ProvenanceChainBuilder {
        ProvenanceChainBuilder::default()
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new(
        input_hash: String,
        config_hash: String,
        plan_hash: String,
        output_hash: String,
        combined_hash: String,
        algorithm_id: String,
        algorithm_version: String,
        backend_id: String,
        kernel_version: String,
        wasm_build_hash: String,
    ) -> Self {
        ProvenanceChain {
            input_hash,
            config_hash,
            plan_hash,
            output_hash,
            combined_hash,
            algorithm_id,
            algorithm_version,
            backend_id,
            kernel_version,
            wasm_build_hash,
        }
    }

    /// Verify all 10 fields are present and non-empty
    pub fn validate(&self) -> Result<(), String> {
        if self.input_hash.is_empty() {
            return Err("input_hash is empty".to_string());
        }
        if self.config_hash.is_empty() {
            return Err("config_hash is empty".to_string());
        }
        if self.plan_hash.is_empty() {
            return Err("plan_hash is empty".to_string());
        }
        if self.output_hash.is_empty() {
            return Err("output_hash is empty".to_string());
        }
        if self.combined_hash.is_empty() {
            return Err("combined_hash is empty".to_string());
        }
        if self.algorithm_id.is_empty() {
            return Err("algorithm_id is empty".to_string());
        }
        if self.algorithm_version.is_empty() {
            return Err("algorithm_version is empty".to_string());
        }
        if self.backend_id.is_empty() {
            return Err("backend_id is empty".to_string());
        }
        if self.kernel_version.is_empty() {
            return Err("kernel_version is empty".to_string());
        }
        if self.wasm_build_hash.is_empty() {
            return Err("wasm_build_hash is empty".to_string());
        }
        Ok(())
    }

    /// All hashes must be 64 hex characters (BLAKE3 256-bit)
    pub fn validate_hash_format(&self) -> Result<(), String> {
        let hashes = vec![
            ("input_hash", &self.input_hash),
            ("config_hash", &self.config_hash),
            ("plan_hash", &self.plan_hash),
            ("output_hash", &self.output_hash),
            ("combined_hash", &self.combined_hash),
            ("wasm_build_hash", &self.wasm_build_hash),
        ];

        for (name, hash) in hashes {
            if hash.len() != 64 {
                return Err(format!(
                    "{} has incorrect length: {} (expected 64)",
                    name,
                    hash.len()
                ));
            }
            if !hash.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(format!("{} contains non-hex characters", name));
            }
        }
        Ok(())
    }
}

/// Builder for ProvenanceChain to avoid too-many-arguments clippy lint
#[derive(Default)]
pub struct ProvenanceChainBuilder {
    input_hash: Option<String>,
    config_hash: Option<String>,
    plan_hash: Option<String>,
    output_hash: Option<String>,
    combined_hash: Option<String>,
    algorithm_id: Option<String>,
    algorithm_version: Option<String>,
    backend_id: Option<String>,
    kernel_version: Option<String>,
    wasm_build_hash: Option<String>,
}

impl ProvenanceChainBuilder {
    pub fn input_hash(mut self, hash: String) -> Self {
        self.input_hash = Some(hash);
        self
    }

    pub fn config_hash(mut self, hash: String) -> Self {
        self.config_hash = Some(hash);
        self
    }

    pub fn plan_hash(mut self, hash: String) -> Self {
        self.plan_hash = Some(hash);
        self
    }

    pub fn output_hash(mut self, hash: String) -> Self {
        self.output_hash = Some(hash);
        self
    }

    pub fn combined_hash(mut self, hash: String) -> Self {
        self.combined_hash = Some(hash);
        self
    }

    pub fn algorithm_id(mut self, id: String) -> Self {
        self.algorithm_id = Some(id);
        self
    }

    pub fn algorithm_version(mut self, version: String) -> Self {
        self.algorithm_version = Some(version);
        self
    }

    pub fn backend_id(mut self, id: String) -> Self {
        self.backend_id = Some(id);
        self
    }

    pub fn kernel_version(mut self, version: String) -> Self {
        self.kernel_version = Some(version);
        self
    }

    pub fn wasm_build_hash(mut self, hash: String) -> Self {
        self.wasm_build_hash = Some(hash);
        self
    }

    pub fn build(self) -> Result<ProvenanceChain, String> {
        Ok(ProvenanceChain {
            input_hash: self.input_hash.ok_or("input_hash is required")?,
            config_hash: self.config_hash.ok_or("config_hash is required")?,
            plan_hash: self.plan_hash.ok_or("plan_hash is required")?,
            output_hash: self.output_hash.ok_or("output_hash is required")?,
            combined_hash: self.combined_hash.ok_or("combined_hash is required")?,
            algorithm_id: self.algorithm_id.ok_or("algorithm_id is required")?,
            algorithm_version: self
                .algorithm_version
                .ok_or("algorithm_version is required")?,
            backend_id: self.backend_id.ok_or("backend_id is required")?,
            kernel_version: self.kernel_version.ok_or("kernel_version is required")?,
            wasm_build_hash: self.wasm_build_hash.ok_or("wasm_build_hash is required")?,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_provenance() -> ProvenanceChain {
        ProvenanceChain::new(
            "a".repeat(64),
            "b".repeat(64),
            "c".repeat(64),
            "d".repeat(64),
            "e".repeat(64),
            "dfg".to_string(),
            "26.4.10".to_string(),
            "wasm".to_string(),
            "26.4.10".to_string(),
            "f".repeat(64),
        )
    }

    #[test]
    fn test_provenance_validation() {
        let prov = create_test_provenance();
        assert!(prov.validate().is_ok());
    }

    #[test]
    fn test_provenance_hash_format() {
        let prov = create_test_provenance();
        assert!(prov.validate_hash_format().is_ok());
    }

    #[test]
    fn test_provenance_empty_hash() {
        let mut prov = create_test_provenance();
        prov.input_hash = String::new();
        assert!(prov.validate().is_err());
    }

    #[test]
    fn test_provenance_invalid_hash_length() {
        let mut prov = create_test_provenance();
        prov.input_hash = "a".repeat(32); // Too short
        assert!(prov.validate_hash_format().is_err());
    }
}
