//! Audit trail generation for determinism verification
//!
//! Creates comprehensive audit records of generation pipeline execution,
//! enabling verification that the same inputs produce identical outputs.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use std::path::Path;
use std::time::Duration;

/// Complete audit trail for generation verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrail {
    /// Generation timestamp (ISO 8601)
    pub generated_at: String,

    /// ggen version
    pub ggen_version: String,

    /// Input hashes for determinism verification
    pub inputs: AuditInputs,

    /// Pipeline execution log
    pub pipeline: Vec<AuditStep>,

    /// Generated file manifest
    pub outputs: Vec<AuditOutput>,

    /// Overall validation status
    pub validation_passed: bool,

    /// Total duration (ms)
    pub total_duration_ms: u64,
}

/// Hashes of all input files
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditInputs {
    /// SHA256 of ggen.toml
    pub manifest_hash: String,

    /// SHA256 of each ontology file (BTreeMap for determinism)
    pub ontology_hashes: BTreeMap<String, String>,

    /// SHA256 of each template file
    pub template_hashes: BTreeMap<String, String>,

    /// SHA256 of each SPARQL file
    pub query_hashes: BTreeMap<String, String>,
}

/// Record of a pipeline execution step
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditStep {
    /// Step type ("load_ontology", "inference", "construct", "render")
    pub step_type: String,

    /// Rule/file name
    pub name: String,

    /// Duration (ms)
    pub duration_ms: u64,

    /// Triples added (for graph operations)
    #[serde(default)]
    pub triples_added: Option<usize>,

    /// Status ("success", "skipped", "error")
    pub status: String,

    /// Error message if failed
    #[serde(default)]
    pub error: Option<String>,
}

/// Record of a generated output file
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditOutput {
    /// Output file path
    pub path: String,

    /// SHA256 of generated content
    pub content_hash: String,

    /// File size in bytes
    pub size_bytes: usize,

    /// Generation rule that produced this
    pub source_rule: String,
}

/// Builder for constructing audit trails
pub struct AuditTrailBuilder {
    /// Current ggen version
    ggen_version: String,

    /// Input file hashes
    inputs: AuditInputs,

    /// Pipeline execution steps
    pipeline: Vec<AuditStep>,

    /// Generated outputs
    outputs: Vec<AuditOutput>,

    /// Start time for duration calculation
    started_at: std::time::Instant,
}

impl AuditTrailBuilder {
    /// Create a new audit trail builder
    pub fn new() -> Self {
        Self {
            ggen_version: env!("CARGO_PKG_VERSION").to_string(),
            inputs: AuditInputs {
                manifest_hash: String::new(),
                ontology_hashes: BTreeMap::new(),
                template_hashes: BTreeMap::new(),
                query_hashes: BTreeMap::new(),
            },
            pipeline: Vec::new(),
            outputs: Vec::new(),
            started_at: std::time::Instant::now(),
        }
    }

    /// Record input file hashes
    ///
    /// # Arguments
    /// * `manifest` - Path to ggen.toml
    /// * `ontologies` - Paths to ontology files
    /// * `templates` - Paths to template files
    pub fn record_inputs(
        &mut self,
        manifest: &Path,
        ontologies: &[&Path],
        templates: &[&Path],
    ) -> Result<&mut Self> {
        // Hash manifest
        self.inputs.manifest_hash = Self::hash_file(manifest)?;

        // Hash ontologies
        for ont in ontologies {
            let hash = Self::hash_file(ont)?;
            self.inputs
                .ontology_hashes
                .insert(ont.display().to_string(), hash);
        }

        // Hash templates
        for tmpl in templates {
            let hash = Self::hash_file(tmpl)?;
            self.inputs
                .template_hashes
                .insert(tmpl.display().to_string(), hash);
        }

        Ok(self)
    }

    /// Record a pipeline execution step
    ///
    /// # Arguments
    /// * `step_type` - Type of step (e.g., "inference", "render")
    /// * `name` - Name of the rule/file
    /// * `duration` - Execution duration
    /// * `triples` - Optional triple count for graph operations
    /// * `status` - Execution status
    pub fn record_step(
        &mut self,
        step_type: &str,
        name: &str,
        duration: Duration,
        triples: Option<usize>,
        status: &str,
    ) -> &mut Self {
        self.pipeline.push(AuditStep {
            step_type: step_type.to_string(),
            name: name.to_string(),
            duration_ms: duration.as_millis() as u64,
            triples_added: triples,
            status: status.to_string(),
            error: None,
        });
        self
    }

    /// Record a pipeline step that failed
    pub fn record_step_error(
        &mut self,
        step_type: &str,
        name: &str,
        duration: Duration,
        error: &str,
    ) -> &mut Self {
        self.pipeline.push(AuditStep {
            step_type: step_type.to_string(),
            name: name.to_string(),
            duration_ms: duration.as_millis() as u64,
            triples_added: None,
            status: "error".to_string(),
            error: Some(error.to_string()),
        });
        self
    }

    /// Record a generated output file
    ///
    /// # Arguments
    /// * `path` - Output file path
    /// * `content` - Generated content
    /// * `source_rule` - Rule that generated this file
    pub fn record_output(&mut self, path: &Path, content: &str, source_rule: &str) -> &mut Self {
        let hash = Self::hash_string(content);
        self.outputs.push(AuditOutput {
            path: path.display().to_string(),
            content_hash: hash,
            size_bytes: content.len(),
            source_rule: source_rule.to_string(),
        });
        self
    }

    /// Build the final audit trail
    pub fn build(&self, validation_passed: bool) -> AuditTrail {
        let total_duration = self.started_at.elapsed();

        AuditTrail {
            generated_at: chrono::Utc::now().to_rfc3339(),
            ggen_version: self.ggen_version.clone(),
            inputs: self.inputs.clone(),
            pipeline: self.pipeline.clone(),
            outputs: self.outputs.clone(),
            validation_passed,
            total_duration_ms: total_duration.as_millis() as u64,
        }
    }

    /// Write audit trail to a file
    pub fn write_to(trail: &AuditTrail, path: &Path) -> Result<()> {
        let json = serde_json::to_string_pretty(trail)
            .map_err(|e| Error::new(&format!("Failed to serialize audit trail: {}", e)))?;

        std::fs::write(path, json)
            .map_err(|e| Error::new(&format!("Failed to write audit trail: {}", e)))?;

        Ok(())
    }

    /// Calculate SHA256 hash of a file
    fn hash_file(path: &Path) -> Result<String> {
        let content = std::fs::read(path)
            .map_err(|e| Error::new(&format!("Failed to read '{}': {}", path.display(), e)))?;
        Ok(Self::hash_bytes(&content))
    }

    /// Calculate SHA256 hash of a string
    fn hash_string(content: &str) -> String {
        Self::hash_bytes(content.as_bytes())
    }

    /// Calculate SHA256 hash of bytes
    fn hash_bytes(bytes: &[u8]) -> String {
        let mut hasher = Sha256::new();
        hasher.update(bytes);
        format!("{:x}", hasher.finalize())
    }
}

impl Default for AuditTrailBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[test]
    fn test_audit_builder() {
        let mut builder = AuditTrailBuilder::new();

        builder.record_step("inference", "auditable_fields", Duration::from_millis(5), Some(10), "success");
        builder.record_step("render", "structs", Duration::from_millis(15), None, "success");

        let trail = builder.build(true);

        assert_eq!(trail.pipeline.len(), 2);
        assert!(trail.validation_passed);
    }

    #[test]
    fn test_hash_string() {
        let hash1 = AuditTrailBuilder::hash_string("hello world");
        let hash2 = AuditTrailBuilder::hash_string("hello world");
        let hash3 = AuditTrailBuilder::hash_string("different");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
    }

    #[test]
    fn test_record_output() {
        let mut builder = AuditTrailBuilder::new();
        builder.record_output(Path::new("test.rs"), "fn main() {}", "structs");

        let trail = builder.build(true);
        assert_eq!(trail.outputs.len(), 1);
        assert_eq!(trail.outputs[0].path, "test.rs");
        assert_eq!(trail.outputs[0].source_rule, "structs");
    }
}
