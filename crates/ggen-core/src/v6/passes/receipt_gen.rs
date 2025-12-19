//! μ₅: Receipt Generation Pass
//!
//! Produces a build receipt binding outputs to inputs via cryptographic hashes.
//! This is the final pass that creates the provenance proof.

use crate::v6::pass::{Pass, PassContext, PassExecution, PassResult, PassType};
use crate::v6::receipt::{BuildReceipt, OutputFile, ReceiptPolicies};
use crate::v6::Epoch;
use ggen_utils::error::{Error, Result};
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use std::time::Instant;

/// μ₅: Receipt generation pass implementation
#[derive(Debug, Clone)]
pub struct ReceiptGenerationPass {
    /// Path to write the receipt
    receipt_path: Option<PathBuf>,

    /// Toolchain version to record
    toolchain_version: String,

    /// Policies to include
    policies: ReceiptPolicies,
}

impl ReceiptGenerationPass {
    /// Create a new receipt generation pass
    pub fn new(toolchain_version: impl Into<String>) -> Self {
        Self {
            receipt_path: None,
            toolchain_version: toolchain_version.into(),
            policies: ReceiptPolicies::default(),
        }
    }

    /// Set the receipt output path
    pub fn with_receipt_path(mut self, path: PathBuf) -> Self {
        self.receipt_path = Some(path);
        self
    }

    /// Set policies
    pub fn with_policies(mut self, policies: ReceiptPolicies) -> Self {
        self.policies = policies;
        self
    }

    /// Create output file records from generated files
    fn create_output_records(
        &self, ctx: &PassContext<'_>, pass_name: &str,
    ) -> Result<Vec<OutputFile>> {
        let mut outputs = Vec::new();

        for rel_path in &ctx.generated_files {
            let full_path = ctx.output_dir.join(rel_path);

            if !full_path.exists() {
                continue;
            }

            let content = std::fs::read(&full_path).map_err(|e| {
                Error::new(&format!(
                    "Failed to read output file '{}': {}",
                    full_path.display(),
                    e
                ))
            })?;

            let hash = format!("{:x}", Sha256::digest(&content));

            outputs.push(OutputFile {
                path: rel_path.clone(),
                hash,
                size_bytes: content.len(),
                produced_by: pass_name.to_string(),
            });
        }

        Ok(outputs)
    }
}

impl Default for ReceiptGenerationPass {
    fn default() -> Self {
        Self::new(env!("CARGO_PKG_VERSION"))
    }
}

impl Pass for ReceiptGenerationPass {
    fn pass_type(&self) -> PassType {
        PassType::Receipt
    }

    fn name(&self) -> &str {
        "μ₅:receipt"
    }

    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();

        // Create output records
        let outputs = self.create_output_records(ctx, "μ₃:emission")?;

        // Create a minimal epoch from context (in real usage, this would be passed in)
        let epoch = Epoch {
            id: format!("{:x}", Sha256::digest(ctx.project_name.as_bytes())),
            timestamp: chrono::Utc::now().to_rfc3339(),
            inputs: std::collections::BTreeMap::new(),
            total_triples: 0,
        };

        // Create the receipt
        let receipt = BuildReceipt::new(&epoch, Vec::new(), outputs, &self.toolchain_version)
            .with_policies(self.policies.clone());

        // Write receipt if path specified
        if let Some(ref receipt_path) = self.receipt_path {
            let full_receipt_path = ctx.output_dir.join(receipt_path);

            if let Some(parent) = full_receipt_path.parent() {
                std::fs::create_dir_all(parent).map_err(|e| {
                    Error::new(&format!(
                        "Failed to create receipt directory: {}",
                        e
                    ))
                })?;
            }

            receipt.write_to_file(&full_receipt_path)?;
        }

        let duration = start.elapsed();
        Ok(PassResult::success()
            .with_files(vec![self.receipt_path.clone().unwrap_or_default()])
            .with_duration(duration))
    }
}

/// Builder for creating a complete receipt with all pass information
#[allow(dead_code)]
pub struct ReceiptBuilder {
    epoch: Option<Epoch>,
    passes: Vec<PassExecution>,
    outputs: Vec<OutputFile>,
    toolchain_version: String,
    policies: ReceiptPolicies,
}

#[allow(dead_code)]
impl ReceiptBuilder {
    /// Create a new receipt builder
    pub fn new(toolchain_version: impl Into<String>) -> Self {
        Self {
            epoch: None,
            passes: Vec::new(),
            outputs: Vec::new(),
            toolchain_version: toolchain_version.into(),
            policies: ReceiptPolicies::default(),
        }
    }

    /// Set the input epoch
    pub fn with_epoch(mut self, epoch: Epoch) -> Self {
        self.epoch = Some(epoch);
        self
    }

    /// Add a pass execution record
    pub fn add_pass(mut self, execution: PassExecution) -> Self {
        self.passes.push(execution);
        self
    }

    /// Add pass executions
    pub fn with_passes(mut self, passes: Vec<PassExecution>) -> Self {
        self.passes = passes;
        self
    }

    /// Add output files
    pub fn with_outputs(mut self, outputs: Vec<OutputFile>) -> Self {
        self.outputs = outputs;
        self
    }

    /// Set policies
    pub fn with_policies(mut self, policies: ReceiptPolicies) -> Self {
        self.policies = policies;
        self
    }

    /// Build the receipt
    pub fn build(self) -> Result<BuildReceipt> {
        let epoch = self.epoch.ok_or_else(|| Error::new("Epoch is required"))?;

        Ok(BuildReceipt::new(
            &epoch,
            self.passes,
            self.outputs,
            &self.toolchain_version,
        )
        .with_policies(self.policies))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use tempfile::TempDir;

    #[test]
    fn test_receipt_generation_empty() {
        let graph = Graph::new().unwrap();
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir).unwrap();

        let pass = ReceiptGenerationPass::new("6.0.0");

        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir)
            .with_project("test".to_string(), "1.0.0".to_string());

        let result = pass.execute(&mut ctx).unwrap();
        assert!(result.success);
    }

    #[test]
    fn test_receipt_generation_with_files() {
        let graph = Graph::new().unwrap();
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir).unwrap();

        // Create a generated file
        std::fs::write(output_dir.join("model.rs"), "pub struct Model {}").unwrap();

        let pass = ReceiptGenerationPass::new("6.0.0")
            .with_receipt_path(PathBuf::from("receipt.json"));

        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone())
            .with_project("test".to_string(), "1.0.0".to_string());
        ctx.generated_files.push(PathBuf::from("model.rs"));

        let result = pass.execute(&mut ctx).unwrap();
        assert!(result.success);
        assert!(output_dir.join("receipt.json").exists());
    }

    #[test]
    fn test_receipt_builder() {
        let epoch = Epoch {
            id: "0123456789abcdef".repeat(4), // 64 chars
            timestamp: chrono::Utc::now().to_rfc3339(),
            inputs: std::collections::BTreeMap::new(),
            total_triples: 0,
        };

        let receipt = ReceiptBuilder::new("6.0.0")
            .with_epoch(epoch)
            .with_outputs(vec![OutputFile {
                path: PathBuf::from("model.rs"),
                hash: "fedcba9876543210".repeat(4), // 64 chars
                size_bytes: 100,
                produced_by: "μ₃:emission".to_string(),
            }])
            .build()
            .unwrap();

        assert_eq!(receipt.toolchain_version, "6.0.0");
        assert_eq!(receipt.outputs.len(), 1);
    }
}
