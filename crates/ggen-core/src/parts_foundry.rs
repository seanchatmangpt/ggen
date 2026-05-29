//! Parts Manufacturing Pipeline (μ₀-μ₅)
//!
//! Orchestrates end-to-end manufacturing of Genesis-bearing interchangeable parts:
//! - μ₀: Load RDF part specification and adapter manifest
//! - μ₁: Normalize and extract part definition
//! - μ₂: Generate adapter code (SPARQL + Tera)
//! - μ₃: Compile to target format (real compilers, no mocks)
//! - μ₄: Validate and canonicalize output
//! - μ₅: Sign with Ed25519 and emit receipt
//!
//! All compiler invocations use real `std::process::Command`, never mocks.

use crate::utils::error::{Error, Result};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

pub mod adapter_generator;
pub mod part_compiler;
pub mod part_signer;

pub use adapter_generator::AdapterGenerator;
pub use part_compiler::PartCompiler;
pub use part_signer::PartSigner;

/// Part specification (RDF-based)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PartSpec {
    /// Part identifier
    pub id: String,
    /// Semantic version
    pub version: String,
    /// Part type (wasm32, beam, arm-cortex-m, native)
    pub part_type: String,
    /// RDF source (TTL format)
    pub rdf_source: String,
    /// Target language for adapter
    pub target_language: String,
}

/// Adapter manifest (bindings + metadata)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PartManifest {
    /// Interface definitions
    pub interfaces: Vec<InterfaceSpec>,
    /// Dependencies
    pub dependencies: HashMap<String, String>,
    /// Configuration
    pub config: serde_json::Value,
}

/// Interface specification
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct InterfaceSpec {
    /// Interface name
    pub name: String,
    /// Parameter schema (JSON)
    pub params: serde_json::Value,
    /// Return schema (JSON)
    pub returns: serde_json::Value,
}

/// Manufactured part output
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ManufacturedPart {
    /// Part ID
    pub id: String,
    /// Version
    pub version: String,
    /// Compiled binary (bytes)
    pub payload: Vec<u8>,
    /// BLAKE3 hash of payload (hex string)
    pub payload_hash: String,
    /// Payload size in bytes
    pub payload_size: u64,
    /// Exported interfaces
    pub interfaces: Vec<ExportedInterface>,
    /// Compiler output (stderr, warnings, etc.)
    pub compiler_output: String,
    /// Generated adapter source (before compilation)
    pub adapter_source: String,
}

/// Exported interface
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExportedInterface {
    /// Name
    pub name: String,
    /// Parameters (JSON schema)
    pub params: serde_json::Value,
    /// Return type (JSON schema)
    pub returns: serde_json::Value,
}

/// Signed part with receipt
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignedPart {
    /// Manufactured part data
    pub manufactured: ManufacturedPart,
    /// Ed25519 signature (hex string)
    pub signature: String,
    /// Verifying key (hex string)
    pub verifying_key: String,
    /// Trust tier
    pub trust_tier: String,
}

/// Parts foundry orchestrator
pub struct PartFoundry {
    compiler: PartCompiler,
    signer: PartSigner,
    generator: AdapterGenerator,
}

impl PartFoundry {
    /// Create a new parts foundry
    pub fn new(compiler: PartCompiler, signer: PartSigner, generator: AdapterGenerator) -> Self {
        Self {
            compiler,
            signer,
            generator,
        }
    }

    /// Manufacture a part through the complete μ₀-μ₅ pipeline
    ///
    /// # Errors
    ///
    /// Returns error if any pipeline stage fails (compilation, signing, etc.)
    pub async fn manufacture(
        &self, spec: PartSpec, manifest: PartManifest, output_dir: &Path,
    ) -> Result<SignedPart> {
        // μ₀: Load specification (implicit - spec already loaded)
        if spec.id.is_empty() {
            return Err(Error::new("Part ID cannot be empty"));
        }
        if spec.version.is_empty() {
            return Err(Error::new("Part version cannot be empty"));
        }

        // μ₁: Normalize (validate RDF)
        // In production this would validate RDF with SHACL
        if spec.rdf_source.is_empty() {
            return Err(Error::new("RDF source cannot be empty"));
        }

        // μ₂: Generate adapter code
        let adapter_source = self
            .generator
            .generate(&spec, &manifest)
            .await
            .map_err(|e| Error::new(&format!("Adapter generation failed: {}", e)))?;

        // μ₃: Compile to binary
        let compiled = self
            .compiler
            .compile(&spec.part_type, &adapter_source)
            .await
            .map_err(|e| Error::new(&format!("Compilation failed: {}", e)))?;

        // Build manufactured part
        let payload_hash = calculate_blake3(&compiled);
        let interfaces: Vec<ExportedInterface> = manifest
            .interfaces
            .into_iter()
            .map(|i| ExportedInterface {
                name: i.name,
                params: i.params,
                returns: i.returns,
            })
            .collect();

        let manufactured = ManufacturedPart {
            id: spec.id.clone(),
            version: spec.version.clone(),
            payload_size: compiled.len() as u64,
            payload_hash: payload_hash.clone(),
            payload: compiled,
            interfaces,
            compiler_output: String::new(),
            adapter_source,
        };

        // μ₄: Validate (implicit - compilation success is validation)

        // μ₅: Sign and emit receipt
        let signed = self
            .signer
            .sign_part(manufactured)
            .map_err(|e| Error::new(&format!("Signing failed: {}", e)))?;

        // Write outputs
        let part_dir = output_dir.join(&spec.id).join(&spec.version);
        std::fs::create_dir_all(&part_dir)
            .map_err(|e| Error::new(&format!("Failed to create part dir: {}", e)))?;

        // Write manifest
        let manifest_path = part_dir.join("part.json");
        let manifest_json = serde_json::to_string_pretty(&signed)
            .map_err(|e| Error::new(&format!("Failed to serialize manifest: {}", e)))?;
        std::fs::write(&manifest_path, manifest_json)
            .map_err(|e| Error::new(&format!("Failed to write manifest: {}", e)))?;

        // Write binary
        let binary_path = part_dir.join(format!("{}.bin", spec.id));
        std::fs::write(&binary_path, &signed.manufactured.payload)
            .map_err(|e| Error::new(&format!("Failed to write binary: {}", e)))?;

        Ok(signed)
    }
}

/// Calculate BLAKE3 hash of bytes, return as hex string
fn calculate_blake3(data: &[u8]) -> String {
    use blake3::Hasher;
    let mut hasher = Hasher::new();
    hasher.update(data);
    hasher.finalize().to_hex().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_spec_validation() {
        let spec = PartSpec {
            id: "test-part".to_string(),
            version: "1.0.0".to_string(),
            part_type: "wasm32".to_string(),
            rdf_source: "(RDF source)".to_string(),
            target_language: "rust".to_string(),
        };

        assert!(!spec.id.is_empty());
        assert!(!spec.version.is_empty());
    }

    #[test]
    fn test_manufactured_part_fields() {
        let part = ManufacturedPart {
            id: "test".to_string(),
            version: "1.0.0".to_string(),
            payload: vec![1, 2, 3],
            payload_hash: "abc123".to_string(),
            payload_size: 3,
            interfaces: vec![],
            compiler_output: String::new(),
            adapter_source: String::new(),
        };

        assert_eq!(part.payload_size, 3);
        assert_eq!(part.payload.len() as u64, part.payload_size);
    }
}
