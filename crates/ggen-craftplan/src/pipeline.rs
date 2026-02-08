//! Five-stage μ pipeline orchestrator
//!
//! This module orchestrates the complete transformation from RDF to Elixir:
//! μ₁ (Normalize) → μ₂ (Extract) → μ₃ (Emit) → μ₄ (Canonicalize) → μ₅ (Receipt)

use crate::canonicalize::Canonicalizer;
use crate::emit::Emitter;
use crate::error::{CraftplanError, Result};
use crate::extract::Extractor;
use crate::models::{ExtractedData, GenerationReceipt};
use crate::normalize::{NormalizedData, Normalizer};
use crate::receipt::ReceiptGenerator;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tracing::{info, instrument};

/// Code generator orchestrating the full μ pipeline
///
/// Implements the transformation A = μ(O) where:
/// - A = Generated Elixir code
/// - μ = Five-stage transformation pipeline
/// - O = RDF ontology
pub struct CodeGenerator {
    /// Output directory for generated code
    output_dir: PathBuf,

    /// Whether to generate receipts
    generate_receipts: bool,
}

impl CodeGenerator {
    /// Create a new code generator
    ///
    /// # Arguments
    /// * `output_dir` - Directory where generated code will be written
    ///
    /// # Examples
    ///
    /// ```
    /// use ggen_craftplan::pipeline::CodeGenerator;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let generator = CodeGenerator::new("output")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new<P: AsRef<Path>>(output_dir: P) -> Result<Self> {
        let output_dir = output_dir.as_ref().to_path_buf();

        // Create output directory
        std::fs::create_dir_all(&output_dir).map_err(|e| CraftplanError::Io {
            path: output_dir.clone(),
            source: e,
        })?;

        Ok(Self {
            output_dir,
            generate_receipts: true,
        })
    }

    /// Generate Elixir code from RDF ontology file
    ///
    /// # Arguments
    /// * `rdf_path` - Path to Turtle (.ttl) RDF file
    ///
    /// # Returns
    /// * `Ok(GenerationReceipt)` - Receipt with cryptographic proofs
    /// * `Err(CraftplanError)` - Generation failed
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use ggen_craftplan::pipeline::CodeGenerator;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let generator = CodeGenerator::new("output")?;
    /// let receipt = generator.generate_from_rdf("ontology.ttl")?;
    /// # Ok(())
    /// # }
    /// ```
    #[instrument(skip(self))]
    pub fn generate_from_rdf<P: AsRef<Path> + std::fmt::Debug>(&self, rdf_path: P) -> Result<GenerationReceipt> {
        let rdf_path = rdf_path.as_ref();
        info!("Starting code generation from: {}", rdf_path.display());

        let start_time = Instant::now();

        // μ₁: Normalize
        info!("Stage μ₁ (Normalize): Validating RDF");
        let mut normalizer = Normalizer::new()?;
        let normalized = self.normalize_stage(&mut normalizer, rdf_path)?;

        // μ₂: Extract
        info!("Stage μ₂ (Extract): Querying RDF graph");
        let mut extractor = Extractor::new(&normalizer)?;
        let extracted = self.extract_stage(&mut extractor, &normalized)?;

        // μ₃: Emit
        info!("Stage μ₃ (Emit): Rendering Elixir code");
        let emitter = Emitter::new(&self.output_dir)?;
        let generated_files = self.emit_stage(&emitter, &normalized, &extracted)?;

        // μ₄: Canonicalize
        info!("Stage μ₄ (Canonicalize): Normalizing output");
        let canonicalizer = Canonicalizer::new();
        let _hashes = self.canonicalize_stage(&canonicalizer, &generated_files)?;

        // μ₅: Receipt
        info!("Stage μ₅ (Receipt): Generating audit trail");
        let receipt_generator = ReceiptGenerator::new();
        let duration_ms = start_time.elapsed().as_millis() as u64;
        let receipt = self.receipt_stage(
            &receipt_generator,
            rdf_path,
            &generated_files,
            extracted.len(),
            duration_ms,
        )?;

        // Write receipt if enabled
        if self.generate_receipts {
            let receipt_path = self.output_dir.join("generation_receipt.json");
            receipt_generator.write_receipt(&receipt, receipt_path.to_str().unwrap())?;
        }

        info!(
            "Generation complete: {} files in {}ms",
            generated_files.len(),
            duration_ms
        );

        Ok(receipt)
    }

    /// μ₁ stage: Normalize RDF
    fn normalize_stage(&self, normalizer: &mut Normalizer, rdf_path: &Path) -> Result<NormalizedData> {
        normalizer.load_rdf(rdf_path)?;
        normalizer.validate()?;

        // Build normalized data structure
        let entities = normalizer.resolve_dependencies()?;

        Ok(NormalizedData {
            store: normalizer.store().clone(),
            entities,
        })
    }

    /// μ₂ stage: Extract entity data
    fn extract_stage(
        &self, extractor: &mut Extractor, normalized: &NormalizedData,
    ) -> Result<ExtractedData> {
        let entities = extractor.extract_entities(&normalized.store)?;

        // For now, just return empty ExtractedData
        // Full implementation would build ElixirModule from entities
        let _ = entities; // Suppress unused warning for now

        Ok(ExtractedData::new())
    }

    /// μ₃ stage: Emit Elixir code
    fn emit_stage(
        &self, _emitter: &Emitter, _normalized: &NormalizedData, _extracted: &ExtractedData,
    ) -> Result<Vec<String>> {
        // This is a placeholder - actual implementation would use the emit module
        // For now, just return empty vector
        Ok(vec![])
    }

    /// μ₄ stage: Canonicalize output
    fn canonicalize_stage(
        &self, canonicalizer: &Canonicalizer, files: &[String],
    ) -> Result<Vec<String>> {
        canonicalizer.canonicalize(files)
    }

    /// μ₅ stage: Generate receipt
    fn receipt_stage(
        &self, generator: &ReceiptGenerator, input_path: &Path, output_files: &[String],
        entity_count: usize, duration_ms: u64,
    ) -> Result<GenerationReceipt> {
        generator.generate(
            input_path.to_str().unwrap(),
            output_files,
            entity_count,
            duration_ms,
        )
    }

    /// Set whether to generate receipt files
    pub fn with_receipts(mut self, generate: bool) -> Self {
        self.generate_receipts = generate;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_generator_creation() {
        let temp_dir = tempfile::tempdir().unwrap();
        let generator = CodeGenerator::new(temp_dir.path());

        assert!(generator.is_ok());
    }

    #[test]
    fn test_with_receipts() {
        let temp_dir = tempfile::tempdir().unwrap();
        let generator = CodeGenerator::new(temp_dir.path()).unwrap();

        assert!(generator.generate_receipts);

        let generator = generator.with_receipts(false);
        assert!(!generator.generate_receipts);
    }
}
