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
    pub generate_receipts: bool,
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
    pub fn generate_from_rdf<P: AsRef<Path> + std::fmt::Debug>(
        &self, rdf_path: P,
    ) -> Result<GenerationReceipt> {
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
            let receipt_path_str = receipt_path.to_str()
                .ok_or_else(|| CraftplanError::FileNotFound {
                    path: receipt_path.clone(),
                })?;
            receipt_generator.write_receipt(&receipt, receipt_path_str)?;
        }

        info!(
            "Generation complete: {} files in {}ms",
            generated_files.len(),
            duration_ms
        );

        Ok(receipt)
    }

    /// μ₁ stage: Normalize RDF
    fn normalize_stage(
        &self, normalizer: &mut Normalizer, rdf_path: &Path,
    ) -> Result<NormalizedData> {
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

        // Build ExtractedData from extracted entities (80/20 version)
        let mut extracted_data = ExtractedData::new();

        for entity in entities {
            // Extract attributes for this entity
            let attributes = extractor.extract_attributes(&normalized.store, &entity.name)?;

            // Convert to ElixirModule (basic 80/20 version)
            let elixir_module = crate::models::ElixirModule {
                name: format!("Craftplan.{}", entity.name),
                module_type: crate::models::ModuleType::AshResource,
                entity: crate::models::EntityMetadata {
                    iri: format!("http://craftplan.org/ontology/{}", entity.name),
                    local_name: entity.name.clone(),
                    description: None,
                    class_type: "owl:Class".to_string(),
                    namespace: "Craftplan".to_string(),
                },
                fields: attributes
                    .into_iter()
                    .map(|attr| crate::models::Field {
                        name: attr.name.clone(),
                        field_type: map_rdf_type_to_elixir(&attr.type_),
                        required: attr.required,
                        default: None,
                        label: None,
                        help_text: attr.doc,
                        primary_key: attr.name == "id",
                        unique: false,
                    })
                    .collect(),
                relationships: entity
                    .relationships
                    .into_iter()
                    .map(|rel| crate::models::Relationship {
                        name: rel.name,
                        target_iri: format!("http://craftplan.org/ontology/{}", rel.target_entity),
                        cardinality: map_cardinality(&rel.cardinality),
                        foreign_key: None,
                        join_relationship: rel.inverse_of,
                        required: false,
                    })
                    .collect(),
                actions: vec![],
                validations: vec![],
            };

            extracted_data.add_entity(elixir_module);
        }

        Ok(extracted_data)
    }

    /// μ₃ stage: Emit Elixir code
    fn emit_stage(
        &self, emitter: &Emitter, _normalized: &NormalizedData, extracted: &ExtractedData,
    ) -> Result<Vec<String>> {
        let mut generated_files = Vec::new();

        // Generate Ash Resource files for each entity (80/20 version)
        for entity in &extracted.entities {
            let config = crate::emit::GenerationConfig::new(
                "craftplan".to_string(),
                "Craftplan".to_string(),
                "0.1.0".to_string(),
            );

            // Convert ElixirModule to Entity for template rendering
            let template_entity = crate::extract::Entity {
                name: entity.entity.local_name.clone(),
                plural: None,
                attributes: entity
                    .fields
                    .iter()
                    .map(|f| crate::extract::Attribute {
                        name: f.name.clone(),
                        type_: format!("{:?}", f.field_type),
                        required: f.required,
                        doc: f.help_text.clone(),
                    })
                    .collect(),
                relationships: entity
                    .relationships
                    .iter()
                    .map(|r| crate::extract::Relationship {
                        name: r.name.clone(),
                        cardinality: format!("{:?}", r.cardinality),
                        target_entity: entity.entity.local_name.clone(),
                        inverse_of: r.join_relationship.clone(),
                    })
                    .collect(),
            };

            let rendered = emitter.render_ash_resource(&template_entity, &config)?;

            // Write to file
            let file_name = format!("{}.ex", entity.entity.local_name.to_lowercase());
            let file_path = self.output_dir.join(&file_name);
            std::fs::write(&file_path, rendered).map_err(|e| CraftplanError::Io {
                path: file_path.clone(),
                source: e,
            })?;

            generated_files.push(file_path.to_string_lossy().to_string());
        }

        Ok(generated_files)
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
        let input_path_str = input_path.to_str()
            .ok_or_else(|| CraftplanError::FileNotFound {
                path: input_path.to_path_buf(),
            })?;
        generator.generate(
            input_path_str,
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

/// Map RDF type to Elixir type (80/20 version)
fn map_rdf_type_to_elixir(rdf_type: &str) -> crate::models::ElixirType {
    match rdf_type {
        "http://www.w3.org/2001/XMLSchema#string" => crate::models::ElixirType::String,
        "http://www.w3.org/2001/XMLSchema#integer" => crate::models::ElixirType::Integer,
        "http://www.w3.org/2001/XMLSchema#decimal" | "http://www.w3.org/2001/XMLSchema#float" => {
            crate::models::ElixirType::Float
        }
        "http://www.w3.org/2001/XMLSchema#boolean" => crate::models::ElixirType::Boolean,
        "http://www.w3.org/2001/XMLSchema#dateTime" => crate::models::ElixirType::DateTime,
        "http://www.w3.org/2001/XMLSchema#date" => crate::models::ElixirType::Date,
        _ => crate::models::ElixirType::Custom(rdf_type.to_string()),
    }
}

/// Map cardinality string to Cardinality enum
fn map_cardinality(cardinality: &str) -> crate::models::Cardinality {
    match cardinality {
        "one-to-one" => crate::models::Cardinality::OneToOne,
        "one-to-many" => crate::models::Cardinality::OneToMany,
        "many-to-many" => crate::models::Cardinality::ManyToMany,
        "belongs-to" => crate::models::Cardinality::BelongsTo,
        _ => crate::models::Cardinality::OneToMany,
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
