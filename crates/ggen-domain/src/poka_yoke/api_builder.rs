//! # Error-Impossible API Design Using Type-State Pattern
//!
//! This module implements the type-state pattern to create APIs that are
//! impossible to use incorrectly. Missing required configuration is a
//! compile-time error, not a runtime error.

use std::marker::PhantomData;
use std::path::PathBuf;
use crate::poka_yoke::ontology_guards::{SchemaVersion, ValidatedOntology, SchemaChecked};
use crate::poka_yoke::template_selection::{
    ValidatedTemplate, TemplateType, TemplateCompatibility, Validated as TemplateValidated,
};

/// Builder state markers
pub trait BuilderState: private::Sealed {}

/// Marker for incomplete builder
#[derive(Debug, Clone, Copy)]
pub struct Incomplete;

/// Marker for complete builder (all required fields set)
#[derive(Debug, Clone, Copy)]
pub struct Complete;

/// Marker for builder with template set
#[derive(Debug, Clone, Copy)]
pub struct WithTemplate;

/// Marker for builder with ontology set
#[derive(Debug, Clone, Copy)]
pub struct WithOntology;

/// Marker for builder with output path set
#[derive(Debug, Clone, Copy)]
pub struct WithOutput;

impl BuilderState for Incomplete {}
impl BuilderState for Complete {}
impl BuilderState for WithTemplate {}
impl BuilderState for WithOntology {}
impl BuilderState for WithOutput {}

/// Generator builder with type-state pattern
///
/// # Type Safety
///
/// The builder uses phantom types to track which required fields have been set.
/// The `generate()` method is only available when all required fields are set,
/// making it impossible to forget required configuration.
///
/// # Example
///
/// ```ignore
/// use ggen_domain::poka_yoke::api_builder::GeneratorBuilder;
///
/// // This compiles - all required fields set
/// let generator = GeneratorBuilder::new()
///     .with_template(template)
///     .with_ontology(ontology)
///     .with_output(output_path)
///     .generate()?;
///
/// // This does NOT compile - missing required fields
/// let generator = GeneratorBuilder::new()
///     .with_template(template)
///     .generate()?;  // Compile error: method not available
/// ```
#[derive(Debug)]
pub struct GeneratorBuilder<V, T, S>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
    S: BuilderState,
{
    template: Option<ValidatedTemplate<T, V, TemplateValidated>>,
    ontology: Option<ValidatedOntology<V, SchemaChecked>>,
    output_path: Option<PathBuf>,
    variables: std::collections::HashMap<String, String>,
    dry_run: bool,
    _version: PhantomData<V>,
    _template_type: PhantomData<T>,
    _state: PhantomData<S>,
}

/// Initial state - no fields set
impl<V, T> GeneratorBuilder<V, T, Incomplete>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Create a new generator builder
    pub fn new() -> Self {
        Self {
            template: None,
            ontology: None,
            output_path: None,
            variables: std::collections::HashMap::new(),
            dry_run: false,
            _version: PhantomData,
            _template_type: PhantomData,
            _state: PhantomData,
        }
    }
}

impl<V, T> Default for GeneratorBuilder<V, T, Incomplete>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    fn default() -> Self {
        Self::new()
    }
}

/// State transitions - each method moves to a new state
impl<V, T> GeneratorBuilder<V, T, Incomplete>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Set the template (required)
    ///
    /// # Physical Constraint
    ///
    /// Template must be validated and compatible with schema version V
    pub fn with_template(
        mut self,
        template: ValidatedTemplate<T, V, TemplateValidated>,
    ) -> GeneratorBuilder<V, T, WithTemplate> {
        self.template = Some(template);
        GeneratorBuilder {
            template: self.template,
            ontology: self.ontology,
            output_path: self.output_path,
            variables: self.variables,
            dry_run: self.dry_run,
            _version: PhantomData,
            _template_type: PhantomData,
            _state: PhantomData,
        }
    }
}

impl<V, T> GeneratorBuilder<V, T, WithTemplate>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Set the ontology (required)
    ///
    /// # Physical Constraint
    ///
    /// Ontology must be schema-checked for version V
    pub fn with_ontology(
        mut self,
        ontology: ValidatedOntology<V, SchemaChecked>,
    ) -> GeneratorBuilder<V, T, WithOntology> {
        self.ontology = Some(ontology);
        GeneratorBuilder {
            template: self.template,
            ontology: self.ontology,
            output_path: self.output_path,
            variables: self.variables,
            dry_run: self.dry_run,
            _version: PhantomData,
            _template_type: PhantomData,
            _state: PhantomData,
        }
    }
}

impl<V, T> GeneratorBuilder<V, T, WithOntology>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Set the output path (required)
    pub fn with_output(
        mut self,
        output_path: PathBuf,
    ) -> GeneratorBuilder<V, T, WithOutput> {
        self.output_path = Some(output_path);
        GeneratorBuilder {
            template: self.template,
            ontology: self.ontology,
            output_path: self.output_path,
            variables: self.variables,
            dry_run: self.dry_run,
            _version: PhantomData,
            _template_type: PhantomData,
            _state: PhantomData,
        }
    }
}

impl<V, T> GeneratorBuilder<V, T, WithOutput>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Mark as complete (all required fields set)
    pub fn complete(self) -> GeneratorBuilder<V, T, Complete> {
        GeneratorBuilder {
            template: self.template,
            ontology: self.ontology,
            output_path: self.output_path,
            variables: self.variables,
            dry_run: self.dry_run,
            _version: PhantomData,
            _template_type: PhantomData,
            _state: PhantomData,
        }
    }
}

/// Optional configuration available at all states
impl<V, T, S> GeneratorBuilder<V, T, S>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
    S: BuilderState,
{
    /// Add a template variable (optional)
    pub fn with_variable(mut self, key: String, value: String) -> Self {
        self.variables.insert(key, value);
        self
    }

    /// Set dry-run mode (optional)
    pub fn with_dry_run(mut self, dry_run: bool) -> Self {
        self.dry_run = dry_run;
        self
    }
}

/// Generate is only available when complete
impl<V, T> GeneratorBuilder<V, T, Complete>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Generate code from the configured template and ontology
    ///
    /// # Physical Constraint
    ///
    /// This method is only available when all required fields are set.
    /// It is impossible to call generate() on an incomplete builder.
    ///
    /// # Returns
    ///
    /// A fully configured Generator ready for code generation
    pub fn build(self) -> Generator<V, T> {
        Generator {
            template: self.template.expect("Template must be set"),
            ontology: self.ontology.expect("Ontology must be set"),
            output_path: self.output_path.expect("Output path must be set"),
            variables: self.variables,
            dry_run: self.dry_run,
            _version: PhantomData,
            _template_type: PhantomData,
        }
    }
}

/// Fully configured generator
///
/// # Type Safety
///
/// Can only be created through GeneratorBuilder when all required fields are set
pub struct Generator<V, T>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    template: ValidatedTemplate<T, V, TemplateValidated>,
    ontology: ValidatedOntology<V, SchemaChecked>,
    output_path: PathBuf,
    variables: std::collections::HashMap<String, String>,
    dry_run: bool,
    _version: PhantomData<V>,
    _template_type: PhantomData<T>,
}

impl<V, T> Generator<V, T>
where
    V: SchemaVersion,
    T: TemplateType + TemplateCompatibility<V>,
{
    /// Execute code generation
    ///
    /// # Type Safety
    ///
    /// Template is guaranteed to be compatible with ontology schema version
    pub fn generate(&self) -> Result<GenerationResult, GenerationError> {
        // Validate output path
        self.validate_output_path()?;

        // Generate code
        let files = self.generate_files()?;

        Ok(GenerationResult {
            files_generated: files,
            template_type: T::TYPE_NAME,
            schema_version: V::VERSION,
            dry_run: self.dry_run,
        })
    }

    fn validate_output_path(&self) -> Result<(), GenerationError> {
        if self.output_path.exists() && !self.output_path.is_dir() {
            return Err(GenerationError::InvalidOutputPath {
                path: self.output_path.clone(),
                reason: "Path exists but is not a directory".to_string(),
            });
        }
        Ok(())
    }

    fn generate_files(&self) -> Result<Vec<PathBuf>, GenerationError> {
        // TODO: Implement actual generation
        // For now, return empty vec
        Ok(vec![])
    }

    /// Get template reference
    pub fn template(&self) -> &ValidatedTemplate<T, V, TemplateValidated> {
        &self.template
    }

    /// Get ontology reference
    pub fn ontology(&self) -> &ValidatedOntology<V, SchemaChecked> {
        &self.ontology
    }

    /// Get output path
    pub fn output_path(&self) -> &PathBuf {
        &self.output_path
    }

    /// Check if in dry-run mode
    pub fn is_dry_run(&self) -> bool {
        self.dry_run
    }
}

/// Result of code generation
#[derive(Debug)]
pub struct GenerationResult {
    pub files_generated: Vec<PathBuf>,
    pub template_type: &'static str,
    pub schema_version: &'static str,
    pub dry_run: bool,
}

impl GenerationResult {
    /// Get count of generated files
    pub fn file_count(&self) -> usize {
        self.files_generated.len()
    }

    /// Check if any files were generated
    pub fn has_files(&self) -> bool {
        !self.files_generated.is_empty()
    }
}

/// Generation errors
#[derive(Debug, thiserror::Error)]
pub enum GenerationError {
    #[error("Invalid output path {path:?}: {reason}")]
    InvalidOutputPath { path: PathBuf, reason: String },

    #[error("Template rendering failed: {0}")]
    TemplateRenderError(String),

    #[error("File write failed: {0}")]
    FileWriteError(#[from] std::io::Error),

    #[error("Graph query failed: {0}")]
    GraphQueryError(String),
}

/// Private module to seal traits
mod private {
    pub trait Sealed {}

    impl Sealed for super::Incomplete {}
    impl Sealed for super::Complete {}
    impl Sealed for super::WithTemplate {}
    impl Sealed for super::WithOntology {}
    impl Sealed for super::WithOutput {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::poka_yoke::ontology_guards::{V2_0_0, Unvalidated};
    use crate::poka_yoke::template_selection::RustLibrary;

    // This test demonstrates compile-time safety
    // Uncommenting the following would cause a compile error:
    /*
    #[test]
    fn test_incomplete_builder_cannot_generate() {
        let builder = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new();
        // This line would not compile:
        // let generator = builder.build();
    }
    */

    #[test]
    fn test_builder_state_transitions() {
        // Verify state types are correct
        let builder = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new();
        assert!(!builder.dry_run);
    }

    #[test]
    fn test_generation_result() {
        let result = GenerationResult {
            files_generated: vec![PathBuf::from("test.rs")],
            template_type: "rust-library",
            schema_version: "2.0.0",
            dry_run: false,
        };

        assert_eq!(result.file_count(), 1);
        assert!(result.has_files());
        assert_eq!(result.template_type, "rust-library");
        assert_eq!(result.schema_version, "2.0.0");
    }
}
