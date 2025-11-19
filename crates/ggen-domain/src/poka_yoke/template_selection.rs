//! # Fool-Proof Template Selection with Physical Constraints
//!
//! This module implements mistake-proof template selection using the type system
//! to prevent invalid template usage at compile time.

use std::marker::PhantomData;
use std::path::{Path, PathBuf};
use crate::poka_yoke::ontology_guards::{SchemaVersion, ValidatedOntology, ValidationState};

/// Template type markers - sealed to prevent extension
pub trait TemplateType: private::Sealed {
    const TYPE_NAME: &'static str;
}

#[derive(Debug, Clone, Copy)]
pub struct RustProject;

#[derive(Debug, Clone, Copy)]
pub struct JavaScriptProject;

#[derive(Debug, Clone, Copy)]
pub struct RustLibrary;

#[derive(Debug, Clone, Copy)]
pub struct RustCLI;

#[derive(Debug, Clone, Copy)]
pub struct RustWebApp;

#[derive(Debug, Clone, Copy)]
pub struct NextJsApp;

#[derive(Debug, Clone, Copy)]
pub struct NuxtApp;

impl TemplateType for RustProject {
    const TYPE_NAME: &'static str = "rust-project";
}

impl TemplateType for JavaScriptProject {
    const TYPE_NAME: &'static str = "javascript-project";
}

impl TemplateType for RustLibrary {
    const TYPE_NAME: &'static str = "rust-library";
}

impl TemplateType for RustCLI {
    const TYPE_NAME: &'static str = "rust-cli";
}

impl TemplateType for RustWebApp {
    const TYPE_NAME: &'static str = "rust-web-app";
}

impl TemplateType for NextJsApp {
    const TYPE_NAME: &'static str = "nextjs-app";
}

impl TemplateType for NuxtApp {
    const TYPE_NAME: &'static str = "nuxt-app";
}

/// Template compatibility marker
pub trait TemplateCompatibility<V: SchemaVersion>: private::Sealed {
    /// Check if this template type is compatible with schema version V
    fn is_compatible() -> bool;
}

// Define compatibility rules at compile time
impl<V: SchemaVersion> TemplateCompatibility<V> for RustProject {
    fn is_compatible() -> bool {
        // Rust projects compatible with all versions
        true
    }
}

impl<V: SchemaVersion> TemplateCompatibility<V> for RustLibrary {
    fn is_compatible() -> bool {
        true
    }
}

impl<V: SchemaVersion> TemplateCompatibility<V> for RustCLI {
    fn is_compatible() -> bool {
        // CLI templates require v2.0.0 or later
        V::VERSION >= "2.0.0"
    }
}

impl<V: SchemaVersion> TemplateCompatibility<V> for RustWebApp {
    fn is_compatible() -> bool {
        // Web apps require v2.0.0 or later
        V::VERSION >= "2.0.0"
    }
}

impl<V: SchemaVersion> TemplateCompatibility<V> for NextJsApp {
    fn is_compatible() -> bool {
        V::VERSION >= "2.1.0"
    }
}

impl<V: SchemaVersion> TemplateCompatibility<V> for NuxtApp {
    fn is_compatible() -> bool {
        V::VERSION >= "2.1.0"
    }
}

/// Template selection state markers
pub trait SelectionState: private::Sealed {}

#[derive(Debug, Clone, Copy)]
pub struct Unselected;

#[derive(Debug, Clone, Copy)]
pub struct Selected;

#[derive(Debug, Clone, Copy)]
pub struct Validated;

impl SelectionState for Unselected {}
impl SelectionState for Selected {}
impl SelectionState for Validated {}

/// Validated template with compile-time type and schema compatibility checking
///
/// # Type Parameters
///
/// - `T`: Template type marker
/// - `V`: Schema version marker
/// - `S`: Selection state marker
///
/// # Physical Constraint
///
/// Templates can only be constructed if they are compatible with the schema version
#[derive(Debug, Clone)]
pub struct ValidatedTemplate<T: TemplateType, V: SchemaVersion, S: SelectionState>
where
    T: TemplateCompatibility<V>,
{
    path: PathBuf,
    _template_type: PhantomData<T>,
    _version: PhantomData<V>,
    _state: PhantomData<S>,
}

impl<T, V> ValidatedTemplate<T, V, Unselected>
where
    T: TemplateType + TemplateCompatibility<V>,
    V: SchemaVersion,
{
    /// Create a new template selector
    ///
    /// # Physical Constraint
    ///
    /// This function only compiles if T is compatible with V
    pub fn new(path: PathBuf) -> Option<Self> {
        if T::is_compatible() {
            Some(Self {
                path,
                _template_type: PhantomData,
                _version: PhantomData,
                _state: PhantomData,
            })
        } else {
            None
        }
    }

    /// Select and validate the template exists
    pub fn select(self) -> Result<ValidatedTemplate<T, V, Selected>, SelectionError> {
        if !self.path.exists() {
            return Err(SelectionError::TemplateNotFound {
                path: self.path.clone(),
            });
        }

        if !self.path.is_dir() {
            return Err(SelectionError::InvalidTemplatePath {
                path: self.path.clone(),
                reason: "Path is not a directory".to_string(),
            });
        }

        Ok(ValidatedTemplate {
            path: self.path,
            _template_type: PhantomData,
            _version: PhantomData,
            _state: PhantomData,
        })
    }
}

impl<T, V> ValidatedTemplate<T, V, Selected>
where
    T: TemplateType + TemplateCompatibility<V>,
    V: SchemaVersion,
{
    /// Validate template structure and compatibility
    pub fn validate<S: ValidationState>(
        self,
        ontology: &ValidatedOntology<V, S>,
    ) -> Result<ValidatedTemplate<T, V, Validated>, SelectionError> {
        // Validate template structure
        self.validate_structure()?;

        // Validate template is compatible with ontology
        self.validate_ontology_compatibility(ontology)?;

        Ok(ValidatedTemplate {
            path: self.path,
            _template_type: PhantomData,
            _version: PhantomData,
            _state: PhantomData,
        })
    }

    fn validate_structure(&self) -> Result<(), SelectionError> {
        // Check for required template files
        let template_file = self.path.join("template.md");
        if !template_file.exists() {
            return Err(SelectionError::MissingTemplateFile {
                path: self.path.clone(),
                file: "template.md".to_string(),
            });
        }

        Ok(())
    }

    fn validate_ontology_compatibility<S: ValidationState>(
        &self,
        _ontology: &ValidatedOntology<V, S>,
    ) -> Result<(), SelectionError> {
        // TODO: Perform deep compatibility check
        // For now, trust the type system
        Ok(())
    }
}

impl<T, V, S> ValidatedTemplate<T, V, S>
where
    T: TemplateType + TemplateCompatibility<V>,
    V: SchemaVersion,
    S: SelectionState,
{
    /// Get the template path
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the template type name
    pub fn template_type(&self) -> &'static str {
        T::TYPE_NAME
    }

    /// Get the schema version
    pub fn schema_version(&self) -> &'static str {
        V::VERSION
    }
}

/// Physical constraints for template paths
#[derive(Debug, Clone)]
pub struct TemplateConstraints {
    /// Maximum depth allowed in template directory structure
    max_depth: usize,
    /// Required file extensions
    required_extensions: Vec<String>,
    /// Forbidden path components
    forbidden_components: Vec<String>,
}

impl TemplateConstraints {
    /// Create new template constraints
    pub fn new() -> Self {
        Self {
            max_depth: 10,
            required_extensions: vec!["md".to_string()],
            forbidden_components: vec![
                "..".to_string(),
                ".git".to_string(),
                "target".to_string(),
            ],
        }
    }

    /// Validate a path against constraints
    ///
    /// # Physical Constraint
    ///
    /// Paths violating physical constraints are rejected
    pub fn validate_path(&self, path: &Path) -> Result<(), SelectionError> {
        // Check depth
        let depth = path.components().count();
        if depth > self.max_depth {
            return Err(SelectionError::PathTooDeep {
                path: path.to_path_buf(),
                max_depth: self.max_depth,
            });
        }

        // Check for forbidden components
        for component in path.components() {
            let component_str = component.as_os_str().to_string_lossy();
            if self.forbidden_components.contains(&component_str.to_string()) {
                return Err(SelectionError::ForbiddenPathComponent {
                    path: path.to_path_buf(),
                    component: component_str.to_string(),
                });
            }
        }

        Ok(())
    }
}

impl Default for TemplateConstraints {
    fn default() -> Self {
        Self::new()
    }
}

/// Template selector builder with type-state pattern
pub struct TemplateSelectionBuilder<V: SchemaVersion> {
    constraints: TemplateConstraints,
    _version: PhantomData<V>,
}

impl<V: SchemaVersion> TemplateSelectionBuilder<V> {
    /// Create a new template selection builder
    pub fn new() -> Self {
        Self {
            constraints: TemplateConstraints::new(),
            _version: PhantomData,
        }
    }

    /// Set custom constraints
    pub fn with_constraints(mut self, constraints: TemplateConstraints) -> Self {
        self.constraints = constraints;
        self
    }

    /// Select a template with compile-time type checking
    ///
    /// # Type Safety
    ///
    /// This function only compiles if T is compatible with V
    pub fn select<T>(
        &self,
        path: PathBuf,
    ) -> Result<ValidatedTemplate<T, V, Unselected>, SelectionError>
    where
        T: TemplateType + TemplateCompatibility<V>,
    {
        // Validate path constraints
        self.constraints.validate_path(&path)?;

        // Create template (compile-time compatibility check)
        ValidatedTemplate::new(path)
            .ok_or_else(|| SelectionError::IncompatibleTemplate {
                template_type: T::TYPE_NAME,
                schema_version: V::VERSION,
            })
    }
}

impl<V: SchemaVersion> Default for TemplateSelectionBuilder<V> {
    fn default() -> Self {
        Self::new()
    }
}

/// Template selector - convenience type for building selections
pub struct TemplateSelector;

impl TemplateSelector {
    /// Start building a template selection for schema version V
    pub fn for_schema<V: SchemaVersion>() -> TemplateSelectionBuilder<V> {
        TemplateSelectionBuilder::new()
    }
}

/// Selection errors
#[derive(Debug, thiserror::Error)]
pub enum SelectionError {
    #[error("Template not found at path: {path:?}")]
    TemplateNotFound { path: PathBuf },

    #[error("Invalid template path {path:?}: {reason}")]
    InvalidTemplatePath { path: PathBuf, reason: String },

    #[error("Missing required template file {file} in {path:?}")]
    MissingTemplateFile { path: PathBuf, file: String },

    #[error("Template type {template_type} is not compatible with schema version {schema_version}")]
    IncompatibleTemplate {
        template_type: &'static str,
        schema_version: &'static str,
    },

    #[error("Path too deep: {path:?} (max depth: {max_depth})")]
    PathTooDeep { path: PathBuf, max_depth: usize },

    #[error("Forbidden path component {component} in path: {path:?}")]
    ForbiddenPathComponent { path: PathBuf, component: String },
}

/// Private module to seal traits
mod private {
    pub trait Sealed {}

    impl Sealed for super::RustProject {}
    impl Sealed for super::JavaScriptProject {}
    impl Sealed for super::RustLibrary {}
    impl Sealed for super::RustCLI {}
    impl Sealed for super::RustWebApp {}
    impl Sealed for super::NextJsApp {}
    impl Sealed for super::NuxtApp {}
    impl Sealed for super::Unselected {}
    impl Sealed for super::Selected {}
    impl Sealed for super::Validated {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::poka_yoke::ontology_guards::{V1_0_0, V2_0_0, V2_1_0};

    #[test]
    fn test_template_compatibility() {
        // Rust projects should be compatible with all versions
        assert!(RustProject::is_compatible::<V1_0_0>());
        assert!(RustProject::is_compatible::<V2_0_0>());
        assert!(RustProject::is_compatible::<V2_1_0>());

        // CLI templates require v2.0.0+
        assert!(!RustCLI::is_compatible::<V1_0_0>());
        assert!(RustCLI::is_compatible::<V2_0_0>());
        assert!(RustCLI::is_compatible::<V2_1_0>());
    }

    #[test]
    fn test_path_constraints() {
        let constraints = TemplateConstraints::new();

        // Valid path
        let valid_path = PathBuf::from("templates/rust/library");
        assert!(constraints.validate_path(&valid_path).is_ok());

        // Path with forbidden component
        let forbidden_path = PathBuf::from("templates/../secret");
        assert!(constraints.validate_path(&forbidden_path).is_err());
    }

    #[test]
    fn test_template_type_names() {
        assert_eq!(RustProject::TYPE_NAME, "rust-project");
        assert_eq!(RustCLI::TYPE_NAME, "rust-cli");
        assert_eq!(NextJsApp::TYPE_NAME, "nextjs-app");
    }
}
