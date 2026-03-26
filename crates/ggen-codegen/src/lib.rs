//! Generic code generation framework
//!
//! Type-safe, zero-cost abstractions for composable code generators.
//!
//! # Core Abstractions
//!
//! - `Queryable` trait: Abstracts query execution (e.g., SPARQL SELECT)
//! - `Renderable` trait: Abstracts template rendering (e.g., Tera)
//! - `Rule<Q, T>`: Composable rule combining query + template
//! - `GeneratedFile`: Represents a generated file with content hash
//! - `GenerationMode`: Specifies how to handle output files
//!
//! # Design Principles
//!
//! - **Type-first**: Traits encode contracts at compile time
//! - **Zero-cost**: Generics only, no trait objects or runtime dispatch
//! - **Composable**: Rules combine orthogonal concerns
//! - **Deterministic**: Content hashing verifies identical output

pub mod generated_file;
pub mod generation_mode;
pub mod queryable;
pub mod renderable;
pub mod rule;

pub use generated_file::GeneratedFile;
pub use generation_mode::GenerationMode;
pub use queryable::Queryable;
pub use renderable::Renderable;
pub use rule::Rule;

/// Error type for code generation
pub type Result<T> = std::result::Result<T, Error>;

/// Code generation errors
#[derive(Debug)]
pub struct Error {
    message: String,
}

impl Error {
    /// Create a new error
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    /// Create a template error
    pub fn template(message: impl Into<String>) -> Self {
        Self {
            message: format!("Template error: {}", message.into()),
        }
    }

    /// Create a query error
    pub fn query(message: impl Into<String>) -> Self {
        Self {
            message: format!("Query error: {}", message.into()),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for Error {}
