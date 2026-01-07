//! Rust Equivalents of DSPy Primitives
//!
//! Provides native Rust implementations of DSPy's core abstractions:
//! - `Signature` - Type-safe specification of task interface
//! - `InputField` - Named input to a module with metadata
//! - `OutputField` - Named output from a module with metadata
//! - `Module` - Composable unit of computation

pub mod field;
pub mod signature;
pub mod module;
pub mod predictor;

pub use field::{FieldMetadata, InputField, OutputField};
pub use signature::Signature;
pub use module::{Module, ModuleError};
pub use predictor::{Predictor, ChainOfThought};
