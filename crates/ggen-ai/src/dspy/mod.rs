//! Rust Equivalents of DSPy Primitives
//!
//! Provides native Rust implementations of DSPy's core abstractions:
//! - `Signature` - Type-safe specification of task interface
//! - `InputField` - Named input to a module with metadata
//! - `OutputField` - Named output from a module with metadata
//! - `Module` - Composable unit of computation
//! - `SignatureValidator` - Runtime constraint validation for JSON input
//! - `BootstrapFewShot` - Optimizer for few-shot learning

pub mod field;
pub mod signature;
pub mod module;
pub mod predictor;
pub mod validation_error;
pub mod signature_validator;
pub mod optimizer;

pub use field::{FieldConstraints, FieldMetadata, InputField, OutputField};
pub use signature::Signature;
pub use module::{Module, ModuleError};
pub use predictor::{Predictor, ChainOfThought};
pub use validation_error::{ValidationError, ValidationErrorDetail, ValidationErrorType};
pub use signature_validator::SignatureValidator;
pub use optimizer::{BootstrapFewShot, Example, Demonstration, OptimizedPredictor, MetricFn};
