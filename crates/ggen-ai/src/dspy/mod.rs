//! Rust Equivalents of DSPy Primitives - Now with Constraint Calculus
//!
//! This module provides native Rust implementations of DSPy's core abstractions,
//! but goes further by formalizing the latent constraint system that DSPy accidentally gestures at.
//!
//! ## Core Abstractions
//!
//! - `Signature` - Type-safe specification of task interface
//! - `InputField` / `OutputField` - Named fields with metadata and constraints
//! - `Module` - Composable unit of computation
//! - `Predictor` - LLM-backed prediction with constraint-aware decoding
//!
//! ## Constraint Calculus (New)
//!
//! The key insight: LLM-centric systems are **constraint systems with stochastic solvers**.
//! "Prompting" is a compilation phase. This module formalizes that:
//!
//! - `Constraint` - Atomic constraint primitives (Required, MinLength, Pattern, OneOf, etc.)
//! - `ConstraintSet` - Composable constraint collections with conjunction/disjunction
//! - `decode_and_validate` - Constraint-aware output decoding (the missing piece)
//!
//! ## Prompt-as-IR
//!
//! Prompts are now structured data, not strings:
//!
//! - `PromptAtom` - Semantic units (inputs, outputs, constraints, examples)
//! - `PromptIR` - Intermediate representation before rendering
//! - `PromptRenderer` - Rendering backend trait (TextRenderer, JsonRenderer)
//!
//! ## Model Capabilities
//!
//! Models are more than names - they have capabilities:
//!
//! - `Model` - Typed model with provider and capabilities
//! - `ModelCapabilities` - Context window, structured output, function calling, etc.
//! - `ModelRegistry` - Pre-registered models with known capabilities

pub mod constraint;
pub mod field;
pub mod model_capabilities;
pub mod module;
pub mod predictor;
pub mod prompt_ir;
pub mod signature;
pub mod signature_validator;
pub mod validation_error;

// Core field types
pub use field::{FieldConstraints, FieldMetadata, InputField, OutputField, SHACLConstraint};

// Signature and validation
pub use signature::Signature;
pub use signature_validator::SignatureValidator;
pub use validation_error::{ValidationError, ValidationErrorDetail, ValidationErrorType};

// Module system
pub use module::{Module, ModuleError};
pub use predictor::{ChainOfThought, Predictor};

// Constraint calculus
pub use constraint::{
    decode_and_validate, Constraint, ConstraintSet, ConstraintViolation, JsonType, RepairStrategy,
};

// Prompt-IR
pub use prompt_ir::{OutputFormat, PromptAtom, PromptIR, PromptRenderer, RenderConfig, TextRenderer};

// Model capabilities
pub use model_capabilities::{
    LatencyClass, Model, ModelCapabilities, ModelConfig, ModelCost, ModelProvider,
    ModelRegistry, Modality, ReliabilityClass,
};
