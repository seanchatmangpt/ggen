//! Rust Equivalents of DSPy Primitives
//!
//! Provides native Rust implementations of DSPy's core abstractions:
//! - `Signature` - Type-safe specification of task interface
//! - `InputField` - Named input to a module with metadata
//! - `OutputField` - Named output from a module with metadata
//! - `Module` - Composable unit of computation
//! - `SignatureValidator` - Runtime constraint validation for JSON input
//! - `BootstrapFewShot` - Optimizer for few-shot learning
//! - `optimizers` - Advanced optimizer implementations (KNN, COPRO, etc.)
//! - `assertions` - Assertion and validation system with backtracking
//! - `testing` - Comprehensive testing infrastructure
//! - `evaluation` - Evaluation framework with parallel processing
//! - `constraint` - Formal constraint calculus for LLM I/O
//! - `model_capabilities` - Type-safe model representation with capabilities
//! - `prompt_ir` - Prompt intermediate representation for rendering

pub mod field;
pub mod signature;
pub mod module;
pub mod predictor;
pub mod validation_error;
pub mod signature_validator;
pub mod optimizer;
pub mod optimizers;
pub mod assertions;
pub mod testing;
pub mod evaluation;
pub mod constraint;
pub mod model_capabilities;
pub mod prompt_ir;

pub use field::{FieldConstraints, FieldMetadata, InputField, OutputField};
pub use signature::Signature;
pub use module::{Module, ModuleError};
pub use predictor::{Predictor, ChainOfThought};
pub use validation_error::{ValidationError, ValidationErrorDetail, ValidationErrorType};
pub use signature_validator::SignatureValidator;
pub use optimizer::{BootstrapFewShot, Example, Demonstration, OptimizedPredictor, MetricFn};
pub use optimizers::{
    Optimizer, Metric, Trace as OptimizerTrace, TraceStep, OptimizationStatistics,
    ExactMatchMetric as OptimizerExactMatchMetric, FuzzyMatchMetric,
    LabeledFewShot, KNNFewShot, BootstrapFewShotWithRandomSearch, COPRO,
    Vectorizer, CosineVectorizer,
};
pub use assertions::{
    Assertion, BacktrackExecutor, AssertionError, AssertionResult,
    Validator, ValidationResult, AssertionLevel,
    AssertableModule, AssertedModule,
};
pub use constraint::{
    Constraint, ConstraintSet, ConstraintViolation, JsonType,
    decode_and_validate, suggest_repair, RepairStrategy,
};
pub use model_capabilities::{
    Model, ModelProvider, ModelCapabilities, ModelCost, ModelConfig,
    ModelRegistry, LatencyClass, ReliabilityClass, Modality,
    default_registry,
};
pub use prompt_ir::{
    PromptAtom, PromptIR, PromptMetadata, OutputFormat,
    RenderConfig, PromptRenderer, TextRenderer, JsonRenderer,
};

// Evaluation framework exports
pub use evaluation::{
    Evaluate, EvaluationPoint, EvaluationResult,
    Trace as EvaluationTrace, ModuleCall, MetricResult,
    SimpleMetricFn, EnhancedMetricFn, MetricError,
    exact_match, exact_match_ci, passage_match, substring_match, token_overlap,
    f1_score, length_within_range, composite,
    ExactMatchMetric as EvalExactMatchMetric, F1Metric, PassageMatchMetric,
    display_table, export_to_csv, export_to_json,
};
