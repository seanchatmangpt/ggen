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

pub mod assertions;
pub mod constraint;
pub mod evaluation;
pub mod field;
pub mod model_capabilities;
pub mod module;
pub mod optimizer;
pub mod optimizers;
pub mod predictor;
pub mod prompt_ir;
pub mod signature;
pub mod signature_validator;
pub mod testing;
pub mod validation_error;

pub use assertions::{
    AssertableModule, AssertedModule, Assertion, AssertionError, AssertionLevel, AssertionResult,
    BacktrackExecutor, ValidationResult, Validator,
};
pub use constraint::{
    decode_and_validate, suggest_repair, Constraint, ConstraintSet, ConstraintViolation, JsonType,
    RepairStrategy,
};
pub use field::{FieldConstraints, FieldMetadata, InputField, OutputField};
pub use model_capabilities::{
    default_registry, LatencyClass, Modality, Model, ModelCapabilities, ModelConfig, ModelCost,
    ModelProvider, ModelRegistry, ReliabilityClass,
};
pub use module::{Module, ModuleError};
pub use optimizer::{BootstrapFewShot, Demonstration, Example, MetricFn, OptimizedPredictor};
pub use optimizers::{
    BootstrapFewShotWithRandomSearch, CosineVectorizer,
    ExactMatchMetric as OptimizerExactMatchMetric, FuzzyMatchMetric, KNNFewShot, LabeledFewShot,
    Metric, OptimizationStatistics, Optimizer, Trace as OptimizerTrace, TraceStep, Vectorizer,
    COPRO,
};
pub use predictor::{ChainOfThought, Predictor};
pub use prompt_ir::{
    JsonRenderer, OutputFormat, PromptAtom, PromptIR, PromptMetadata, PromptRenderer, RenderConfig,
    TextRenderer,
};
pub use signature::Signature;
pub use signature_validator::SignatureValidator;
pub use validation_error::{ValidationError, ValidationErrorDetail, ValidationErrorType};

// Evaluation framework exports
pub use evaluation::{
    composite, display_table, exact_match, exact_match_ci, export_to_csv, export_to_json, f1_score,
    length_within_range, passage_match, substring_match, token_overlap, EnhancedMetricFn, Evaluate,
    EvaluationPoint, EvaluationResult, ExactMatchMetric as EvalExactMatchMetric, F1Metric,
    MetricError, MetricResult, ModuleCall, PassageMatchMetric, SimpleMetricFn,
    Trace as EvaluationTrace,
};
