//! Types for DSPy Evaluation Framework
//!
//! Core types for evaluation including traces, metric results, and enhanced metric functions.

use crate::dspy::{ModuleError, Example};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

/// Trace information collected during module execution
///
/// Captures intermediate steps and reasoning paths for trace-aware metrics.
/// Used during optimization to validate not just outputs but also reasoning.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trace {
    /// Module calls with inputs/outputs
    pub module_calls: Vec<ModuleCall>,

    /// Intermediate reasoning steps
    pub reasoning_steps: Vec<String>,

    /// Execution path through modules
    pub path: Vec<String>,

    /// Additional metadata
    #[serde(default)]
    pub metadata: HashMap<String, Value>,
}

impl Trace {
    /// Create a new empty trace
    pub fn new() -> Self {
        Self {
            module_calls: Vec::new(),
            reasoning_steps: Vec::new(),
            path: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add a module call to the trace
    pub fn add_module_call(&mut self, module_call: ModuleCall) {
        self.module_calls.push(module_call);
    }

    /// Add a reasoning step
    pub fn add_reasoning_step(&mut self, step: String) {
        self.reasoning_steps.push(step);
    }

    /// Add to execution path
    pub fn add_to_path(&mut self, module_name: String) {
        self.path.push(module_name);
    }

    /// Set metadata value
    pub fn set_metadata(&mut self, key: String, value: Value) {
        self.metadata.insert(key, value);
    }
}

impl Default for Trace {
    fn default() -> Self {
        Self::new()
    }
}

/// Single module call captured in trace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleCall {
    /// Name of the module
    pub module_name: String,

    /// Input values
    pub inputs: HashMap<String, Value>,

    /// Output values
    pub outputs: HashMap<String, Value>,

    /// Execution duration
    #[serde(default)]
    pub duration: Option<Duration>,
}

impl ModuleCall {
    /// Create a new module call
    pub fn new(
        module_name: impl Into<String>,
        inputs: HashMap<String, Value>,
        outputs: HashMap<String, Value>,
    ) -> Self {
        Self {
            module_name: module_name.into(),
            inputs,
            outputs,
            duration: None,
        }
    }

    /// Set execution duration
    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.duration = Some(duration);
        self
    }
}

/// Metric result - either boolean (bootstrapping) or float (evaluation)
///
/// Dual-mode metrics can return different types depending on context:
/// - Boolean: Used during bootstrapping to accept/reject demonstrations
/// - Score: Used during evaluation/optimization for numeric scoring
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MetricResult {
    /// Boolean result for bootstrapping (accept/reject)
    Boolean(bool),

    /// Numeric score for evaluation (0.0-1.0 typically)
    Score(f64),
}

impl MetricResult {
    /// Convert to boolean (scores > 0.5 = true)
    pub fn as_bool(&self) -> bool {
        match self {
            MetricResult::Boolean(b) => *b,
            MetricResult::Score(s) => *s > 0.5,
        }
    }

    /// Convert to score (bool: 1.0 if true, 0.0 if false)
    pub fn as_score(&self) -> f64 {
        match self {
            MetricResult::Boolean(b) => if *b { 1.0 } else { 0.0 },
            MetricResult::Score(s) => *s,
        }
    }

    /// Create from boolean
    pub fn from_bool(b: bool) -> Self {
        MetricResult::Boolean(b)
    }

    /// Create from score
    pub fn from_score(s: f64) -> Self {
        MetricResult::Score(s)
    }
}

impl From<bool> for MetricResult {
    fn from(b: bool) -> Self {
        MetricResult::Boolean(b)
    }
}

impl From<f64> for MetricResult {
    fn from(s: f64) -> Self {
        MetricResult::Score(s)
    }
}

/// Enhanced metric function signature with trace support
///
/// Supports dual-mode operation:
/// - With trace: Can validate intermediate steps during optimization
/// - Without trace: Standard evaluation of final outputs
pub type EnhancedMetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>, Option<&Trace>) -> Result<MetricResult, MetricError>
        + Send
        + Sync
>;

/// Simple metric function (no trace support)
///
/// For metrics that only need to evaluate final outputs.
pub type SimpleMetricFn = Arc<
    dyn Fn(&Example, &HashMap<String, Value>) -> Result<f64, MetricError> + Send + Sync
>;

/// Metric evaluation errors
#[derive(Debug, thiserror::Error)]
pub enum MetricError {
    #[error("Missing field: {0}")]
    MissingField(String),

    #[error("Invalid field type for '{field}': expected {expected}, got {actual}")]
    InvalidFieldType {
        field: String,
        expected: String,
        actual: String,
    },

    #[error("Metric computation failed: {0}")]
    ComputationError(String),

    #[error("Module error: {0}")]
    ModuleError(#[from] ModuleError),

    #[error("Other error: {0}")]
    Other(String),
}

/// Evaluation point - result for a single example
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationPoint {
    /// The example that was evaluated
    #[serde(skip)]
    pub example: Example,

    /// Prediction outputs from the module
    pub prediction: HashMap<String, Value>,

    /// Score from metric (0.0-1.0)
    pub score: f64,

    /// Error message if evaluation failed
    pub error: Option<String>,

    /// Execution duration
    #[serde(default)]
    pub duration: Option<Duration>,
}

impl EvaluationPoint {
    /// Create a new evaluation point
    pub fn new(
        example: Example,
        prediction: HashMap<String, Value>,
        score: f64,
    ) -> Self {
        Self {
            example,
            prediction,
            score,
            error: None,
            duration: None,
        }
    }

    /// Create a failed evaluation point
    pub fn failed(example: Example, error: String) -> Self {
        Self {
            example,
            prediction: HashMap::new(),
            score: 0.0,
            error: Some(error),
            duration: None,
        }
    }

    /// Set execution duration
    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.duration = Some(duration);
        self
    }

    /// Check if evaluation was successful
    pub fn is_success(&self) -> bool {
        self.error.is_none()
    }
}

/// Aggregated evaluation results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationResult {
    /// Overall score (0.0 - 100.0 percentage)
    pub score: f64,

    /// Individual results for each example
    pub results: Vec<EvaluationPoint>,

    /// Number of successful evaluations
    pub successful: usize,

    /// Number of failed evaluations
    pub failed: usize,

    /// Total evaluation time
    #[serde(skip)]
    pub elapsed: Duration,
}

impl EvaluationResult {
    /// Create new evaluation result
    pub fn new(results: Vec<EvaluationPoint>, elapsed: Duration) -> Self {
        let successful = results.iter().filter(|r| r.is_success()).count();
        let failed = results.len() - successful;

        let total_score: f64 = results.iter().map(|r| r.score).sum();
        let avg_score = if !results.is_empty() {
            (total_score / results.len() as f64) * 100.0
        } else {
            0.0
        };

        Self {
            score: avg_score,
            results,
            successful,
            failed,
            elapsed,
        }
    }

    /// Get average score
    pub fn average_score(&self) -> f64 {
        self.score
    }

    /// Get success rate (0.0-1.0)
    pub fn success_rate(&self) -> f64 {
        if self.results.is_empty() {
            0.0
        } else {
            self.successful as f64 / self.results.len() as f64
        }
    }

    /// Get failed examples
    pub fn failed_examples(&self) -> Vec<&EvaluationPoint> {
        self.results.iter().filter(|r| !r.is_success()).collect()
    }

    /// Get successful examples
    pub fn successful_examples(&self) -> Vec<&EvaluationPoint> {
        self.results.iter().filter(|r| r.is_success()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // ===== Trace Tests =====

    #[test]
    fn test_trace_creation() {
        let trace = Trace::new();
        assert!(trace.module_calls.is_empty());
        assert!(trace.reasoning_steps.is_empty());
        assert!(trace.path.is_empty());
    }

    #[test]
    fn test_trace_add_module_call() {
        let mut trace = Trace::new();
        let call = ModuleCall::new(
            "test_module",
            HashMap::new(),
            HashMap::new(),
        );

        trace.add_module_call(call);
        assert_eq!(trace.module_calls.len(), 1);
        assert_eq!(trace.module_calls[0].module_name, "test_module");
    }

    #[test]
    fn test_trace_add_reasoning_step() {
        let mut trace = Trace::new();
        trace.add_reasoning_step("Step 1: Analyze input".to_string());
        trace.add_reasoning_step("Step 2: Generate response".to_string());

        assert_eq!(trace.reasoning_steps.len(), 2);
        assert_eq!(trace.reasoning_steps[0], "Step 1: Analyze input");
    }

    #[test]
    fn test_trace_path() {
        let mut trace = Trace::new();
        trace.add_to_path("Module1".to_string());
        trace.add_to_path("Module2".to_string());

        assert_eq!(trace.path.len(), 2);
        assert_eq!(trace.path, vec!["Module1", "Module2"]);
    }

    #[test]
    fn test_trace_metadata() {
        let mut trace = Trace::new();
        trace.set_metadata("key1".to_string(), json!("value1"));
        trace.set_metadata("key2".to_string(), json!(42));

        assert_eq!(trace.metadata.len(), 2);
        assert_eq!(trace.metadata.get("key1"), Some(&json!("value1")));
    }

    // ===== ModuleCall Tests =====

    #[test]
    fn test_module_call_creation() {
        let mut inputs = HashMap::new();
        inputs.insert("input".to_string(), json!("test"));

        let mut outputs = HashMap::new();
        outputs.insert("output".to_string(), json!("result"));

        let call = ModuleCall::new("test_module", inputs.clone(), outputs.clone());

        assert_eq!(call.module_name, "test_module");
        assert_eq!(call.inputs.len(), 1);
        assert_eq!(call.outputs.len(), 1);
        assert!(call.duration.is_none());
    }

    #[test]
    fn test_module_call_with_duration() {
        let call = ModuleCall::new("test", HashMap::new(), HashMap::new())
            .with_duration(Duration::from_millis(100));

        assert_eq!(call.duration, Some(Duration::from_millis(100)));
    }

    // ===== MetricResult Tests =====

    #[test]
    fn test_metric_result_boolean() {
        let result = MetricResult::Boolean(true);
        assert!(result.as_bool());
        assert_eq!(result.as_score(), 1.0);

        let result = MetricResult::Boolean(false);
        assert!(!result.as_bool());
        assert_eq!(result.as_score(), 0.0);
    }

    #[test]
    fn test_metric_result_score() {
        let result = MetricResult::Score(0.75);
        assert!(result.as_bool()); // > 0.5
        assert_eq!(result.as_score(), 0.75);

        let result = MetricResult::Score(0.25);
        assert!(!result.as_bool()); // <= 0.5
        assert_eq!(result.as_score(), 0.25);
    }

    #[test]
    fn test_metric_result_from_bool() {
        let result = MetricResult::from_bool(true);
        assert_eq!(result, MetricResult::Boolean(true));

        let result: MetricResult = true.into();
        assert_eq!(result, MetricResult::Boolean(true));
    }

    #[test]
    fn test_metric_result_from_score() {
        let result = MetricResult::from_score(0.8);
        assert_eq!(result, MetricResult::Score(0.8));

        let result: MetricResult = 0.8.into();
        assert_eq!(result, MetricResult::Score(0.8));
    }

    // ===== EvaluationPoint Tests =====

    #[test]
    fn test_evaluation_point_creation() {
        let example = Example::new(HashMap::new(), HashMap::new());
        let prediction = HashMap::new();

        let point = EvaluationPoint::new(example, prediction, 0.9);

        assert_eq!(point.score, 0.9);
        assert!(point.is_success());
        assert!(point.error.is_none());
    }

    #[test]
    fn test_evaluation_point_failed() {
        let example = Example::new(HashMap::new(), HashMap::new());
        let point = EvaluationPoint::failed(example, "Test error".to_string());

        assert_eq!(point.score, 0.0);
        assert!(!point.is_success());
        assert_eq!(point.error, Some("Test error".to_string()));
    }

    #[test]
    fn test_evaluation_point_with_duration() {
        let example = Example::new(HashMap::new(), HashMap::new());
        let point = EvaluationPoint::new(example, HashMap::new(), 1.0)
            .with_duration(Duration::from_secs(2));

        assert_eq!(point.duration, Some(Duration::from_secs(2)));
    }

    // ===== EvaluationResult Tests =====

    #[test]
    fn test_evaluation_result_creation() {
        let example1 = Example::new(HashMap::new(), HashMap::new());
        let example2 = Example::new(HashMap::new(), HashMap::new());

        let results = vec![
            EvaluationPoint::new(example1, HashMap::new(), 1.0),
            EvaluationPoint::new(example2, HashMap::new(), 0.5),
        ];

        let result = EvaluationResult::new(results, Duration::from_secs(1));

        assert_eq!(result.score, 75.0); // (1.0 + 0.5) / 2 * 100
        assert_eq!(result.successful, 2);
        assert_eq!(result.failed, 0);
    }

    #[test]
    fn test_evaluation_result_with_failures() {
        let example1 = Example::new(HashMap::new(), HashMap::new());
        let example2 = Example::new(HashMap::new(), HashMap::new());

        let results = vec![
            EvaluationPoint::new(example1, HashMap::new(), 1.0),
            EvaluationPoint::failed(example2, "Error".to_string()),
        ];

        let result = EvaluationResult::new(results, Duration::from_secs(1));

        assert_eq!(result.successful, 1);
        assert_eq!(result.failed, 1);
        assert_eq!(result.success_rate(), 0.5);
    }

    #[test]
    fn test_evaluation_result_failed_examples() {
        let example1 = Example::new(HashMap::new(), HashMap::new());
        let example2 = Example::new(HashMap::new(), HashMap::new());

        let results = vec![
            EvaluationPoint::new(example1, HashMap::new(), 1.0),
            EvaluationPoint::failed(example2, "Error".to_string()),
        ];

        let result = EvaluationResult::new(results, Duration::from_secs(1));
        let failed = result.failed_examples();

        assert_eq!(failed.len(), 1);
        assert_eq!(failed[0].error, Some("Error".to_string()));
    }

    #[test]
    fn test_evaluation_result_successful_examples() {
        let example1 = Example::new(HashMap::new(), HashMap::new());
        let example2 = Example::new(HashMap::new(), HashMap::new());

        let results = vec![
            EvaluationPoint::new(example1, HashMap::new(), 1.0),
            EvaluationPoint::failed(example2, "Error".to_string()),
        ];

        let result = EvaluationResult::new(results, Duration::from_secs(1));
        let successful = result.successful_examples();

        assert_eq!(successful.len(), 1);
        assert_eq!(successful[0].score, 1.0);
    }

    #[test]
    fn test_evaluation_result_empty() {
        let result = EvaluationResult::new(vec![], Duration::from_secs(0));

        assert_eq!(result.score, 0.0);
        assert_eq!(result.successful, 0);
        assert_eq!(result.failed, 0);
        assert_eq!(result.success_rate(), 0.0);
    }
}
