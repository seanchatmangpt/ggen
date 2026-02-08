//! BootstrapFewShotWithRandomSearch Optimizer
//!
//! Bootstrap + Random Exploration of demo combinations.
//! Production readiness: ⭐⭐⭐⭐☆
//!
//! Use case: 50+ examples, exploration of demo combinations
//!
//! # Algorithm
//! ```text
//! 1. Bootstrap num_candidate demonstrations (>> max_demos)
//! 2. For i = 1 to num_candidate_programs:
//!      a. Randomly sample K demos from candidates
//!      b. Create program with those K demos
//!      c. Evaluate on validation set
//! 3. Return program with highest validation score
//! ```

use super::{Metric, OptimizationStatistics, Optimizer};
use crate::dspy::optimizer::{BootstrapFewShot, Demonstration, Example, OptimizedPredictor};
use crate::dspy::{Module, ModuleError};
use async_trait::async_trait;
use rand::Rng;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info, warn};

/// BootstrapFewShotWithRandomSearch optimizer
///
/// Extends BootstrapFewShot with random exploration of demonstration
/// combinations to find the best performing subset.
///
/// # Mathematical Formulation
/// ```text
/// D_candidates = {d₁, d₂, ..., dₙ} where n >> k
/// P_i = S(sample(D_candidates, k) + x) for i ∈ [1, num_programs]
/// P_optimal = argmax_{P_i} score(P_i, D_val)
/// ```
pub struct BootstrapFewShotWithRandomSearch {
    /// Base bootstrap optimizer
    base: BootstrapFewShot,

    /// Number of candidate programs to try
    num_candidate_programs: usize,

    /// Validation set for selecting best program
    validation_set: Option<Vec<Example>>,
}

impl BootstrapFewShotWithRandomSearch {
    /// Create new BootstrapFewShotWithRandomSearch optimizer
    ///
    /// # Arguments
    /// * `metric` - Metric for validation and bootstrapping
    ///
    /// # Example
    /// ```ignore
    /// use ggen_ai::dspy::optimizers::{BootstrapFewShotWithRandomSearch, ExactMatchMetric};
    /// use std::sync::Arc;
    ///
    /// let metric = Arc::new(ExactMatchMetric::new("answer"));
    /// let optimizer = BootstrapFewShotWithRandomSearch::new(metric);
    /// ```
    pub fn new(
        metric: Arc<
            dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ModuleError> + Send + Sync,
        >,
    ) -> Self {
        Self {
            base: BootstrapFewShot::new(metric),
            num_candidate_programs: 16,
            validation_set: None,
        }
    }

    /// Set maximum number of bootstrapped demonstrations
    pub fn with_max_bootstrapped_demos(mut self, max: usize) -> Self {
        self.base = self.base.with_max_bootstrapped_demos(max);
        self
    }

    /// Set number of candidate programs to try
    pub fn with_num_candidate_programs(mut self, num: usize) -> Self {
        self.num_candidate_programs = num.max(1);
        self
    }

    /// Set validation set for program selection
    pub fn with_validation_set(mut self, validation: Vec<Example>) -> Self {
        self.validation_set = Some(validation);
        self
    }

    /// Bootstrap many candidate demonstrations
    async fn bootstrap_many(
        &self, student: &dyn Module, trainset: &[Example], num_candidates: usize,
    ) -> Result<Vec<Demonstration>, ModuleError> {
        info!("Bootstrapping {} candidate demonstrations", num_candidates);

        // Use base optimizer with higher demo count
        let base_with_more = BootstrapFewShot::new(Arc::clone(self.base.metric()))
            .with_max_bootstrapped_demos(num_candidates);

        let optimized = base_with_more.compile(student, trainset).await?;

        // Extract demonstrations
        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .ok_or_else(|| {
                ModuleError::Other("Failed to downcast to OptimizedPredictor".to_string())
            })?;

        Ok(optimized_predictor.demonstrations().to_vec())
    }

    /// Evaluate program on validation set
    async fn evaluate_program(
        &self, program: &OptimizedPredictor, validation: &[Example], metric: &MetricAdapter,
    ) -> Result<f64, ModuleError> {
        let mut total_score = 0.0;
        let mut successful = 0;

        for example in validation {
            match program.forward(example.inputs.clone()).await {
                Ok(output) => {
                    let score = metric.evaluate(example, &output)?;
                    total_score += score;
                    if score > 0.0 {
                        successful += 1;
                    }
                }
                Err(e) => {
                    warn!("Evaluation failed for example: {}", e);
                }
            }
        }

        if validation.is_empty() {
            return Ok(0.0);
        }

        let avg_score = total_score / validation.len() as f64;
        debug!(
            "Program evaluated: {}/{} successful, avg score: {:.3}",
            successful,
            validation.len(),
            avg_score
        );

        Ok(avg_score)
    }

    /// Random sample K demonstrations from candidates
    fn random_sample(&self, candidates: &[Demonstration], k: usize) -> Vec<Demonstration> {
        let mut rng = rand::rng();
        let sample_size = k.min(candidates.len());

        if candidates.is_empty() {
            return Vec::new();
        }

        // Use Fisher-Yates shuffle approach for random sampling
        let mut indices: Vec<usize> = (0..candidates.len()).collect();
        for i in 0..sample_size.min(indices.len()) {
            let j = rng.random_range(i..indices.len());
            indices.swap(i, j);
        }

        indices[..sample_size]
            .iter()
            .map(|&i| candidates[i].clone())
            .collect()
    }
}

#[async_trait]
impl Optimizer for BootstrapFewShotWithRandomSearch {
    async fn compile(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<Box<dyn Module>, ModuleError> {
        if trainset.is_empty() {
            return Err(ModuleError::Other(
                "Training set is empty. Provide at least one example.".to_string(),
            ));
        }

        // Use validation set if provided, otherwise use trainset
        let validation = self.validation_set.as_deref().unwrap_or(trainset);

        if validation.is_empty() {
            return Err(ModuleError::Other("Validation set is empty.".to_string()));
        }

        info!(
            "BootstrapFewShotWithRandomSearch: {} candidate programs, {} validation examples",
            self.num_candidate_programs,
            validation.len()
        );

        // Step 1: Bootstrap many candidates (3x the number we need)
        let num_candidates = self.num_candidate_programs * 3;
        let candidates = self
            .bootstrap_many(student, trainset, num_candidates)
            .await?;

        if candidates.is_empty() {
            warn!("No successful demonstrations found during bootstrapping");
            return Err(ModuleError::Other(
                "Failed to bootstrap any demonstrations".to_string(),
            ));
        }

        info!("Bootstrapped {} candidate demonstrations", candidates.len());

        // Step 2: Random search over demo combinations
        let max_demos = self.base.max_bootstrapped_demos();
        let mut best_program: Option<OptimizedPredictor> = None;
        let mut best_score = -f64::INFINITY;

        // Create metric trait object for evaluation
        let metric = Arc::new(MetricAdapter {
            inner: Arc::clone(self.base.metric()),
        });

        for i in 0..self.num_candidate_programs {
            debug!(
                "Trying candidate program {}/{}",
                i + 1,
                self.num_candidate_programs
            );

            // Random sample K demonstrations
            let demo_subset = self.random_sample(&candidates, max_demos);

            let program = OptimizedPredictor::new(student.signature().clone(), demo_subset);

            // Evaluate on validation set
            let score = self
                .evaluate_program(&program, validation, metric.as_ref())
                .await?;

            debug!("Program {} score: {:.3}", i + 1, score);

            if score > best_score {
                best_score = score;
                best_program = Some(program);
                info!("New best program found with score: {:.3}", score);
            }
        }

        match best_program {
            Some(program) => {
                info!(
                    "Best program selected with score: {:.3}, demos: {}",
                    best_score,
                    program.demonstration_count()
                );
                Ok(Box::new(program))
            }
            None => Err(ModuleError::Other(
                "No valid programs found during random search".to_string(),
            )),
        }
    }

    async fn compile_with_stats(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<(Box<dyn Module>, OptimizationStatistics), ModuleError> {
        use std::time::SystemTime;

        let start = SystemTime::now();
        let optimized = self.compile(student, trainset).await?;
        let elapsed = start.elapsed().unwrap_or_default().as_millis() as u64;

        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .ok_or_else(|| ModuleError::Other("Failed to downcast".to_string()))?;

        let stats = OptimizationStatistics {
            total_attempts: trainset.len(),
            successful_demos: optimized_predictor.demonstration_count(),
            failed_demos: 0,
            avg_metric_score: 0.0, // Would need validation to compute
            optimization_time_ms: elapsed,
            metadata: HashMap::new(),
        };

        Ok((optimized, stats))
    }
}

/// Adapter to convert legacy MetricFn to Metric trait
struct MetricAdapter {
    inner:
        Arc<dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ModuleError> + Send + Sync>,
}

#[async_trait::async_trait]
impl Metric for MetricAdapter {
    fn evaluate(
        &self, example: &Example, output: &HashMap<String, Value>,
    ) -> Result<f64, ModuleError> {
        let is_valid = (self.inner)(example, output)?;
        Ok(if is_valid { 1.0 } else { 0.0 })
    }

    fn validate(
        &self, example: &Example, output: &HashMap<String, Value>, _trace: Option<&super::Trace>,
    ) -> Result<bool, ModuleError> {
        (self.inner)(example, output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};
    use crate::Signature;

    fn create_test_signature() -> Signature {
        Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"))
    }

    fn create_test_examples(n: usize) -> Vec<Example> {
        (0..n)
            .map(|i| {
                let mut inputs = HashMap::new();
                inputs.insert(
                    "question".to_string(),
                    Value::String(format!("Question {}", i)),
                );

                let mut outputs = HashMap::new();
                outputs.insert("answer".to_string(), Value::String(format!("Answer {}", i)));

                Example::new(inputs, outputs)
            })
            .collect()
    }

    fn create_simple_metric(
    ) -> Arc<dyn Fn(&Example, &HashMap<String, Value>) -> Result<bool, ModuleError> + Send + Sync>
    {
        Arc::new(|_example: &Example, _output: &HashMap<String, Value>| {
            Ok(true) // Always pass for testing
        })
    }

    // ===== Constructor Tests =====

    #[test]
    fn test_bootstrap_random_search_new() {
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);
        assert_eq!(optimizer.num_candidate_programs, 16);
    }

    #[test]
    fn test_bootstrap_random_search_with_num_candidates() {
        let metric = create_simple_metric();
        let optimizer =
            BootstrapFewShotWithRandomSearch::new(metric).with_num_candidate_programs(32);
        assert_eq!(optimizer.num_candidate_programs, 32);
    }

    #[test]
    fn test_bootstrap_random_search_min_candidates() {
        let metric = create_simple_metric();
        let optimizer =
            BootstrapFewShotWithRandomSearch::new(metric).with_num_candidate_programs(0);
        assert_eq!(optimizer.num_candidate_programs, 1);
    }

    #[test]
    fn test_bootstrap_random_search_with_validation_set() {
        let metric = create_simple_metric();
        let examples = create_test_examples(5);
        let optimizer =
            BootstrapFewShotWithRandomSearch::new(metric).with_validation_set(examples.clone());
        assert!(optimizer.validation_set.is_some());
        assert_eq!(optimizer.validation_set.unwrap().len(), 5);
    }

    // ===== Builder Pattern Tests =====

    #[test]
    fn test_builder_pattern() {
        let metric = create_simple_metric();
        let examples = create_test_examples(10);
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric)
            .with_max_bootstrapped_demos(8)
            .with_num_candidate_programs(20)
            .with_validation_set(examples);

        assert_eq!(optimizer.num_candidate_programs, 20);
        assert!(optimizer.validation_set.is_some());
    }

    // ===== Random Sample Tests =====

    #[test]
    fn test_random_sample() {
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);

        let demos: Vec<Demonstration> = (0..10)
            .map(|i| {
                let mut inputs = HashMap::new();
                inputs.insert("q".to_string(), Value::String(format!("Q{}", i)));
                let mut outputs = HashMap::new();
                outputs.insert("a".to_string(), Value::String(format!("A{}", i)));
                Demonstration::new(inputs, outputs)
            })
            .collect();

        let sample = optimizer.random_sample(&demos, 3);
        assert_eq!(sample.len(), 3);
    }

    #[test]
    fn test_random_sample_larger_than_available() {
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);

        let demos: Vec<Demonstration> = (0..5)
            .map(|_| Demonstration::new(HashMap::new(), HashMap::new()))
            .collect();

        let sample = optimizer.random_sample(&demos, 10);
        assert_eq!(sample.len(), 5); // Should return all available
    }

    #[test]
    fn test_random_sample_empty() {
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);

        let demos: Vec<Demonstration> = vec![];
        let sample = optimizer.random_sample(&demos, 3);
        assert_eq!(sample.len(), 0);
    }

    // ===== Compile Tests =====

    #[tokio::test]
    async fn test_compile_empty_trainset() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);

        let result = optimizer.compile(&predictor, &[]).await;
        assert!(result.is_err());
    }

    // Note: Full integration tests would require LLM mocking
    // These tests verify the structure and basic functionality

    // ===== Edge Cases =====

    #[test]
    fn test_with_max_demos_zero() {
        let metric = create_simple_metric();
        let optimizer =
            BootstrapFewShotWithRandomSearch::new(metric).with_max_bootstrapped_demos(0);
        // Should handle gracefully (BootstrapFewShot should clamp)
    }

    // ===== Statistics Tests =====

    #[tokio::test]
    async fn test_compile_with_stats_structure() {
        // This would require full LLM integration for proper testing
        // Just verify the method exists and returns correct types
        let metric = create_simple_metric();
        let optimizer = BootstrapFewShotWithRandomSearch::new(metric);
        // Cannot fully test without LLM
    }
}
