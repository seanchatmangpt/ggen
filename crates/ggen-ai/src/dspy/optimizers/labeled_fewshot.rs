//! LabeledFewShot Optimizer
//!
//! Simplest optimizer: select first k examples from labeled trainset.
//! No validation, no quality filtering, static selection.
//! Production readiness: ⭐⭐☆☆☆ (Baseline only)
//!
//! Use case: Quick baseline with manually labeled data

use super::{OptimizationStatistics, Optimizer};
use crate::dspy::optimizer::{Demonstration, Example, OptimizedPredictor};
use crate::dspy::{Module, ModuleError};
use async_trait::async_trait;
use tracing::{debug, info};

/// LabeledFewShot optimizer - simple baseline
///
/// Selects first k examples from training set without validation.
/// Assumes all training examples are high-quality and correctly labeled.
///
/// # Algorithm
/// ```text
/// 1. Select first k examples from labeled trainset
/// 2. Format as few-shot demonstrations
/// 3. Prepend to prompt
/// ```
///
/// # Mathematical Formulation
/// ```text
/// D = {(x₁, y₁), (x₂, y₂), ..., (xₙ, yₙ)}  // Training set
/// P_optimized(x) = LLM(D[:k] + x)           // First k examples + query
/// ```
pub struct LabeledFewShot {
    /// Number of examples to select
    k: usize,

    /// Whether to shuffle examples before selection
    shuffle: bool,
}

impl LabeledFewShot {
    /// Create new LabeledFewShot optimizer
    ///
    /// # Arguments
    /// * `k` - Number of examples to use (default: 4)
    ///
    /// # Example
    /// ```ignore
    /// use ggen_ai::dspy::optimizers::LabeledFewShot;
    ///
    /// let optimizer = LabeledFewShot::new(8);
    /// ```
    pub fn new(k: usize) -> Self {
        Self {
            k: k.max(1),
            shuffle: false,
        }
    }

    /// Enable shuffling of examples before selection
    pub fn with_shuffle(mut self, shuffle: bool) -> Self {
        self.shuffle = shuffle;
        self
    }

    /// Get number of examples to select
    pub fn k(&self) -> usize {
        self.k
    }
}

impl Default for LabeledFewShot {
    fn default() -> Self {
        Self::new(4)
    }
}

#[async_trait]
impl Optimizer for LabeledFewShot {
    async fn compile(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<Box<dyn Module>, ModuleError> {
        if trainset.is_empty() {
            return Err(ModuleError::Other(
                "Training set is empty. Provide at least one example.".to_string(),
            ));
        }

        info!(
            "LabeledFewShot: Selecting {} examples from {} available",
            self.k.min(trainset.len()),
            trainset.len()
        );

        // Select examples (with optional shuffling)
        let mut examples = trainset.to_vec();
        if self.shuffle {
            use rand::seq::SliceRandom;
            examples.shuffle(&mut rand::rng());
            debug!("Shuffled {} examples", examples.len());
        }

        // Take first k examples
        let selected: Vec<&Example> = examples.iter().take(self.k).collect();

        // Convert to demonstrations
        let demonstrations: Vec<Demonstration> = selected
            .iter()
            .map(|ex| Demonstration::new(ex.inputs.clone(), ex.outputs.clone()))
            .collect();

        debug!("Created {} demonstrations", demonstrations.len());

        // Create optimized predictor
        Ok(Box::new(OptimizedPredictor::new(
            student.signature().clone(),
            demonstrations,
        )))
    }

    async fn compile_with_stats(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<(Box<dyn Module>, OptimizationStatistics), ModuleError> {
        use std::time::SystemTime;

        let start = SystemTime::now();
        let optimized = self.compile(student, trainset).await?;
        let elapsed = start.elapsed().unwrap_or_default().as_millis() as u64;

        let num_selected = self.k.min(trainset.len());
        let stats = OptimizationStatistics {
            total_attempts: trainset.len(),
            successful_demos: num_selected,
            failed_demos: 0,
            avg_metric_score: 1.0, // No validation, assume all good
            optimization_time_ms: elapsed,
            metadata: std::collections::HashMap::new(),
        };

        Ok((optimized, stats))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};
    use crate::Signature;
    use serde_json::Value;
    use std::collections::HashMap;

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

    // ===== Constructor Tests =====

    #[test]
    fn test_labeled_fewshot_new() {
        let optimizer = LabeledFewShot::new(8);
        assert_eq!(optimizer.k(), 8);
        assert!(!optimizer.shuffle);
    }

    #[test]
    fn test_labeled_fewshot_default() {
        let optimizer = LabeledFewShot::default();
        assert_eq!(optimizer.k(), 4);
        assert!(!optimizer.shuffle);
    }

    #[test]
    fn test_labeled_fewshot_min_k() {
        let optimizer = LabeledFewShot::new(0);
        assert_eq!(optimizer.k(), 1); // Should be clamped to 1
    }

    #[test]
    fn test_labeled_fewshot_with_shuffle() {
        let optimizer = LabeledFewShot::new(4).with_shuffle(true);
        assert!(optimizer.shuffle);
    }

    // ===== Builder Pattern Tests =====

    #[test]
    fn test_builder_pattern() {
        let optimizer = LabeledFewShot::new(10).with_shuffle(true);
        assert_eq!(optimizer.k(), 10);
        assert!(optimizer.shuffle);
    }

    // ===== Compile Tests =====

    #[tokio::test]
    async fn test_compile_empty_trainset() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(4);

        let result = optimizer.compile(&predictor, &[]).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("empty"));
    }

    #[tokio::test]
    async fn test_compile_success() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(3);
        let examples = create_test_examples(5);

        let result = optimizer.compile(&predictor, &examples).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_compile_selects_k_examples() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(3);
        let examples = create_test_examples(10);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        // Downcast to OptimizedPredictor to check demonstrations
        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .expect("Should be OptimizedPredictor");

        assert_eq!(optimized_predictor.demonstration_count(), 3);
    }

    #[tokio::test]
    async fn test_compile_fewer_examples_than_k() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(10);
        let examples = create_test_examples(3);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .expect("Should be OptimizedPredictor");

        // Should select all available examples (3)
        assert_eq!(optimized_predictor.demonstration_count(), 3);
    }

    #[tokio::test]
    async fn test_compile_with_shuffle() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(3).with_shuffle(true);
        let examples = create_test_examples(10);

        let result = optimizer.compile(&predictor, &examples).await;
        assert!(result.is_ok());
        // Note: Can't test randomness deterministically, just ensure it compiles
    }

    // ===== Statistics Tests =====

    #[tokio::test]
    async fn test_compile_with_stats() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(4);
        let examples = create_test_examples(10);

        let (optimized, stats) = optimizer
            .compile_with_stats(&predictor, &examples)
            .await
            .unwrap();

        assert!(optimized.as_any().is::<OptimizedPredictor>());
        assert_eq!(stats.total_attempts, 10);
        assert_eq!(stats.successful_demos, 4);
        assert_eq!(stats.failed_demos, 0);
        assert_eq!(stats.avg_metric_score, 1.0);
        assert!(stats.optimization_time_ms > 0);
    }

    #[tokio::test]
    async fn test_compile_with_stats_fewer_examples() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(8);
        let examples = create_test_examples(5);

        let (_optimized, stats) = optimizer
            .compile_with_stats(&predictor, &examples)
            .await
            .unwrap();

        assert_eq!(stats.total_attempts, 5);
        assert_eq!(stats.successful_demos, 5); // All available
        assert_eq!(stats.success_rate(), 1.0);
    }

    // ===== Edge Cases =====

    #[tokio::test]
    async fn test_compile_single_example() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(1);
        let examples = create_test_examples(1);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .unwrap();

        assert_eq!(optimized_predictor.demonstration_count(), 1);
    }

    #[tokio::test]
    async fn test_compile_large_k() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(1000);
        let examples = create_test_examples(10);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .unwrap();

        // Should cap at number of available examples
        assert_eq!(optimized_predictor.demonstration_count(), 10);
    }

    // ===== Integration Tests =====

    #[tokio::test]
    async fn test_demonstration_content() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(2);
        let examples = create_test_examples(5);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let optimized_predictor = optimized
            .as_any()
            .downcast_ref::<OptimizedPredictor>()
            .unwrap();

        let demos = optimized_predictor.demonstrations();
        assert_eq!(demos.len(), 2);

        // Check first demonstration content
        assert_eq!(
            demos[0].inputs.get("question").unwrap(),
            &Value::String("Question 0".to_string())
        );
        assert_eq!(
            demos[0].outputs.get("answer").unwrap(),
            &Value::String("Answer 0".to_string())
        );
    }

    #[tokio::test]
    async fn test_signature_preservation() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig.clone());
        let optimizer = LabeledFewShot::new(3);
        let examples = create_test_examples(5);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        assert_eq!(optimized.signature().name, sig.name);
        assert_eq!(optimized.signature().description, sig.description);
    }

    // ===== Success Rate Tests =====

    #[tokio::test]
    async fn test_success_rate_calculation() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(7);
        let examples = create_test_examples(10);

        let (_optimized, stats) = optimizer
            .compile_with_stats(&predictor, &examples)
            .await
            .unwrap();

        assert_eq!(stats.success_rate(), 0.7); // 7/10
    }

    // ===== Getter Tests =====

    #[test]
    fn test_k_getter() {
        let optimizer = LabeledFewShot::new(5);
        assert_eq!(optimizer.k(), 5);
    }

    // ===== Multiple Compilation Tests =====

    #[tokio::test]
    async fn test_multiple_compilations() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = LabeledFewShot::new(3);
        let examples = create_test_examples(10);

        // First compilation
        let result1 = optimizer.compile(&predictor, &examples).await;
        assert!(result1.is_ok());

        // Second compilation
        let result2 = optimizer.compile(&predictor, &examples).await;
        assert!(result2.is_ok());
    }
}
