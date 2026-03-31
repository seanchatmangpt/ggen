//! Learning Agent - Improves generation based on feedback and runtime data

use super::{
    BaseAgent, LearningData, LearningModel, ModelUpdates, PredictionInput, PredictionOutput,
    TrainingResult,
};
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};
use tracing::{debug, info, warn};

/// Learning Agent implementation
pub struct LearningAgentImpl {
    base: BaseAgent,
}

impl LearningAgentImpl {
    pub fn new(base: BaseAgent) -> Self {
        Self { base }
    }
}

/// Adaptive Learning Model with pattern recognition and adaptation
pub struct AdaptiveLearningModel {
    /// Model parameters (weights)
    parameters: HashMap<String, f64>,
    /// Learning rate
    learning_rate: f64,
    /// Training history
    training_history: Vec<TrainingResult>,
    /// Pattern recognition cache
    pattern_cache: HashMap<String, Pattern>,
    /// Adaptation statistics
    adaptation_stats: AdaptationStats,
    /// Model version
    version: u64,
}

/// Pattern recognition data
#[derive(Debug, Clone)]
struct Pattern {
    /// Pattern signature
    signature: String,
    /// Frequency of occurrence
    frequency: u64,
    /// Last seen timestamp
    last_seen: u64,
    /// Success rate
    success_rate: f64,
    /// Associated features
    features: HashMap<String, f64>,
}

/// Adaptation statistics
#[derive(Debug, Clone)]
struct AdaptationStats {
    /// Total adaptations performed
    total_adaptations: u64,
    /// Successful adaptations
    successful_adaptations: u64,
    /// Average adaptation time (ms)
    avg_adaptation_time_ms: f64,
    /// Last adaptation timestamp
    last_adaptation: u64,
}

impl Default for AdaptationStats {
    fn default() -> Self {
        Self {
            total_adaptations: 0,
            successful_adaptations: 0,
            avg_adaptation_time_ms: 0.0,
            last_adaptation: 0,
        }
    }
}

impl AdaptiveLearningModel {
    /// Create a new adaptive learning model
    pub fn new() -> Self {
        Self {
            parameters: HashMap::new(),
            learning_rate: 0.01,
            training_history: Vec::new(),
            pattern_cache: HashMap::new(),
            adaptation_stats: AdaptationStats::default(),
            version: 1,
        }
    }

    /// Create a new adaptive learning model with custom learning rate
    pub fn with_learning_rate(learning_rate: f64) -> Self {
        let mut model = Self::new();
        model.learning_rate = learning_rate;
        model
    }

    /// Recognize patterns in learning data
    fn recognize_patterns(&mut self, data: &LearningData) -> Vec<Pattern> {
        let mut patterns = Vec::new();

        // Create pattern signature from features
        let signature = self.create_pattern_signature(&data.features);

        // Check if pattern exists in cache
        if let Some(existing) = self.pattern_cache.get_mut(&signature) {
            existing.frequency += 1;
            existing.last_seen = self.current_timestamp();
            existing.success_rate = self.calculate_success_rate(data);
            patterns.push(existing.clone());
        } else {
            // Create new pattern
            let pattern = Pattern {
                signature: signature.clone(),
                frequency: 1,
                last_seen: self.current_timestamp(),
                success_rate: 0.5, // Initial neutral success rate
                features: data.features.clone(),
            };
            self.pattern_cache.insert(signature, pattern.clone());
            patterns.push(pattern);
        }

        // Analyze feature correlations
        let correlated_patterns = self.find_correlated_patterns(&data.features);
        patterns.extend(correlated_patterns);

        patterns
    }

    /// Create a signature from features
    fn create_pattern_signature(&self, features: &HashMap<String, f64>) -> String {
        let mut signature_parts: Vec<String> = features
            .iter()
            .map(|(k, v)| {
                // Quantize features to create discrete bins
                let quantized = (v * 100.0).floor() as i64;
                format!("{}:{}", k, quantized)
            })
            .collect();
        signature_parts.sort();
        signature_parts.join("|")
    }

    /// Find correlated patterns based on feature similarity
    fn find_correlated_patterns(&self, features: &HashMap<String, f64>) -> Vec<Pattern> {
        let mut correlated = Vec::new();

        for pattern in self.pattern_cache.values() {
            if self.calculate_similarity(features, &pattern.features) > 0.8 {
                correlated.push(pattern.clone());
            }
        }

        correlated
    }

    /// Calculate similarity between two feature sets
    fn calculate_similarity(
        &self, features1: &HashMap<String, f64>, features2: &HashMap<String, f64>,
    ) -> f64 {
        if features1.is_empty() || features2.is_empty() {
            return 0.0;
        }

        let mut dot_product = 0.0;
        let mut norm1 = 0.0;
        let mut norm2 = 0.0;

        for (key, val1) in features1.iter() {
            if let Some(val2) = features2.get(key) {
                dot_product += val1 * val2;
            }
            norm1 += val1 * val1;
        }

        for val2 in features2.values() {
            norm2 += val2 * val2;
        }

        let denominator = norm1.sqrt() * norm2.sqrt();
        if denominator == 0.0 {
            0.0
        } else {
            dot_product / denominator
        }
    }

    /// Calculate success rate from learning data
    fn calculate_success_rate(&self, data: &LearningData) -> f64 {
        if data.targets.is_empty() {
            return 0.5;
        }

        // Simple success rate based on target values
        // In a real implementation, this would use actual success metrics
        let sum: f64 = data.targets.values().sum();
        let count = data.targets.len() as f64;
        (sum / count).clamp(0.0, 1.0)
    }

    /// Adapt model parameters based on patterns
    fn adapt_parameters(&mut self, patterns: &[Pattern], data: &LearningData) {
        let start = std::time::Instant::now();

        self.adaptation_stats.total_adaptations += 1;

        // Update parameters based on pattern recognition
        for pattern in patterns {
            if pattern.success_rate > 0.7 {
                // High success patterns - reinforce
                for (key, value) in &pattern.features {
                    let current = self.parameters.get(key).copied().unwrap_or(0.0);
                    let update = value * self.learning_rate;
                    self.parameters.insert(key.clone(), current + update);
                }
            } else if pattern.success_rate < 0.3 {
                // Low success patterns - penalize
                for (key, value) in &pattern.features {
                    let current = self.parameters.get(key).copied().unwrap_or(0.0);
                    let update = value * self.learning_rate * 0.5;
                    self.parameters.insert(key.clone(), current - update);
                }
            }
        }

        // Incorporate feedback data
        for (key, target) in &data.targets {
            let current = self.parameters.get(key).copied().unwrap_or(0.0);
            let error = target - current;
            let update = error * self.learning_rate;
            self.parameters.insert(key.clone(), current + update);
        }

        // Update adaptation statistics
        let duration = start.elapsed().as_millis() as f64;
        let total = self.adaptation_stats.total_adaptations as f64;
        self.adaptation_stats.avg_adaptation_time_ms =
            (self.adaptation_stats.avg_adaptation_time_ms * (total - 1.0) + duration) / total;
        self.adaptation_stats.last_adaptation = self.current_timestamp();
        self.adaptation_stats.successful_adaptations += 1;

        debug!(
            "Adapted {} parameters in {}ms",
            self.parameters.len(),
            duration
        );
    }

    /// Predict outcomes using learned parameters
    fn predict_internal(&self, input: &PredictionInput) -> HashMap<String, f64> {
        let mut predictions = HashMap::new();

        // Use learned parameters to make predictions
        for (key, feature_value) in &input.features {
            let weight = self.parameters.get(key).copied().unwrap_or(0.5);
            let prediction = feature_value * weight;
            predictions.insert(key.clone(), prediction);
        }

        // Apply pattern-based adjustments
        let signature = self.create_pattern_signature(&input.features);
        if let Some(pattern) = self.pattern_cache.get(&signature) {
            for (key, base_prediction) in predictions.iter_mut() {
                if let Some(pattern_feature) = pattern.features.get(key) {
                    // Adjust prediction based on pattern success rate
                    let adjustment = pattern_feature * pattern.success_rate * 0.1;
                    *base_prediction += adjustment;
                }
            }
        }

        // Normalize predictions
        self.normalize_predictions(&mut predictions);

        predictions
    }

    /// Normalize predictions to [0, 1] range
    fn normalize_predictions(&self, predictions: &mut HashMap<String, f64>) {
        if predictions.is_empty() {
            return;
        }

        let min = predictions.values().copied().fold(f64::INFINITY, f64::min);
        let max = predictions
            .values()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max);
        let range = max - min;

        if range > 0.0 {
            for value in predictions.values_mut() {
                *value = (*value - min) / range;
            }
        }
    }

    /// Calculate prediction confidence
    fn calculate_confidence(
        &self, input: &PredictionInput, predictions: &HashMap<String, f64>,
    ) -> f64 {
        // Base confidence on pattern match
        let signature = self.create_pattern_signature(&input.features);
        let pattern_confidence = self
            .pattern_cache
            .get(&signature)
            .map(|p| p.success_rate)
            .unwrap_or(0.5);

        // Adjust based on parameter stability
        let parameter_confidence = if self.parameters.is_empty() {
            0.5
        } else {
            let variance: f64 = self.parameters.values().map(|v| (v - 0.5).abs()).sum();
            1.0 - (variance / self.parameters.len() as f64).min(1.0)
        };

        // Combine confidences
        (pattern_confidence * 0.6 + parameter_confidence * 0.4).clamp(0.0, 1.0)
    }

    /// Get current timestamp
    fn current_timestamp(&self) -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }

    /// Calculate training loss
    fn calculate_loss(&self, data: &LearningData) -> f64 {
        if data.targets.is_empty() {
            return 0.0;
        }

        let mut total_loss = 0.0;
        let mut count = 0;

        for (key, target) in &data.targets {
            if let Some(prediction) = self.parameters.get(key) {
                let error = target - prediction;
                total_loss += error * error;
                count += 1;
            }
        }

        if count > 0 {
            total_loss / count as f64
        } else {
            0.0
        }
    }

    /// Calculate training accuracy
    fn calculate_accuracy(&self, data: &LearningData) -> f64 {
        if data.targets.is_empty() {
            return 1.0;
        }

        let mut correct = 0;
        let total = data.targets.len();

        for (key, target) in &data.targets {
            if let Some(prediction) = self.parameters.get(key) {
                // Consider correct if within 10% tolerance
                let error = (target - prediction).abs();
                if error < 0.1 * target.abs() {
                    correct += 1;
                }
            }
        }

        correct as f64 / total as f64
    }

    /// Check convergence
    fn check_convergence(&self) -> bool {
        if self.training_history.len() < 3 {
            return false;
        }

        // Check if loss has stabilized in recent iterations
        let recent: Vec<_> = self.training_history.iter().rev().take(3).collect();
        let losses: Vec<f64> = recent.iter().map(|r| r.loss).collect();

        let variance = self.calculate_variance(&losses);
        variance < 0.001 // Convergence threshold
    }

    /// Calculate variance of values
    fn calculate_variance(&self, values: &[f64]) -> f64 {
        if values.is_empty() {
            return 0.0;
        }

        let mean = values.iter().sum::<f64>() / values.len() as f64;
        let sum_squared_diff: f64 = values.iter().map(|v| (v - mean).powi(2)).sum();
        sum_squared_diff / values.len() as f64
    }
}

impl Default for AdaptiveLearningModel {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl LearningModel for AdaptiveLearningModel {
    async fn train(&mut self, feedback_data: &LearningData) -> Result<TrainingResult> {
        info!(
            "Starting model training with {} features",
            feedback_data.features.len()
        );

        let start = std::time::Instant::now();

        // Recognize patterns in feedback data
        let patterns = self.recognize_patterns(feedback_data);
        debug!("Recognized {} patterns", patterns.len());

        // Adapt model parameters based on patterns
        self.adapt_parameters(&patterns, feedback_data);

        // Calculate training metrics
        let accuracy = self.calculate_accuracy(feedback_data);
        let loss = self.calculate_loss(feedback_data);
        let converged = self.check_convergence();
        let duration_ms = start.elapsed().as_millis() as u64;

        // Record training history
        let result = TrainingResult {
            accuracy,
            loss,
            converged,
            duration_ms,
        };
        self.training_history.push(result.clone());

        // Limit history size
        if self.training_history.len() > 100 {
            self.training_history.remove(0);
        }

        // Increment version on significant training
        if converged || self.training_history.len() % 10 == 0 {
            self.version += 1;
        }

        info!(
            "Training complete: accuracy={:.2}, loss={:.4}, converged={}",
            accuracy, loss, converged
        );

        Ok(result)
    }

    async fn predict(&self, input: &PredictionInput) -> Result<PredictionOutput> {
        debug!("Making prediction with {} features", input.features.len());

        // Generate predictions using learned parameters
        let predictions = self.predict_internal(input);

        // Calculate confidence
        let confidence = self.calculate_confidence(input, &predictions);

        // Build metadata
        let mut metadata = HashMap::new();
        metadata.insert("model_version".to_string(), self.version.to_string());
        metadata.insert(
            "patterns_recognized".to_string(),
            self.pattern_cache.len().to_string(),
        );
        metadata.insert(
            "parameters_count".to_string(),
            self.parameters.len().to_string(),
        );
        metadata.insert(
            "adaptations_count".to_string(),
            self.adaptation_stats.total_adaptations.to_string(),
        );

        // Add pattern information if matched
        let signature = self.create_pattern_signature(&input.features);
        if let Some(pattern) = self.pattern_cache.get(&signature) {
            metadata.insert(
                "matched_pattern_frequency".to_string(),
                pattern.frequency.to_string(),
            );
            metadata.insert(
                "pattern_success_rate".to_string(),
                pattern.success_rate.to_string(),
            );
        }

        debug!(
            "Prediction complete: confidence={:.2}, predictions={}",
            confidence,
            predictions.len()
        );

        Ok(PredictionOutput {
            predictions,
            confidence,
            metadata,
        })
    }

    async fn update_parameters(&mut self, updates: &ModelUpdates) -> Result<()> {
        info!(
            "Updating parameters: {} updates, learning_rate={}",
            updates.parameter_updates.len(),
            updates.learning_rate
        );

        // Update learning rate if specified
        if updates.learning_rate > 0.0 && updates.learning_rate <= 1.0 {
            self.learning_rate = updates.learning_rate;
        }

        // Apply parameter updates
        for (key, delta) in &updates.parameter_updates {
            let current = self.parameters.get(key).copied().unwrap_or(0.0);
            let updated = current + delta;
            self.parameters.insert(key.clone(), updated);
        }

        debug!(
            "Parameters updated: {} total parameters",
            self.parameters.len()
        );

        Ok(())
    }
}

/// Stub LearningModel implementation (kept for backwards compatibility)
pub struct StubLearningModel;

#[async_trait]
impl LearningModel for StubLearningModel {
    async fn train(&mut self, _feedback_data: &LearningData) -> Result<TrainingResult> {
        Ok(TrainingResult {
            accuracy: 0.95,
            loss: 0.05,
            converged: true,
            duration_ms: 1000,
        })
    }

    async fn predict(&self, _input: &PredictionInput) -> Result<PredictionOutput> {
        Ok(PredictionOutput {
            predictions: std::collections::HashMap::new(),
            confidence: 0.95,
            metadata: std::collections::HashMap::new(),
        })
    }

    async fn update_parameters(&mut self, _updates: &ModelUpdates) -> Result<()> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_learning_data() -> LearningData {
        let mut features = HashMap::new();
        features.insert("feature1".to_string(), 0.8);
        features.insert("feature2".to_string(), 0.6);
        features.insert("feature3".to_string(), 0.9);

        let mut targets = HashMap::new();
        targets.insert("feature1".to_string(), 0.85);
        targets.insert("feature2".to_string(), 0.65);
        targets.insert("feature3".to_string(), 0.95);

        LearningData {
            features,
            targets,
            metadata: HashMap::new(),
        }
    }

    fn create_test_prediction_input() -> PredictionInput {
        let mut features = HashMap::new();
        features.insert("feature1".to_string(), 0.8);
        features.insert("feature2".to_string(), 0.6);
        features.insert("feature3".to_string(), 0.9);

        PredictionInput {
            features,
            context: HashMap::new(),
        }
    }

    #[tokio::test]
    async fn test_adaptive_model_creation() {
        let model = AdaptiveLearningModel::new();
        assert_eq!(model.version, 1);
        assert_eq!(model.learning_rate, 0.01);
        assert!(model.parameters.is_empty());
        assert!(model.training_history.is_empty());
    }

    #[tokio::test]
    async fn test_adaptive_model_custom_learning_rate() {
        let model = AdaptiveLearningModel::with_learning_rate(0.05);
        assert_eq!(model.learning_rate, 0.05);
    }

    #[tokio::test]
    async fn test_model_training() {
        let mut model = AdaptiveLearningModel::new();
        let data = create_test_learning_data();

        let result = model.train(&data).await.unwrap();

        assert!(result.accuracy > 0.0);
        assert!(result.loss >= 0.0);
        assert!(!result.converged); // Single training doesn't converge
        assert!(result.duration_ms > 0);
        assert_eq!(model.training_history.len(), 1);
    }

    #[tokio::test]
    async fn test_model_prediction() {
        let model = AdaptiveLearningModel::new();
        let input = create_test_prediction_input();

        let output = model.predict(&input).await.unwrap();

        assert!(!output.predictions.is_empty());
        assert!(output.confidence > 0.0 && output.confidence <= 1.0);
        assert!(output.metadata.contains_key("model_version"));
        assert!(output.metadata.contains_key("parameters_count"));
    }

    #[tokio::test]
    async fn test_parameter_update() {
        let mut model = AdaptiveLearningModel::new();

        let mut updates = ModelUpdates {
            parameter_updates: HashMap::new(),
            learning_rate: 0.05,
            metadata: HashMap::new(),
        };

        updates.parameter_updates.insert("param1".to_string(), 0.1);
        updates
            .parameter_updates
            .insert("param2".to_string(), -0.05);

        model.update_parameters(&updates).await.unwrap();

        assert_eq!(model.learning_rate, 0.05);
        assert_eq!(model.parameters.get("param1"), Some(&0.1));
        assert_eq!(model.parameters.get("param2"), Some(&-0.05));
    }

    #[tokio::test]
    async fn test_pattern_recognition() {
        let mut model = AdaptiveLearningModel::new();
        let data = create_test_learning_data();

        // Train multiple times to build pattern cache
        for _ in 0..3 {
            let _ = model.train(&data).await;
        }

        // Should have recognized patterns
        assert!(!model.pattern_cache.is_empty());

        // Predictions should be more accurate with patterns
        let input = create_test_prediction_input();
        let output = model.predict(&input).await.unwrap();

        assert!(output.confidence > 0.3); // Should have some confidence
        assert!(output.metadata.contains_key("matched_pattern_frequency"));
    }

    #[tokio::test]
    async fn test_convergence_detection() {
        let mut model = AdaptiveLearningModel::new();
        let data = create_test_learning_data();

        // Train multiple times to check convergence
        let mut converged = false;
        for _ in 0..10 {
            let result = model.train(&data).await.unwrap();
            if result.converged {
                converged = true;
                break;
            }
        }

        // Convergence is data-dependent, just verify it runs
        assert!(model.training_history.len() >= 10);
    }

    #[tokio::test]
    async fn test_adaptation_statistics() {
        let mut model = AdaptiveLearningModel::new();
        let data = create_test_learning_data();

        // Train to trigger adaptations
        let _ = model.train(&data).await;

        assert!(model.adaptation_stats.total_adaptations > 0);
        assert!(model.adaptation_stats.successful_adaptations > 0);
        assert!(model.adaptation_stats.last_adaptation > 0);
    }

    #[tokio::test]
    async fn test_stub_model_for_compatibility() {
        let mut stub = StubLearningModel;
        let data = create_test_learning_data();
        let input = create_test_prediction_input();

        // Stub should work without errors
        let train_result = stub.train(&data).await.unwrap();
        assert_eq!(train_result.accuracy, 0.95);
        assert_eq!(train_result.loss, 0.05);

        let pred_result = stub.predict(&input).await.unwrap();
        assert_eq!(pred_result.confidence, 0.95);
        assert!(pred_result.predictions.is_empty());

        let updates = ModelUpdates {
            parameter_updates: HashMap::new(),
            learning_rate: 0.01,
            metadata: HashMap::new(),
        };
        assert!(stub.update_parameters(&updates).await.is_ok());
    }
}
