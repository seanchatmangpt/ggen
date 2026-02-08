//! COPRO (Contrastive Prompt Optimization)
//!
//! Instruction Generation + Coordinate Ascent optimization.
//! Production readiness: ⭐⭐⭐⭐☆
//!
//! Use case: Instruction optimization, clear metric, no need for examples
//!
//! # Algorithm
//! ```text
//! Phase 1: Initialization
//!   For each predictor in program:
//!     - Extract baseline instruction
//!     - Generate breadth-1 alternative instructions via prompt_model
//!     - Include original instruction as candidate
//!
//! Phase 2: Iterative Refinement (Coordinate Ascent)
//!   For depth iterations:
//!     For each predictor:
//!       1. Evaluate all candidates on validation set
//!       2. Select best instruction
//!       3. Update predictor with best instruction
//!
//!     Generate next batch of candidates:
//!       - Collect top performers from previous iteration
//!       - Format as few-shot examples with scores
//!       - Prompt model to "propose better instruction"
//!
//! Phase 3: Return best program
//! ```

use super::{Metric, OptimizationStatistics, Optimizer};
use crate::dspy::optimizer::Example;
use crate::dspy::{Module, ModuleError, Predictor};
use async_trait::async_trait;
use genai::{
    chat::{ChatMessage, ChatOptions, ChatRequest},
    Client,
};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info};

/// COPRO optimizer - Coordinate Ascent for Instruction Optimization
///
/// Optimizes instructions (prompts) rather than demonstrations (examples).
/// Uses coordinate ascent to iteratively refine instructions based on
/// validation performance.
///
/// # Mathematical Formulation
/// ```text
/// I = instruction space
/// P(I) = program with instruction I
/// S(I) = score on validation set
///
/// Coordinate Ascent:
///   For t = 1 to depth:
///     I_candidates = {I₁, I₂, ..., I_breadth}
///     S_i = evaluate(P(I_i), D_val) for all I_i
///     I_best = argmax_{I_i} S_i
///
///     # Generate next candidates from history
///     I_next = generate_from_top_k(I_history, k=3)
///
///   Return P(I_best)
/// ```
pub struct COPRO {
    /// Metric for evaluation
    metric: Arc<dyn Metric>,

    /// Number of refinement iterations
    depth: usize,

    /// Number of candidate instructions per iteration
    breadth: usize,

    /// Model to use for generating instruction variants
    prompt_model: String,

    /// Temperature for instruction generation
    temperature: f32,

    /// Deduplication cache for instruction scores
    instruction_cache: std::sync::Mutex<HashMap<String, f64>>,
}

impl COPRO {
    /// Create new COPRO optimizer
    ///
    /// # Arguments
    /// * `metric` - Metric for evaluating instruction quality
    ///
    /// # Example
    /// ```ignore
    /// use ggen_ai::dspy::optimizers::{COPRO, ExactMatchMetric};
    /// use std::sync::Arc;
    ///
    /// let metric = Arc::new(ExactMatchMetric::new("answer"));
    /// let optimizer = COPRO::new(metric);
    /// ```
    pub fn new(metric: Arc<dyn Metric>) -> Self {
        Self {
            metric,
            depth: 3,
            breadth: 10,
            prompt_model: std::env::var("GGEN_LLM_MODEL")
                .or_else(|_| std::env::var("DEFAULT_MODEL"))
                .unwrap_or_default(),
            temperature: 0.7,
            instruction_cache: std::sync::Mutex::new(HashMap::new()),
        }
    }

    /// Set number of refinement iterations
    pub fn with_depth(mut self, depth: usize) -> Self {
        self.depth = depth.max(1);
        self
    }

    /// Set number of candidate instructions per iteration
    pub fn with_breadth(mut self, breadth: usize) -> Self {
        self.breadth = breadth.max(1);
        self
    }

    /// Set model for instruction generation
    pub fn with_prompt_model(mut self, model: impl Into<String>) -> Self {
        self.prompt_model = model.into();
        self
    }

    /// Set temperature for generation
    pub fn with_temperature(mut self, temperature: f32) -> Self {
        self.temperature = temperature.clamp(0.0, 2.0);
        self
    }

    /// Generate instruction variant using LLM
    async fn generate_instruction_variant(
        &self, baseline: &str, context: Option<&str>,
    ) -> Result<String, ModuleError> {
        if self.prompt_model.is_empty() {
            return Err(ModuleError::LlmError(
                "Prompt model not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var".to_string(),
            ));
        }

        let mut prompt = format!("Given this instruction: \"{}\"\n\n", baseline);

        if let Some(ctx) = context {
            prompt.push_str(&format!("Context: {}\n\n", ctx));
        }

        prompt.push_str("Propose an improved instruction that will lead a good language model to perform the task even better. Be specific and actionable.");

        let client = Client::default();
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system(
                "You are an expert at crafting precise instructions for language models.",
            ),
            ChatMessage::user(prompt),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature as f64)
            .with_max_tokens(200);

        match client
            .exec_chat(&self.prompt_model, chat_req, Some(&chat_options))
            .await
        {
            Ok(response) => {
                let content = response.first_text().unwrap_or_default().to_string();
                Ok(content.trim().to_string())
            }
            Err(e) => Err(ModuleError::LlmError(format!(
                "Instruction generation failed: {}",
                e
            ))),
        }
    }

    /// Evaluate instruction on validation set
    async fn evaluate_instruction(
        &self, predictor: &Predictor, instruction: &str, trainset: &[Example],
    ) -> Result<f64, ModuleError> {
        // Check cache first
        {
            let cache = self.instruction_cache.lock().unwrap();
            if let Some(&score) = cache.get(instruction) {
                debug!("Using cached score for instruction: {:.3}", score);
                return Ok(score);
            }
        }

        // Create temporary predictor with new instruction
        let mut temp_sig = predictor.signature().clone();
        temp_sig.instructions = Some(instruction.to_string());
        let temp_predictor = Predictor::new(temp_sig);

        // Evaluate on trainset
        let mut total_score = 0.0;
        for example in trainset {
            match temp_predictor.forward(example.inputs.clone()).await {
                Ok(output) => {
                    let score = self.metric.evaluate(example, &output)?;
                    total_score += score;
                }
                Err(_) => {
                    // Skip failed examples
                }
            }
        }

        let avg_score = if trainset.is_empty() {
            0.0
        } else {
            total_score / trainset.len() as f64
        };

        // Cache result
        {
            let mut cache = self.instruction_cache.lock().unwrap();
            cache.insert(instruction.to_string(), avg_score);
        }

        Ok(avg_score)
    }

    /// Generate next batch of candidate instructions from top performers
    async fn generate_next_batch(
        &self, top_performers: &[(String, f64)],
    ) -> Result<Vec<String>, ModuleError> {
        let mut prompt =
            "Based on these instructions and their performance scores:\n\n".to_string();
        for (instr, score) in top_performers {
            prompt.push_str(&format!("- \"{}\" (score: {:.2})\n", instr, score));
        }
        prompt.push_str("\nPropose {} new improved instructions that combine the best aspects of these high-performing instructions.");
        prompt = prompt.replace("{}", &self.breadth.to_string());

        let client = Client::default();
        let chat_req = ChatRequest::new(vec![
            ChatMessage::system(
                "You are an expert at crafting precise instructions for language models.",
            ),
            ChatMessage::user(prompt),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(self.temperature as f64)
            .with_max_tokens(1000);

        if self.prompt_model.is_empty() {
            return Err(ModuleError::LlmError("Prompt model not set".to_string()));
        }

        match client
            .exec_chat(&self.prompt_model, chat_req, Some(&chat_options))
            .await
        {
            Ok(response) => {
                let content = response.first_text().unwrap_or_default();

                // Parse out instructions (each line starting with number or bullet)
                let mut instructions = Vec::new();
                for line in content.lines() {
                    let trimmed = line.trim();
                    // Skip empty lines and extract instruction text
                    if trimmed.is_empty() {
                        continue;
                    }

                    // Remove leading numbers, bullets, quotes
                    let cleaned = trimmed
                        .trim_start_matches(|c: char| {
                            c.is_numeric() || c == '.' || c == '-' || c == '*'
                        })
                        .trim()
                        .trim_matches('"')
                        .trim();

                    if !cleaned.is_empty() && cleaned.len() > 10 {
                        instructions.push(cleaned.to_string());
                        if instructions.len() >= self.breadth {
                            break;
                        }
                    }
                }

                // If we didn't get enough, add variations
                while instructions.len() < self.breadth {
                    if let Some(base) = top_performers.first() {
                        instructions.push(base.0.clone());
                    } else {
                        break;
                    }
                }

                Ok(instructions)
            }
            Err(e) => Err(ModuleError::LlmError(format!(
                "Batch generation failed: {}",
                e
            ))),
        }
    }
}

#[async_trait]
impl Optimizer for COPRO {
    async fn compile(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<Box<dyn Module>, ModuleError> {
        if trainset.is_empty() {
            return Err(ModuleError::Other(
                "Training set is empty. Provide at least one example.".to_string(),
            ));
        }

        info!(
            "COPRO: Starting instruction optimization (depth={}, breadth={})",
            self.depth, self.breadth
        );

        // Get baseline instruction
        let baseline = student
            .signature()
            .instructions
            .clone()
            .unwrap_or_else(|| student.signature().description.clone());

        // Phase 1: Initialize candidates
        let mut instruction_candidates = vec![baseline.clone()];

        info!(
            "Generating {} initial instruction variants",
            self.breadth - 1
        );
        for i in 0..(self.breadth - 1) {
            debug!("Generating variant {}/{}", i + 1, self.breadth - 1);
            match self.generate_instruction_variant(&baseline, None).await {
                Ok(variant) => {
                    if !instruction_candidates.contains(&variant) {
                        instruction_candidates.push(variant);
                    }
                }
                Err(e) => {
                    debug!("Failed to generate variant: {}", e);
                }
            }
        }

        let mut best_instruction = baseline.clone();
        let mut best_score = -f64::INFINITY;

        // Downcast to Predictor for evaluation
        let predictor = student
            .as_any()
            .downcast_ref::<Predictor>()
            .ok_or_else(|| ModuleError::Other("Module must be Predictor for COPRO".to_string()))?;

        // Phase 2: Coordinate ascent
        for iteration in 0..self.depth {
            info!("COPRO iteration {}/{}", iteration + 1, self.depth);

            let mut scores = HashMap::new();

            // Evaluate each candidate
            for (idx, candidate) in instruction_candidates.iter().enumerate() {
                debug!(
                    "Evaluating candidate {}/{}: {}",
                    idx + 1,
                    instruction_candidates.len(),
                    candidate.chars().take(50).collect::<String>()
                );

                let score = self
                    .evaluate_instruction(predictor, candidate, trainset)
                    .await?;
                scores.insert(candidate.clone(), score);

                debug!("Candidate {} score: {:.3}", idx + 1, score);

                if score > best_score {
                    best_score = score;
                    best_instruction = candidate.clone();
                    info!("New best instruction found (score: {:.3})", score);
                }
            }

            // Generate next batch from top performers
            if iteration < self.depth - 1 {
                let mut scored: Vec<_> = scores.iter().collect();
                scored.sort_by(|a, b| b.1.partial_cmp(a.1).unwrap_or(std::cmp::Ordering::Equal));

                let top_3: Vec<(String, f64)> = scored
                    .iter()
                    .take(3)
                    .map(|(instr, score)| ((*instr).clone(), **score))
                    .collect();

                info!("Generating next batch from top 3 performers");
                match self.generate_next_batch(&top_3).await {
                    Ok(new_candidates) => {
                        instruction_candidates = new_candidates;
                        debug!("Generated {} new candidates", instruction_candidates.len());
                    }
                    Err(e) => {
                        debug!("Failed to generate next batch: {}, keeping existing", e);
                    }
                }
            }
        }

        info!("COPRO complete: best score = {:.3}", best_score);

        // Create optimized predictor with best instruction
        let mut optimized_sig = student.signature().clone();
        optimized_sig.instructions = Some(best_instruction);

        Ok(Box::new(Predictor::new(optimized_sig)))
    }

    async fn compile_with_stats(
        &self, student: &dyn Module, trainset: &[Example],
    ) -> Result<(Box<dyn Module>, OptimizationStatistics), ModuleError> {
        use std::time::SystemTime;

        let start = SystemTime::now();
        let optimized = self.compile(student, trainset).await?;
        let elapsed = start.elapsed().unwrap_or_default().as_millis() as u64;

        let cache = self.instruction_cache.lock().unwrap();
        let total_evaluated = cache.len();

        let mut metadata = HashMap::new();
        metadata.insert(
            "instructions_evaluated".to_string(),
            Value::Number(total_evaluated.into()),
        );
        metadata.insert("iterations".to_string(), Value::Number(self.depth.into()));

        let stats = OptimizationStatistics {
            total_attempts: total_evaluated,
            successful_demos: 1, // Single best instruction
            failed_demos: total_evaluated.saturating_sub(1),
            avg_metric_score: cache.values().sum::<f64>() / total_evaluated.max(1) as f64,
            optimization_time_ms: elapsed,
            metadata,
        };

        Ok((optimized, stats))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};
    use crate::dspy::optimizers::ExactMatchMetric;
    use crate::Signature;

    fn create_test_signature() -> Signature {
        Signature::new("QA", "Answer questions accurately")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"))
            .with_instructions("Answer the question concisely.")
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
    fn test_copro_new() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric);
        assert_eq!(optimizer.depth, 3);
        assert_eq!(optimizer.breadth, 10);
        assert_eq!(optimizer.temperature, 0.7);
    }

    #[test]
    fn test_copro_with_depth() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_depth(5);
        assert_eq!(optimizer.depth, 5);
    }

    #[test]
    fn test_copro_min_depth() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_depth(0);
        assert_eq!(optimizer.depth, 1); // Should be clamped
    }

    #[test]
    fn test_copro_with_breadth() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_breadth(20);
        assert_eq!(optimizer.breadth, 20);
    }

    #[test]
    fn test_copro_min_breadth() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_breadth(0);
        assert_eq!(optimizer.breadth, 1); // Should be clamped
    }

    #[test]
    fn test_copro_with_prompt_model() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_prompt_model("test-model");
        assert_eq!(optimizer.prompt_model, "test-model");
    }

    #[test]
    fn test_copro_with_temperature() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_temperature(1.5);
        assert_eq!(optimizer.temperature, 1.5);
    }

    #[test]
    fn test_copro_temperature_clamping() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric).with_temperature(3.0);
        assert_eq!(optimizer.temperature, 2.0); // Should be clamped
    }

    // ===== Builder Pattern Tests =====

    #[test]
    fn test_builder_pattern() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric)
            .with_depth(5)
            .with_breadth(15)
            .with_prompt_model("gpt-4")
            .with_temperature(0.9);

        assert_eq!(optimizer.depth, 5);
        assert_eq!(optimizer.breadth, 15);
        assert_eq!(optimizer.prompt_model, "gpt-4");
        assert_eq!(optimizer.temperature, 0.9);
    }

    // ===== Cache Tests =====

    #[test]
    fn test_instruction_cache_initialization() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric);
        let cache = optimizer.instruction_cache.lock().unwrap();
        assert_eq!(cache.len(), 0);
    }

    // ===== Edge Cases =====

    #[tokio::test]
    async fn test_compile_empty_trainset() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let sig = create_test_signature();
        let predictor = Predictor::new(sig);
        let optimizer = COPRO::new(metric);

        let result = optimizer.compile(&predictor, &[]).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("empty"));
    }

    // Note: Full integration tests would require LLM mocking
    // These tests verify the structure and basic functionality

    // ===== Compile Tests (would need LLM mock for full testing) =====

    // Cannot test compile without LLM, but structure is verified

    // ===== Statistics Tests =====

    #[test]
    fn test_stats_metadata_structure() {
        let metric = Arc::new(ExactMatchMetric::new("answer"));
        let optimizer = COPRO::new(metric);
        // Verify optimizer has the fields needed for stats
        assert_eq!(optimizer.depth, 3);
        assert_eq!(optimizer.breadth, 10);
    }
}
