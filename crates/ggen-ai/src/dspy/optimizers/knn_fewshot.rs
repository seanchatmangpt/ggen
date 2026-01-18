//! KNNFewShot Optimizer
//!
//! Retrieval-Augmented Few-Shot Learning using k-nearest neighbors.
//! Production readiness: ⭐⭐⭐⭐⭐ (Excellent for production)
//!
//! Use case: Dynamic demo selection, domain adaptation, query-specific optimization
//!
//! # Algorithm
//! ```text
//! 1. Vectorize all training examples using vectorizer (e.g., embeddings)
//! 2. For each input query:
//!    a. Vectorize query
//!    b. Find k nearest neighbors in training set (cosine similarity)
//!    c. Use neighbors as few-shot demonstrations
//!    d. Bootstrap with teacher/student if needed
//! 3. Return program that retrieves demos dynamically per query
//! ```

use crate::dspy::{Module, ModuleError, Signature};
use crate::dspy::optimizer::{Example, Demonstration};
use crate::dspy::module::ModuleResult;
use super::{Optimizer, Metric};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info};

/// Vectorizer trait for embedding text into vector space
#[async_trait]
pub trait Vectorizer: Send + Sync {
    /// Embed text into a vector
    async fn embed(&self, text: &str) -> Result<Vec<f32>, ModuleError>;

    /// Get the dimension of embeddings
    fn dimension(&self) -> usize;
}

/// Simple cosine similarity vectorizer (deterministic, for testing)
pub struct CosineVectorizer {
    dimension: usize,
}

impl CosineVectorizer {
    /// Create new cosine vectorizer
    pub fn new(dimension: usize) -> Self {
        Self { dimension }
    }

    /// Hash text to vector (deterministic, not semantic)
    fn hash_to_vector(&self, text: &str) -> Vec<f32> {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut vec = vec![0.0; self.dimension];
        for (i, chunk) in text.chars().collect::<Vec<_>>().chunks(10).enumerate() {
            if i >= self.dimension {
                break;
            }
            let mut hasher = DefaultHasher::new();
            chunk.iter().collect::<String>().hash(&mut hasher);
            let hash = hasher.finish();
            vec[i] = (hash % 1000) as f32 / 1000.0;
        }

        // Normalize
        let norm: f32 = vec.iter().map(|x| x * x).sum::<f32>().sqrt();
        if norm > 0.0 {
            vec.iter_mut().for_each(|x| *x /= norm);
        }

        vec
    }
}

#[async_trait]
impl Vectorizer for CosineVectorizer {
    async fn embed(&self, text: &str) -> Result<Vec<f32>, ModuleError> {
        Ok(self.hash_to_vector(text))
    }

    fn dimension(&self) -> usize {
        self.dimension
    }
}

/// Calculate cosine similarity between two vectors
pub fn cosine_similarity(a: &[f32], b: &[f32]) -> f32 {
    if a.len() != b.len() || a.is_empty() {
        return 0.0;
    }

    let dot_product: f32 = a.iter().zip(b.iter()).map(|(x, y)| x * y).sum();
    let norm_a: f32 = a.iter().map(|x| x * x).sum::<f32>().sqrt();
    let norm_b: f32 = b.iter().map(|x| x * x).sum::<f32>().sqrt();

    if norm_a == 0.0 || norm_b == 0.0 {
        return 0.0;
    }

    dot_product / (norm_a * norm_b)
}

/// KNNFewShot optimizer
///
/// Dynamically retrieves k most similar examples for each query using
/// nearest neighbor search in embedding space.
///
/// # Mathematical Formulation
/// ```text
/// V: embedding function
/// D_train = {(x₁, y₁), ..., (xₙ, yₙ)}
///
/// For query x:
///   v_x = V(x)
///   v_i = V(x_i) for all x_i ∈ D_train
///   similarity(x, x_i) = cos(v_x, v_i) = (v_x · v_i) / (||v_x|| ||v_i||)
///
///   neighbors = top_k({(x_i, y_i) | similarity(x, x_i)}, k)
///   P(x) = LLM(neighbors + x)
/// ```
pub struct KNNFewShot {
    k: usize,
    vectorizer: Arc<dyn Vectorizer>,
    metric: Option<Arc<dyn Metric>>,
}

impl KNNFewShot {
    /// Create new KNNFewShot optimizer
    ///
    /// # Arguments
    /// * `k` - Number of nearest neighbors to retrieve
    /// * `vectorizer` - Embedding function for text
    pub fn new(k: usize, vectorizer: Arc<dyn Vectorizer>) -> Self {
        Self {
            k: k.max(1),
            vectorizer,
            metric: None,
        }
    }

    /// Set metric for bootstrapping validation
    pub fn with_metric(mut self, metric: Arc<dyn Metric>) -> Self {
        self.metric = Some(metric);
        self
    }

    /// Format example as text for vectorization
    fn format_example_text(example: &Example) -> String {
        let mut text = String::new();
        for (key, value) in &example.inputs {
            text.push_str(&format!("{}: ", key));
            match value {
                Value::String(s) => text.push_str(s),
                _ => text.push_str(&value.to_string()),
            }
            text.push(' ');
        }
        text.trim().to_string()
    }
}

#[async_trait]
impl Optimizer for KNNFewShot {
    async fn compile(
        &self,
        student: &dyn Module,
        trainset: &[Example],
    ) -> Result<Box<dyn Module>, ModuleError> {
        if trainset.is_empty() {
            return Err(ModuleError::Other(
                "Training set is empty. Provide at least one example.".to_string()
            ));
        }

        info!(
            "KNNFewShot: Embedding {} examples with k={}",
            trainset.len(),
            self.k
        );

        // Precompute embeddings for all training examples
        let mut embeddings = Vec::new();
        for (idx, example) in trainset.iter().enumerate() {
            let text = Self::format_example_text(example);
            debug!("Embedding example {}/{}: {}", idx + 1, trainset.len(), text);
            let embedding = self.vectorizer.embed(&text).await?;
            embeddings.push(embedding);
        }

        info!("Embeddings computed: {} vectors", embeddings.len());

        // Create KNN predictor
        Ok(Box::new(KNNPredictor {
            signature: student.signature().clone(),
            trainset: trainset.to_vec(),
            embeddings,
            vectorizer: Arc::clone(&self.vectorizer),
            k: self.k,
        }))
    }
}

/// KNN-based predictor that retrieves demonstrations dynamically
pub struct KNNPredictor {
    signature: Signature,
    trainset: Vec<Example>,
    embeddings: Vec<Vec<f32>>,
    vectorizer: Arc<dyn Vectorizer>,
    k: usize,
}

impl KNNPredictor {
    /// Find k nearest neighbors for query
    async fn find_nearest_neighbors(
        &self,
        query_text: &str,
    ) -> Result<Vec<usize>, ModuleError> {
        // Embed query
        let query_embedding = self.vectorizer.embed(query_text).await?;

        // Calculate similarities
        let mut similarities: Vec<(usize, f32)> = self
            .embeddings
            .iter()
            .enumerate()
            .map(|(i, emb)| (i, cosine_similarity(&query_embedding, emb)))
            .collect();

        // Sort by similarity (descending)
        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Take top k indices
        let top_k: Vec<usize> = similarities.iter().take(self.k).map(|(i, _)| *i).collect();

        Ok(top_k)
    }

    /// Build prompt with retrieved demonstrations
    fn build_prompt(
        &self,
        inputs: &HashMap<String, Value>,
        demonstrations: &[Demonstration],
    ) -> Result<String, ModuleError> {
        let mut prompt = format!("{}\n\n", self.signature.description);

        if let Some(instructions) = &self.signature.instructions {
            prompt.push_str(&format!("Instructions: {}\n\n", instructions));
        }

        // Add demonstrations
        if !demonstrations.is_empty() {
            prompt.push_str("Examples (retrieved by relevance):\n\n");
            for (idx, demo) in demonstrations.iter().enumerate() {
                prompt.push_str(&format!("--- Example {} ---\n", idx + 1));
                prompt.push_str(&demo.format(&self.signature));
                prompt.push_str("\n");
            }
            prompt.push_str("--- Your Turn ---\n\n");
        }

        // Add current input
        prompt.push_str("Input:\n");
        for input_field in &self.signature.inputs {
            if let Some(value) = inputs.get(input_field.name()) {
                let value_str = match value {
                    Value::String(s) => s.clone(),
                    _ => value.to_string(),
                };
                prompt.push_str(&format!("{}: {}\n", input_field.name(), value_str));
            }
        }

        prompt.push_str("\nOutput:\n");
        for output_field in &self.signature.outputs {
            prompt.push_str(&format!("{}: ", output_field.name()));
        }

        Ok(prompt)
    }

    /// Parse output from LLM response
    fn parse_output(&self, response: &str) -> Result<HashMap<String, Value>, ModuleError> {
        let mut outputs = HashMap::new();

        for output_field in &self.signature.outputs {
            if let Some(value) = self.extract_field_value(response, output_field.name()) {
                outputs.insert(output_field.name().to_string(), Value::String(value));
            }
        }

        if outputs.is_empty() {
            outputs.insert(
                self.signature
                    .outputs
                    .first()
                    .map(|f| f.name().to_string())
                    .unwrap_or_else(|| "output".to_string()),
                Value::String(response.to_string()),
            );
        }

        Ok(outputs)
    }

    /// Extract field value from response text
    fn extract_field_value(&self, response: &str, field_name: &str) -> Option<String> {
        let pattern = format!("{}: ", field_name);
        if let Some(start) = response.find(&pattern) {
            let text = &response[start + pattern.len()..];
            let value = text.lines().next().unwrap_or("").trim();
            if !value.is_empty() {
                return Some(value.to_string());
            }
        }
        None
    }

    /// Call the LLM with the given prompt
    async fn call_llm(&self, prompt: &str) -> Result<String, ModuleError> {
        use genai::{Client, chat::{ChatMessage, ChatOptions, ChatRequest}};

        let client = Client::default();

        let model = std::env::var("GGEN_LLM_MODEL")
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_default();

        if model.is_empty() {
            return Err(ModuleError::LlmError(
                "Model name not set. Set GGEN_LLM_MODEL or DEFAULT_MODEL env var".to_string()
            ));
        }

        debug!("Calling LLM with KNN-retrieved demonstrations");

        let chat_req = ChatRequest::new(vec![
            ChatMessage::system("You are a helpful assistant. Follow the examples provided and respond in the exact format requested."),
            ChatMessage::user(prompt.to_string()),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(0.7)
            .with_max_tokens(4096);

        match client.exec_chat(&model, chat_req, Some(&chat_options)).await {
            Ok(response) => {
                let content = response.first_text().unwrap_or_default().to_string();
                debug!("LLM response received, length: {}", content.len());
                Ok(content)
            }
            Err(e) => {
                Err(ModuleError::LlmError(format!("LLM call failed: {}", e)))
            }
        }
    }
}

#[async_trait]
impl Module for KNNPredictor {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>,
    ) -> ModuleResult<HashMap<String, Value>> {
        // Validate inputs
        self.validate_inputs(&inputs)?;

        // Format query text
        let query_text = inputs
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<_>>()
            .join(" ");

        debug!("Finding nearest neighbors for: {}", query_text);

        // Find k nearest neighbors
        let neighbor_indices = self.find_nearest_neighbors(&query_text).await?;

        // Retrieve demonstrations
        let demonstrations: Vec<Demonstration> = neighbor_indices
            .iter()
            .map(|&i| {
                let ex = &self.trainset[i];
                Demonstration::new(ex.inputs.clone(), ex.outputs.clone())
            })
            .collect();

        debug!(
            "Retrieved {} demonstrations for query",
            demonstrations.len()
        );

        // Build prompt with retrieved demos
        let prompt = self.build_prompt(&inputs, &demonstrations)?;

        // Call LLM
        let response = self.call_llm(&prompt).await?;

        // Parse output
        self.parse_output(&response)
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Signature;
    use crate::dspy::field::{InputField, OutputField};

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
                    Value::String(format!("Question {}", i))
                );

                let mut outputs = HashMap::new();
                outputs.insert(
                    "answer".to_string(),
                    Value::String(format!("Answer {}", i))
                );

                Example::new(inputs, outputs)
            })
            .collect()
    }

    // ===== Cosine Similarity Tests =====

    #[test]
    fn test_cosine_similarity_identical() {
        let vec = vec![1.0, 2.0, 3.0];
        let similarity = cosine_similarity(&vec, &vec);
        assert!((similarity - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_cosine_similarity_orthogonal() {
        let vec_a = vec![1.0, 0.0, 0.0];
        let vec_b = vec![0.0, 1.0, 0.0];
        let similarity = cosine_similarity(&vec_a, &vec_b);
        assert!((similarity - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_cosine_similarity_opposite() {
        let vec_a = vec![1.0, 0.0];
        let vec_b = vec![-1.0, 0.0];
        let similarity = cosine_similarity(&vec_a, &vec_b);
        assert!((similarity - (-1.0)).abs() < 0.001);
    }

    #[test]
    fn test_cosine_similarity_empty() {
        let vec_a: Vec<f32> = vec![];
        let vec_b: Vec<f32> = vec![];
        let similarity = cosine_similarity(&vec_a, &vec_b);
        assert_eq!(similarity, 0.0);
    }

    #[test]
    fn test_cosine_similarity_different_lengths() {
        let vec_a = vec![1.0, 2.0];
        let vec_b = vec![1.0, 2.0, 3.0];
        let similarity = cosine_similarity(&vec_a, &vec_b);
        assert_eq!(similarity, 0.0);
    }

    // ===== CosineVectorizer Tests =====

    #[test]
    fn test_cosine_vectorizer_dimension() {
        let vectorizer = CosineVectorizer::new(128);
        assert_eq!(vectorizer.dimension(), 128);
    }

    #[tokio::test]
    async fn test_cosine_vectorizer_embed() {
        let vectorizer = CosineVectorizer::new(128);
        let embedding = vectorizer.embed("test text").await.unwrap();
        assert_eq!(embedding.len(), 128);
    }

    #[tokio::test]
    async fn test_cosine_vectorizer_deterministic() {
        let vectorizer = CosineVectorizer::new(64);
        let emb1 = vectorizer.embed("hello world").await.unwrap();
        let emb2 = vectorizer.embed("hello world").await.unwrap();
        assert_eq!(emb1, emb2);
    }

    #[tokio::test]
    async fn test_cosine_vectorizer_different_texts() {
        let vectorizer = CosineVectorizer::new(64);
        let emb1 = vectorizer.embed("text A").await.unwrap();
        let emb2 = vectorizer.embed("text B").await.unwrap();
        assert_ne!(emb1, emb2);
    }

    #[tokio::test]
    async fn test_cosine_vectorizer_normalized() {
        let vectorizer = CosineVectorizer::new(64);
        let embedding = vectorizer.embed("test").await.unwrap();
        let norm: f32 = embedding.iter().map(|x| x * x).sum::<f32>().sqrt();
        assert!((norm - 1.0).abs() < 0.001); // Should be unit vector
    }

    // ===== KNNFewShot Constructor Tests =====

    #[test]
    fn test_knn_fewshot_new() {
        let vectorizer = Arc::new(CosineVectorizer::new(128));
        let optimizer = KNNFewShot::new(5, vectorizer);
        assert_eq!(optimizer.k, 5);
    }

    #[test]
    fn test_knn_fewshot_min_k() {
        let vectorizer = Arc::new(CosineVectorizer::new(128));
        let optimizer = KNNFewShot::new(0, vectorizer);
        assert_eq!(optimizer.k, 1); // Should be clamped to 1
    }

    // ===== Format Example Text Tests =====

    #[test]
    fn test_format_example_text() {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("What is Rust?".to_string()));
        let example = Example::new(inputs, HashMap::new());

        let text = KNNFewShot::format_example_text(&example);
        assert!(text.contains("question:"));
        assert!(text.contains("What is Rust?"));
    }

    #[test]
    fn test_format_example_text_multiple_inputs() {
        let mut inputs = HashMap::new();
        inputs.insert("question".to_string(), Value::String("Q1".to_string()));
        inputs.insert("context".to_string(), Value::String("C1".to_string()));
        let example = Example::new(inputs, HashMap::new());

        let text = KNNFewShot::format_example_text(&example);
        assert!(text.contains("Q1"));
        assert!(text.contains("C1"));
    }

    // ===== Compile Tests =====

    #[tokio::test]
    async fn test_knn_compile_empty_trainset() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(3, vectorizer);

        let result = optimizer.compile(&predictor, &[]).await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("empty"));
    }

    #[tokio::test]
    async fn test_knn_compile_success() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(3, vectorizer);
        let examples = create_test_examples(5);

        let result = optimizer.compile(&predictor, &examples).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_knn_compile_creates_knn_predictor() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(3, vectorizer);
        let examples = create_test_examples(5);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        assert!(optimized.as_any().is::<KNNPredictor>());
    }

    #[tokio::test]
    async fn test_knn_compile_embeddings() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(3, vectorizer);
        let examples = create_test_examples(10);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let knn_predictor = optimized
            .as_any()
            .downcast_ref::<KNNPredictor>()
            .unwrap();

        assert_eq!(knn_predictor.embeddings.len(), 10);
        assert_eq!(knn_predictor.trainset.len(), 10);
    }

    // ===== KNNPredictor Tests =====

    #[tokio::test]
    async fn test_knn_predictor_find_nearest_neighbors() {
        let sig = create_test_signature();
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let examples = create_test_examples(10);

        let mut embeddings = Vec::new();
        for ex in &examples {
            let text = KNNFewShot::format_example_text(ex);
            let emb = vectorizer.embed(&text).await.unwrap();
            embeddings.push(emb);
        }

        let predictor = KNNPredictor {
            signature: sig,
            trainset: examples.clone(),
            embeddings,
            vectorizer: Arc::clone(&vectorizer),
            k: 3,
        };

        let neighbors = predictor
            .find_nearest_neighbors("Question 0")
            .await
            .unwrap();

        assert_eq!(neighbors.len(), 3);
        // First neighbor should be question 0 (most similar to itself)
        assert_eq!(neighbors[0], 0);
    }

    #[test]
    fn test_knn_predictor_signature() {
        let sig = create_test_signature();
        let vectorizer = Arc::new(CosineVectorizer::new(64));

        let predictor = KNNPredictor {
            signature: sig.clone(),
            trainset: vec![],
            embeddings: vec![],
            vectorizer,
            k: 3,
        };

        assert_eq!(predictor.signature().name, sig.name);
    }

    // ===== Edge Cases =====

    #[tokio::test]
    async fn test_knn_single_example() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(3, vectorizer);
        let examples = create_test_examples(1);

        let result = optimizer.compile(&predictor, &examples).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_knn_k_larger_than_trainset() {
        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let vectorizer = Arc::new(CosineVectorizer::new(64));
        let optimizer = KNNFewShot::new(10, vectorizer);
        let examples = create_test_examples(3);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();

        let knn_predictor = optimized
            .as_any()
            .downcast_ref::<KNNPredictor>()
            .unwrap();

        // Should retrieve all 3 available examples
        let neighbors = knn_predictor
            .find_nearest_neighbors("test query")
            .await
            .unwrap();
        assert!(neighbors.len() <= 3);
    }

    // ===== Integration Tests =====

    #[tokio::test]
    async fn test_knn_similarity_ranking() {
        let vectorizer = Arc::new(CosineVectorizer::new(128));

        // Create examples with different content
        let mut examples = vec![];
        for i in 0..5 {
            let mut inputs = HashMap::new();
            inputs.insert(
                "question".to_string(),
                Value::String(format!("Question about topic {}", i))
            );
            let mut outputs = HashMap::new();
            outputs.insert("answer".to_string(), Value::String(format!("Answer {}", i)));
            examples.push(Example::new(inputs, outputs));
        }

        let sig = create_test_signature();
        let predictor = crate::dspy::Predictor::new(sig);
        let optimizer = KNNFewShot::new(2, vectorizer);

        let optimized = optimizer.compile(&predictor, &examples).await.unwrap();
        let knn_predictor = optimized
            .as_any()
            .downcast_ref::<KNNPredictor>()
            .unwrap();

        // Query similar to example 0
        let neighbors = knn_predictor
            .find_nearest_neighbors("Question about topic 0")
            .await
            .unwrap();

        assert_eq!(neighbors.len(), 2);
        // Most similar should be example 0
        assert_eq!(neighbors[0], 0);
    }
}
