//! DummyLM - Test-focused Language Model with Multiple Response Modes
//!
//! Provides predictable LM behavior for testing without real API calls.
//! Based on Python DSPy's DummyLM with 3 operating modes.

use crate::dspy::{Module, ModuleError, Signature};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Call history entry for tracking LM interactions
#[derive(Debug, Clone)]
pub struct CallHistory {
    pub prompt: String,
    pub response: String,
    pub timestamp: std::time::SystemTime,
}

impl CallHistory {
    fn new(prompt: String, response: String) -> Self {
        Self {
            prompt,
            response,
            timestamp: std::time::SystemTime::now(),
        }
    }
}

/// Operating modes for DummyLM
#[derive(Debug, Clone)]
pub enum DummyLMMode {
    /// Sequential: Return responses in order (cycles when exhausted)
    Sequential(Vec<HashMap<String, Value>>),

    /// QueryBased: Match prompt content to response
    QueryBased(HashMap<String, HashMap<String, Value>>),

    /// ExampleFollowing: Extract from demonstration history
    ExampleFollowing {
        demonstrations: Vec<(HashMap<String, Value>, HashMap<String, Value>)>,
    },
}

/// DummyLM - Test-focused language model
///
/// Provides deterministic responses for testing DSPy modules without
/// making actual LLM API calls. Supports three modes:
///
/// 1. **Sequential**: Returns responses in sequence
/// 2. **QueryBased**: Matches prompts to responses
/// 3. **ExampleFollowing**: Extracts from demonstrations
///
/// # Example
///
/// ```
/// use ggen_ai::dspy::testing::DummyLM;
/// use serde_json::json;
/// use std::collections::HashMap;
///
/// // Sequential mode
/// let responses = vec![
///     HashMap::from([("answer".to_string(), json!("Paris"))]),
///     HashMap::from([("answer".to_string(), json!("London"))]),
/// ];
/// let dummy = DummyLM::sequential(responses);
///
/// // Query-based mode
/// let mut query_map = HashMap::new();
/// query_map.insert(
///     "France".to_string(),
///     HashMap::from([("answer".to_string(), json!("Paris"))]),
/// );
/// let dummy = DummyLM::query_based(query_map);
/// ```
pub struct DummyLM {
    mode: DummyLMMode,
    call_count: Arc<Mutex<usize>>,
    history: Arc<Mutex<Vec<CallHistory>>>,
    signature: Signature,
}

impl DummyLM {
    /// Create sequential mode DummyLM
    ///
    /// Returns responses in sequence, cycling when exhausted.
    pub fn sequential(responses: Vec<HashMap<String, Value>>) -> Self {
        Self {
            mode: DummyLMMode::Sequential(responses),
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
            signature: Self::default_signature(),
        }
    }

    /// Create query-based mode DummyLM
    ///
    /// Matches prompt content to responses. If prompt contains key,
    /// returns associated response.
    pub fn query_based(responses: HashMap<String, HashMap<String, Value>>) -> Self {
        Self {
            mode: DummyLMMode::QueryBased(responses),
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
            signature: Self::default_signature(),
        }
    }

    /// Create example-following mode DummyLM
    ///
    /// Matches prompt inputs to demonstration inputs and returns
    /// corresponding outputs.
    pub fn example_following(
        demonstrations: Vec<(HashMap<String, Value>, HashMap<String, Value>)>,
    ) -> Self {
        Self {
            mode: DummyLMMode::ExampleFollowing { demonstrations },
            call_count: Arc::new(Mutex::new(0)),
            history: Arc::new(Mutex::new(Vec::new())),
            signature: Self::default_signature(),
        }
    }

    /// Create DummyLM with custom signature
    pub fn with_signature(mut self, signature: Signature) -> Self {
        self.signature = signature;
        self
    }

    /// Get call count for test assertions
    pub fn call_count(&self) -> usize {
        *self.call_count.lock().unwrap()
    }

    /// Get interaction history for verification
    pub fn history(&self) -> Vec<CallHistory> {
        self.history.lock().unwrap().clone()
    }

    /// Reset call count and history
    pub fn reset(&self) {
        *self.call_count.lock().unwrap() = 0;
        self.history.lock().unwrap().clear();
    }

    /// Get the last N calls from history
    pub fn last_n_calls(&self, n: usize) -> Vec<CallHistory> {
        let history = self.history.lock().unwrap();
        history.iter().rev().take(n).cloned().collect()
    }

    /// Check if prompt contains any of the given keywords
    #[allow(dead_code)]
    fn prompt_contains_any(&self, prompt: &str, keywords: &[&str]) -> bool {
        let prompt_lower = prompt.to_lowercase();
        keywords.iter().any(|kw| prompt_lower.contains(kw))
    }

    /// Extract inputs from prompt text
    fn extract_inputs_from_prompt(&self, prompt: &str) -> HashMap<String, Value> {
        let mut inputs = HashMap::new();

        // Simple extraction: look for "field: value" patterns
        for line in prompt.lines() {
            if let Some((key, value)) = line.split_once(':') {
                let key = key.trim().to_string();
                let value = value.trim().to_string();
                inputs.insert(key, json!(value));
            }
        }

        inputs
    }

    /// Find matching demonstration for given inputs
    fn find_matching_demo(
        &self,
        inputs: &HashMap<String, Value>,
        demonstrations: &[(HashMap<String, Value>, HashMap<String, Value>)],
    ) -> Option<HashMap<String, Value>> {
        for (demo_inputs, demo_outputs) in demonstrations {
            // Check if any input value matches
            let has_match = inputs.iter().any(|(k, v)| {
                demo_inputs.get(k).map_or(false, |dv| dv == v)
            });

            if has_match {
                return Some(demo_outputs.clone());
            }
        }
        None
    }

    /// Default signature for DummyLM
    fn default_signature() -> Signature {
        use crate::dspy::field::{InputField, OutputField};

        Signature::new("DummyLM", "Test language model")
            .with_input(InputField::new("prompt", "Input prompt", "String"))
            .with_output(OutputField::new("response", "Generated response", "String"))
    }

    /// Get response based on mode
    fn get_response(&self, prompt: &str) -> Result<HashMap<String, Value>, ModuleError> {
        let mut count = self.call_count.lock().unwrap();
        *count += 1;
        let call_index = *count - 1;
        drop(count);

        let response = match &self.mode {
            DummyLMMode::Sequential(responses) => {
                if responses.is_empty() {
                    return Err(ModuleError::Other(
                        "DummyLM has no responses configured".to_string(),
                    ));
                }
                let idx = call_index % responses.len();
                responses[idx].clone()
            }

            DummyLMMode::QueryBased(map) => {
                // Find best match in map
                map.iter()
                    .find(|(key, _)| prompt.contains(key.as_str()))
                    .map(|(_, val)| val.clone())
                    .unwrap_or_else(|| {
                        HashMap::from([(
                            "response".to_string(),
                            json!("default response"),
                        )])
                    })
            }

            DummyLMMode::ExampleFollowing { demonstrations } => {
                // Extract inputs from prompt
                let inputs = self.extract_inputs_from_prompt(prompt);

                // Find matching demonstration
                self.find_matching_demo(&inputs, demonstrations)
                    .unwrap_or_else(|| {
                        HashMap::from([(
                            "response".to_string(),
                            json!("no matching demonstration"),
                        )])
                    })
            }
        };

        // Record in history
        let response_str = serde_json::to_string(&response).unwrap_or_default();
        self.history
            .lock()
            .unwrap()
            .push(CallHistory::new(prompt.to_string(), response_str));

        Ok(response)
    }
}

#[async_trait::async_trait]
impl Module for DummyLM {
    fn signature(&self) -> &Signature {
        &self.signature
    }

    async fn forward(
        &self,
        inputs: HashMap<String, Value>,
    ) -> Result<HashMap<String, Value>, ModuleError> {
        // Extract prompt from inputs
        let prompt = inputs
            .get("prompt")
            .and_then(|v| v.as_str())
            .unwrap_or("");

        // If no prompt, create one from all inputs
        let prompt = if prompt.is_empty() {
            inputs
                .iter()
                .map(|(k, v)| format!("{}: {}", k, v))
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            prompt.to_string()
        };

        self.get_response(&prompt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sequential_mode_basic() {
        let responses = vec![
            HashMap::from([("answer".to_string(), json!("Paris"))]),
            HashMap::from([("answer".to_string(), json!("London"))]),
        ];

        let dummy = DummyLM::sequential(responses);

        let inputs = HashMap::from([("prompt".to_string(), json!("Question 1"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("Paris"));

        let inputs = HashMap::from([("prompt".to_string(), json!("Question 2"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("London"));

        assert_eq!(dummy.call_count(), 2);
    }

    #[tokio::test]
    async fn test_sequential_mode_cycles() {
        let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];

        let dummy = DummyLM::sequential(responses);

        // First call
        let inputs = HashMap::from([("prompt".to_string(), json!("Q1"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("A"));

        // Second call - should cycle back to first response
        let inputs = HashMap::from([("prompt".to_string(), json!("Q2"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("A"));

        assert_eq!(dummy.call_count(), 2);
    }

    #[tokio::test]
    async fn test_query_based_mode() {
        let mut query_map = HashMap::new();
        query_map.insert(
            "France".to_string(),
            HashMap::from([("answer".to_string(), json!("Paris"))]),
        );
        query_map.insert(
            "UK".to_string(),
            HashMap::from([("answer".to_string(), json!("London"))]),
        );

        let dummy = DummyLM::query_based(query_map);

        // Match France
        let inputs = HashMap::from([("prompt".to_string(), json!("Capital of France?"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("Paris"));

        // Match UK
        let inputs = HashMap::from([("prompt".to_string(), json!("Capital of UK?"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("London"));

        assert_eq!(dummy.call_count(), 2);
    }

    #[tokio::test]
    async fn test_query_based_default_response() {
        let query_map = HashMap::new();
        let dummy = DummyLM::query_based(query_map);

        let inputs = HashMap::from([("prompt".to_string(), json!("Unknown query"))]);
        let result = dummy.forward(inputs).await.unwrap();

        // Should return default response
        assert_eq!(result.get("response").unwrap(), &json!("default response"));
    }

    #[tokio::test]
    async fn test_example_following_mode() {
        let demonstrations = vec![
            (
                HashMap::from([("question".to_string(), json!("What is 2+2?"))]),
                HashMap::from([("answer".to_string(), json!("4"))]),
            ),
            (
                HashMap::from([("question".to_string(), json!("What is 3+3?"))]),
                HashMap::from([("answer".to_string(), json!("6"))]),
            ),
        ];

        let dummy = DummyLM::example_following(demonstrations);

        // Match first example
        let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 2+2?"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("4"));

        // Match second example
        let inputs = HashMap::from([("prompt".to_string(), json!("question: What is 3+3?"))]);
        let result = dummy.forward(inputs).await.unwrap();
        assert_eq!(result.get("answer").unwrap(), &json!("6"));
    }

    #[tokio::test]
    async fn test_call_history_tracking() {
        let responses = vec![HashMap::from([("answer".to_string(), json!("Response"))])];
        let dummy = DummyLM::sequential(responses);

        let inputs = HashMap::from([("prompt".to_string(), json!("Test prompt"))]);
        dummy.forward(inputs).await.unwrap();

        let history = dummy.history();
        assert_eq!(history.len(), 1);
        assert!(history[0].prompt.contains("Test prompt"));
    }

    #[tokio::test]
    async fn test_reset_functionality() {
        let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];
        let dummy = DummyLM::sequential(responses);

        // Make some calls
        let inputs = HashMap::from([("prompt".to_string(), json!("Q1"))]);
        dummy.forward(inputs).await.unwrap();
        assert_eq!(dummy.call_count(), 1);

        // Reset
        dummy.reset();
        assert_eq!(dummy.call_count(), 0);
        assert_eq!(dummy.history().len(), 0);
    }

    #[tokio::test]
    async fn test_last_n_calls() {
        let responses = vec![HashMap::from([("answer".to_string(), json!("A"))])];
        let dummy = DummyLM::sequential(responses);

        // Make 3 calls
        for i in 1..=3 {
            let inputs = HashMap::from([("prompt".to_string(), json!(format!("Q{}", i)))]);
            dummy.forward(inputs).await.unwrap();
        }

        let last_2 = dummy.last_n_calls(2);
        assert_eq!(last_2.len(), 2);
        assert!(last_2[0].prompt.contains("Q3"));
        assert!(last_2[1].prompt.contains("Q2"));
    }

    #[tokio::test]
    async fn test_empty_responses_error() {
        let dummy = DummyLM::sequential(vec![]);

        let inputs = HashMap::from([("prompt".to_string(), json!("Test"))]);
        let result = dummy.forward(inputs).await;

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("no responses"));
    }

    #[test]
    fn test_prompt_contains_any() {
        let dummy = DummyLM::sequential(vec![]);

        assert!(dummy.prompt_contains_any("Capital of France", &["france", "paris"]));
        assert!(!dummy.prompt_contains_any("Capital of UK", &["france", "paris"]));
        assert!(dummy.prompt_contains_any("CAPITAL OF FRANCE", &["france"])); // Case insensitive
    }

    #[test]
    fn test_extract_inputs_from_prompt() {
        let dummy = DummyLM::sequential(vec![]);

        let prompt = "question: What is Rust?\nanswer: A programming language";
        let inputs = dummy.extract_inputs_from_prompt(prompt);

        assert_eq!(inputs.len(), 2);
        assert_eq!(inputs.get("question").unwrap(), &json!("What is Rust?"));
        assert_eq!(
            inputs.get("answer").unwrap(),
            &json!("A programming language")
        );
    }
}
