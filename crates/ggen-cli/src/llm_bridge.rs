//! Groq LLM Bridge - Connects async GenAiClient to sync LlmService trait
//!
//! This module provides the bridge between the async LLM client in ggen-ai
//! and the sync LlmService trait in ggen-core, avoiding cyclic dependencies.
//!
//! Architecture:
//! - ggen-ai: Async GenAiClient (rust-genai based)
//! - ggen-core: Sync LlmService trait (for dependency injection)
//! - ggen-cli: GroqLlmBridge (async→sync bridge using tokio runtime)

use ggen_ai::client::{GenAiClient, LlmClient, LlmConfig};
use ggen_core::codegen::pipeline::LlmService;
use std::error::Error;

/// Bridge implementation that converts async GenAiClient to sync LlmService
///
/// This struct wraps the async GenAiClient and provides a synchronous interface
/// by blocking on a tokio runtime for each LLM call.
#[derive(Debug)]
pub struct GroqLlmBridge {
    /// The async LLM client
    client: GenAiClient,
}

impl GroqLlmBridge {
    /// Create a new Groq LLM bridge with default configuration
    ///
    /// Auto-detects Groq when GROQ_API_KEY is set, otherwise uses defaults.
    pub fn new() -> Result<Self, Box<dyn Error + Send + Sync>> {
        let config = LlmConfig::default();
        Self::with_config(config)
    }

    /// Create a new Groq LLM bridge with custom configuration
    ///
    /// # Arguments
    /// * `config` - LLM configuration (model, temperature, max_tokens, etc.)
    pub fn with_config(config: LlmConfig) -> Result<Self, Box<dyn Error + Send + Sync>> {
        let client = GenAiClient::new(config)?;
        Ok(Self { client })
    }

    /// Build an LLM prompt from skill parameters
    ///
    /// Creates a structured prompt for the LLM to generate skill implementation code.
    fn build_prompt(
        skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> String {
        format!(
            "You are an expert {} developer. Generate a complete, production-ready implementation for the following skill.\n\n\
             **Skill Name**: {}\n\
             **Description**: {}\n\
             **Implementation Hint**: {}\n\n\
             Requirements:\n\
             - Write idiomatic {} code that follows best practices\n\
             - Include proper error handling with Result<T, E>\n\
             - Add inline documentation comments\n\
             - Use appropriate types and data structures\n\
             - Ensure thread-safety if applicable\n\
             - Return ONLY the code (no explanations, no markdown blocks)\n\n\
             Generate the implementation now:",
            language, skill_name, system_prompt, implementation_hint, language
        )
    }

    /// Call async LLM client and block on result
    ///
    /// This is the core async→sync bridge. It creates a tokio runtime
    /// and blocks on the async complete() call.
    fn call_llm_sync(&self, prompt: &str) -> Result<String, Box<dyn Error + Send + Sync>> {
        // Create a new tokio runtime for this call
        // Note: We create a new runtime per call to avoid threading issues
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| Box::new(e) as Box<dyn Error + Send + Sync>)?;

        // Block on async call
        let response: ggen_ai::client::LlmResponse = rt
            .block_on(async { self.client.complete(prompt).await })
            .map_err(|e| Box::new(e) as Box<dyn Error + Send + Sync>)?;

        Ok(response.content)
    }
}

impl Default for GroqLlmBridge {
    fn default() -> Self {
        Self::new().expect("Failed to create GroqLlmBridge with default config")
    }
}

impl Clone for GroqLlmBridge {
    fn clone(&self) -> Self {
        // Note: GenAiClient is Clone, so we can delegate
        Self {
            client: self.client.clone(),
        }
    }
}

impl LlmService for GroqLlmBridge {
    fn generate_skill_impl(
        &self, skill_name: &str, system_prompt: &str, implementation_hint: &str, language: &str,
    ) -> Result<String, Box<dyn Error + Send + Sync>> {
        // Build structured prompt
        let prompt = Self::build_prompt(skill_name, system_prompt, implementation_hint, language);

        // Call LLM via async bridge
        let generated = self.call_llm_sync(&prompt)?;

        // Clean up response (remove markdown code blocks if present)
        let cleaned = if generated.contains("```") {
            // Extract code from markdown blocks
            let lines: Vec<&str> = generated.lines().collect();
            let mut in_code_block = false;
            let mut code_lines = Vec::new();

            for line in lines {
                if line.trim().starts_with("```") {
                    in_code_block = !in_code_block;
                    continue;
                }
                if in_code_block {
                    code_lines.push(line);
                }
            }

            if code_lines.is_empty() {
                // Fallback: return original if no code found
                generated
            } else {
                code_lines.join("\n")
            }
        } else {
            generated
        };

        Ok(cleaned)
    }

    fn clone_box(&self) -> Box<dyn LlmService> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_prompt() {
        let prompt = GroqLlmBridge::build_prompt(
            "calculate_fibonacci",
            "Calculate Fibonacci numbers",
            "Use iterative approach for efficiency",
            "rust",
        );

        assert!(prompt.contains("calculate_fibonacci"));
        assert!(prompt.contains("Calculate Fibonacci numbers"));
        assert!(prompt.contains("Use iterative approach"));
        assert!(prompt.contains("rust"));
        assert!(prompt.contains("Result<T, E>"));
    }

    #[test]
    fn test_build_prompt_typescript() {
        let prompt = GroqLlmBridge::build_prompt(
            "fetch_user_data",
            "Fetch user from API",
            "Use async/await",
            "typescript",
        );

        assert!(prompt.contains("typescript"));
        assert!(prompt.contains("fetch_user_data"));
    }

    #[test]
    fn test_bridge_creation() {
        let bridge = GroqLlmBridge::new();
        assert!(bridge.is_ok());
    }
}
